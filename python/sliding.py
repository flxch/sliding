"""
Sliding window aggregation algorithm.

Python design note:
The Go implementation uses channels for the input stream.  In Python the
natural equivalent is an *iterator* (or generator): it is lazy, works for
infinite sequences, and requires no threads or queues.  The internal stream
helper below wraps any iterator and adds the same two primitives that the Go
`input` struct exposes:
  - read()  – advance and return the next element
  - skip(n) – discard elements until the absolute position reaches n
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Callable, Generic, Iterator, Optional, Tuple, TypeVar

A = TypeVar("A")
Op = Callable[[A, A], A]
Window = Tuple[int, int]  # (left_index, right_index), both inclusive


# Internal stream wrapper

class _Stream(Generic[A]):
    """Wraps an iterator and tracks the absolute read position."""

    def __init__(self, it: Iterator[A]) -> None:
        self._it = it
        self.count: int = 0  # number of elements already consumed

    def read(self) -> Tuple[Optional[A], bool]:
        """Return (value, True) or (None, False) if the iterator is exhausted."""
        try:
            v = next(self._it)
            self.count += 1
            return v, True
        except StopIteration:
            return None, False

    def skip(self, n: int) -> bool:
        """Discard elements until self.count == n.  Return False if exhausted."""
        while self.count < n:
            try:
                next(self._it)
                self.count += 1
            except StopIteration:
                return False
        return True


# Option type

@dataclass
class _Option(Generic[A]):
    """A minimal option/maybe type to mirror the Go implementation."""
    _value: Optional[A] = field(default=None, repr=False)
    _ok: bool = field(default=False, repr=False)

    def is_some(self) -> bool:
        return self._ok

    def is_none(self) -> bool:
        return not self._ok

    @property
    def value(self) -> A:
        if not self._ok:
            raise ValueError("Option is None")
        return self._value  # type: ignore[return-value]


def _some(v: A) -> _Option[A]:
    o: _Option[A] = _Option()
    o._value = v
    o._ok = True
    return o


def _none() -> _Option:
    return _Option()


def _lift(op: Op[A]) -> Op[_Option[A]]:
    """Lift an operator to work on _Option values."""
    def lifted(x: _Option[A], y: _Option[A]) -> _Option[A]:
        if x.is_none() or y.is_none():
            return _none()
        return _some(op(x.value, y.value))
    return lifted


# Tree

@dataclass
class _Label(Generic[A]):
    from_idx: int
    to_idx:   int
    agg:      _Option[A]   # some(v) for live nodes, none for discharged


class _Tree(Generic[A]):
    """
    A binary tree node.  A leaf is represented by ``data = None``.
    Interior nodes carry a _Label and left/right children.
    """
    __slots__ = ("data", "left", "right")

    def __init__(
        self,
        data:  Optional[_Label[A]] = None,
        left:  Optional["_Tree[A]"] = None,
        right: Optional["_Tree[A]"] = None,
    ) -> None:
        self.data  = data
        self.left  = left
        self.right = right

    # -- selectors -----------------------------------------------------------

    def left_index(self) -> int:
        return -1 if self.data is None else self.data.from_idx

    def right_index(self) -> int:
        return -1 if self.data is None else self.data.to_idx

    def extract(self) -> A:
        if self.data is None or self.data.agg.is_none():
            raise ValueError("No aggregated value at tree's root")
        return self.data.agg.value

    def discharge(self) -> None:
        """Clear the aggregation of this node in-place."""
        if self.data is not None:
            self.data.agg = _none()

# -- tree constructors -------------------------------------------------------

def _leaf() -> _Tree:
    return _Tree()

def _singleton(i: int, x: A) -> _Tree[A]:
    return _Tree(data=_Label(from_idx=i, to_idx=i, agg=_some(x)))

def _combine(op: Op[_Option[A]], t1: _Tree[A], t2: _Tree[A]) -> _Tree[A]:
    """
    Merge two trees.  t1 is discharged and becomes the left child; t2 becomes
    the right child.  If either tree is a leaf, return the other unchanged.
    """
    if t1.data is None:
        return t2
    if t2.data is None:
        return t1
    v = op(t1.data.agg, t2.data.agg)
    t1.discharge()
    return _Tree(
        data  = _Label(from_idx=t1.data.from_idx, to_idx=t2.data.to_idx, agg=v),
        left  = t1,
        right = t2,
    )


# Core algorithm helpers

def _news(op:  Op[_Option[A]], s: _Stream[A], n: int, i: int, acc: _Tree[A]) -> Tuple[_Tree[A], bool]:
    """
    Read ``n`` new elements from ``s`` starting at absolute index ``i``,
    build singleton trees, and fold them (right-to-left) into ``acc``.
    Returns (updated_acc, True) or (acc, False) if the stream is exhausted.
    """
    if n == 0:
        return acc, True

    v, ok = s.read()
    if not ok:
        return acc, False

    acc, ok = _news(op, s, n - 1, i + 1, acc)
    if not ok:
        return acc, False

    return _combine(op, _singleton(i, v), acc), True


def _reusables(op: Op[_Option[A]], t: _Tree[A], i: int, acc: _Tree[A]) -> _Tree[A]:
    """
    Fold every maximal subtree of ``t`` whose index range is entirely >= ``i``
    into ``acc`` via ``combine``.  Iterative to avoid Python recursion limits.
    """
    while True:
        if i > t.right_index():
            return acc
        if i == t.left_index():
            return _combine(op, t, acc)
        # t must be an interior node here
        r_child = t.right  # type: ignore[assignment]
        l_child = t.left   # type: ignore[assignment]
        if i >= r_child.left_index():  # type: ignore[union-attr]
            t = r_child  # tail-recurse into right subtree
        else:
            acc = _combine(op, r_child, acc)
            t = l_child  # tail-recurse into left subtree


def _slide(op: Op[_Option[A]], s: _Stream[A], t: _Tree[A], w: Window) -> Tuple[_Tree[A], bool]:
    """
    Advance the tree by one window step ``w = (left, right)``.
    Returns (updated_tree, True) or (leaf, False) if the stream was exhausted.
    """
    left, right = w
    i = max(left, 1 + t.right_index())

    # Skip elements that fall between the previous window and the current one.
    if not s.skip(i):
        return _leaf(), False

    n = max(0, right - i + 1)

    # Fold newly read elements into a fresh subtree.
    r, ok = _news(op, s, n, i, _leaf())
    if not ok:
        return _leaf(), False

    # Fold reusable subtrees from the previous window.
    return _reusables(op, t, left, r), True


# Public API

def sliding_window(op: Op[A], xs: Iterator[A], windows: Iterator[Window]) -> Iterator[A]:
    """
    Compute the associative aggregation of each window over the input sequence.

    Parameters
    ----------
    op:
        An associative binary operator ``(A, A) -> A``.
    xs:
        An iterator (possibly infinite) of input elements x_0, x_1, ...
        Elements are consumed lazily: only as many as required by the windows
        seen so far are read.
    windows:
        An iterator of ``(left, right)`` index pairs (both inclusive, 0-based).
        Must satisfy:
          - 0 <= l_0 <= l_1 <= ... (left bounds non-decreasing)
          - 0 <= r_0 <= r_1 <= ... (right bounds non-decreasing)
          - l_i <= r_i for all i

    Yields
    ------
    y_i = x[l_i] op x[l_i+1] op ... op x[r_i]  for each window (l_i, r_i).
    """
    lop = _lift(op)
    s   = _Stream(iter(xs))
    t   = _leaf()

    for w in windows:
        t, ok = _slide(lop, s, t, w)
        if not ok:
            return  # input stream exhausted
        yield t.extract()
