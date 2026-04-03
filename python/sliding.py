"""
Sliding window aggregation algorithm.

Python design notes
-------------------
The Go implementation uses channels for the input stream.  In Python the
natural equivalent is an *iterator* (or generator): it is lazy, works for
infinite sequences, and requires no threads or queues.  The internal stream
helper below wraps any iterator and adds the same two primitives that the Go
`input` struct exposes:
  - read()  – advance and return the next element
  - skip(n) – discard elements until the absolute position reaches n

Both methods raise _StreamExhausted if the underlying iterator is exhausted.
This is a dedicated exception rather than StopIteration so that stream
exhaustion is never confused with the StopIteration that Python uses
internally to drive for-loops and generators (see PEP 479).
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, Generic, Iterator, Optional, Tuple, TypeVar

A = TypeVar("A")
Op = Callable[[A, A], A]
Window = Tuple[int, int]  # (left_index, right_index), both inclusive


# Stream exhaustion exception

class _StreamExhausted(Exception):
    """Raised when the input stream is exhausted before a window is complete."""

# Internal stream wrapper

class _Stream(Generic[A]):
    """Wraps an iterator and tracks the absolute read position."""

    def __init__(self, it: Iterator[A]) -> None:
        self._it = it
        self.count: int = 0  # number of elements already consumed

    def read(self) -> A:
        """Return the next element, advancing the position.
        Raises _StreamExhausted if the stream is exhausted."""
        try:
            v = next(self._it)
        except StopIteration:
            raise _StreamExhausted("input stream exhausted while reading element")
        self.count += 1
        return v

    def skip(self, n: int) -> None:
        """Discard elements until self.count == n.
        Raises _StreamExhausted if the stream is exhausted before reaching n."""
        while self.count < n:
            try:
                next(self._it)
            except StopIteration:
                raise _StreamExhausted("input stream exhausted while skipping elements")
            self.count += 1


# Tree

@dataclass
class _Label(Generic[A]):
    from_idx: int
    to_idx:   int
    agg:      Optional[A]  # not None for live nodes, None for discharged nodes

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
        if self.data is None or self.data.agg is None:
            raise ValueError("No aggregated value at tree's root")
        return self.data.agg

    def discharge(self) -> None:
        """Clear the aggregation of this node in-place."""
        if self.data is not None:
            self.data.agg = None

# -- tree constructors -------------------------------------------------------

def _leaf() -> _Tree:
    return _Tree()

def _singleton(i: int, x: A) -> _Tree[A]:
    return _Tree(data=_Label(from_idx=i, to_idx=i, agg=x))

def _lift(op: Op[A]) -> Op[Optional[A]]:
    """Lift an operator to work on Optional values."""
    def lifted(x: Optional[A], y: Optional[A]) -> Optional[A]:
        if x is None or y is None:
            return None
        return op(x, y)
    return lifted

def _combine(op: Op[Optional[A]], t1: _Tree[A], t2: _Tree[A]) -> _Tree[A]:
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

def _news(op: Op[Optional[A]], s: _Stream[A], n: int, i: int, acc: _Tree[A]) -> _Tree[A]:
    """
    Read ``n`` new elements from ``s`` starting at absolute index ``i``,
    build singleton trees, and fold them (right-to-left) into ``acc``.
    Raises _StreamExhausted if the stream is exhausted before all n elements
    are read.
    """
    if n == 0:
        return acc
    v   = s.read()
    acc = _news(op, s, n - 1, i + 1, acc)
    return _combine(op, _singleton(i, v), acc)


def _reusables(op: Op[Optional[A]], t: _Tree[A], i: int, acc: _Tree[A]) -> _Tree[A]:
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
            t = r_child   # tail-recurse into right subtree
        else:
            acc = _combine(op, r_child, acc)
            t   = l_child  # tail-recurse into left subtree

def _slide(op: Op[Optional[A]], s: _Stream[A], t: _Tree[A], w: Window) -> _Tree[A]:
    """
    Advance the tree by one window step ``w = (left, right)``.
    Raises _StreamExhausted if the stream is exhausted before all required
    elements are available.
    """
    left, right = w
    i = max(left, 1 + t.right_index())

    # Skip elements that fall between the previous window and the current one.
    s.skip(i)

    # Fold newly read elements into a fresh subtree.
    r = _news(op, s, max(0, right - i + 1), i, _leaf())

    # Fold reusable subtrees from the previous window.
    return _reusables(op, t, left, r)


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
        try:
            t = _slide(lop, s, t, w)
        except _StreamExhausted:
            return  # input stream exhausted before window was complete
        yield t.extract()
