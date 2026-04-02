"""
Unit tests for sliding.py

Run with:  python -m pytest test_sliding.py -v
       or: python test_sliding.py
"""

import itertools
import operator
import unittest
from typing import Callable, Iterable, Iterator, List, Tuple

from sliding import sliding_window

Window = Tuple[int, int]


# ---------------------------------------------------------------------------
# Reference implementation (naive O(n·w) brute force)
# ---------------------------------------------------------------------------

def _naive(op: Callable, xs: List, windows: List[Window]) -> List:
    """
    Compute aggregations without any cleverness.  Used to cross-check results.
    """
    results = []
    for l, r in windows:
        acc = xs[l]
        for i in range(l + 1, r + 1):
            acc = op(acc, xs[i])
        results.append(acc)
    return results


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _collect(op, xs, windows) -> List:
    return list(sliding_window(op, iter(xs), iter(windows)))


def _fixed_windows(n: int, size: int) -> List[Window]:
    """Sliding window of fixed size over [0, n)."""
    return [(i, i + size - 1) for i in range(n - size + 1)]


def _expanding_windows(n: int) -> List[Window]:
    """Windows that grow: (0,0), (0,1), ..., (0,n-1)."""
    return [(0, i) for i in range(n)]


def _shrinking_windows(n: int) -> List[Window]:
    """Windows where the left bound advances: (0,n-1), (1,n-1), ..., (n-1,n-1)."""
    return [(i, n - 1) for i in range(n)]


# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

class TestSlidingWindowBasic(unittest.TestCase):

    def test_single_window_full_range(self):
        xs = [3, 1, 4, 1, 5, 9, 2, 6]
        result = _collect(operator.add, xs, [(0, len(xs) - 1)])
        self.assertEqual(result, [sum(xs)])

    def test_single_element_windows(self):
        xs = [10, 20, 30, 40]
        windows = [(i, i) for i in range(len(xs))]
        result = _collect(operator.add, xs, windows)
        self.assertEqual(result, xs)

    def test_two_element_windows(self):
        xs = [1, 2, 3, 4, 5]
        windows = _fixed_windows(len(xs), 2)
        expected = _naive(operator.add, xs, windows)
        self.assertEqual(_collect(operator.add, xs, windows), expected)

    def test_fixed_window_sum(self):
        xs = list(range(10))
        for size in range(1, 10):
            windows = _fixed_windows(len(xs), size)
            expected = _naive(operator.add, xs, windows)
            self.assertEqual(_collect(operator.add, xs, windows), expected,
                             msg=f"size={size}")

    def test_expanding_windows_sum(self):
        xs = [1, 2, 3, 4, 5]
        windows = _expanding_windows(len(xs))
        expected = _naive(operator.add, xs, windows)
        self.assertEqual(_collect(operator.add, xs, windows), expected)

    def test_shrinking_windows_sum(self):
        xs = [1, 2, 3, 4, 5]
        windows = _shrinking_windows(len(xs))
        expected = _naive(operator.add, xs, windows)
        self.assertEqual(_collect(operator.add, xs, windows), expected)


class TestSlidingWindowOperators(unittest.TestCase):

    def _check(self, op, xs, windows):
        expected = _naive(op, xs, windows)
        got = _collect(op, xs, windows)
        self.assertEqual(got, expected)

    def test_multiply(self):
        xs = [2, 3, 4, 5]
        self._check(operator.mul, xs, _fixed_windows(len(xs), 2))

    def test_max(self):
        xs = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
        self._check(max, xs, _fixed_windows(len(xs), 3))

    def test_min(self):
        xs = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
        self._check(min, xs, _fixed_windows(len(xs), 4))

    def test_string_concat(self):
        xs = list("abcde")
        windows = _fixed_windows(len(xs), 3)
        self._check(operator.add, xs, windows)

    def test_list_concat(self):
        xs = [[i] for i in range(5)]
        windows = _fixed_windows(len(xs), 2)
        self._check(operator.add, xs, windows)

    def test_bitwise_or(self):
        xs = [0b0001, 0b0010, 0b0100, 0b1000, 0b0001]
        windows = _fixed_windows(len(xs), 3)
        self._check(operator.or_, xs, windows)

    def test_bitwise_and(self):
        xs = [0b1111, 0b1110, 0b1100, 0b1000, 0b1111]
        windows = _fixed_windows(len(xs), 3)
        self._check(operator.and_, xs, windows)


class TestSlidingWindowEdgeCases(unittest.TestCase):

    def test_empty_windows(self):
        xs = [1, 2, 3]
        result = _collect(operator.add, xs, [])
        self.assertEqual(result, [])

    def test_single_element_sequence(self):
        xs = [42]
        result = _collect(operator.add, xs, [(0, 0)])
        self.assertEqual(result, [42])

    def test_same_window_repeated(self):
        xs = [1, 2, 3, 4, 5]
        windows = [(1, 3)] * 5
        result = _collect(operator.add, xs, windows)
        expected = [sum(xs[1:4])] * 5
        self.assertEqual(result, expected)

    def test_adjacent_non_overlapping_windows(self):
        xs = [1, 2, 3, 4, 5, 6]
        windows = [(0, 1), (2, 3), (4, 5)]
        expected = _naive(operator.add, xs, windows)
        self.assertEqual(_collect(operator.add, xs, windows), expected)

    def test_windows_with_gap(self):
        # Left bound jumps, skipping some elements.
        xs = list(range(10))
        windows = [(0, 2), (5, 7), (8, 9)]
        expected = _naive(operator.add, xs, windows)
        self.assertEqual(_collect(operator.add, xs, windows), expected)

    def test_variable_size_windows(self):
        xs = [2, 3, 5, 7, 11, 13, 17, 19]
        windows = [(0, 2), (1, 4), (2, 5), (3, 7)]
        expected = _naive(operator.add, xs, windows)
        self.assertEqual(_collect(operator.add, xs, windows), expected)


class TestSlidingWindowLarger(unittest.TestCase):

    def test_large_fixed_window_sum(self):
        n = 500
        xs = list(range(n))
        for size in [1, 10, 50, 100, 499, 500]:
            windows = _fixed_windows(n, size)
            expected = _naive(operator.add, xs, windows)
            got = _collect(operator.add, xs, windows)
            self.assertEqual(got, expected, msg=f"size={size}")

    def test_large_max(self):
        import random
        random.seed(0)
        n = 300
        xs = [random.randint(0, 1000) for _ in range(n)]
        windows = _fixed_windows(n, 20)
        expected = _naive(max, xs, windows)
        self.assertEqual(_collect(max, xs, windows), expected)


class TestSlidingWindowInfiniteInput(unittest.TestCase):
    """The algorithm should work on infinite iterators, reading lazily."""

    def test_finite_windows_on_infinite_stream(self):
        # Natural numbers: 0, 1, 2, 3, ...
        nats: Iterator[int] = itertools.count(0)
        windows = [(0, 2), (1, 4), (3, 6)]
        expected = _naive(operator.add, list(range(10)), windows)
        result = list(sliding_window(operator.add, nats, iter(windows)))
        self.assertEqual(result, expected)

    def test_infinite_fixed_windows_on_infinite_stream(self):
        # Take the first 50 results from an infinite stream with window size 5.
        nats: Iterator[int] = itertools.count(0)
        window_gen = ((i, i + 4) for i in range(50))
        result = list(sliding_window(operator.add, nats, window_gen))
        xs = list(range(50 + 4))  # enough elements
        expected = _naive(operator.add, xs, _fixed_windows(len(xs), 5))
        self.assertEqual(result, expected)


class TestSlidingWindowOperatorCount(unittest.TestCase):
    """
    Verify that the number of operator applications is sub-linear (optimal).
    The algorithm guarantees O(n log w) total operations for n windows of
    width w over an n-element sequence; here we just sanity-check it is
    strictly less than the naive O(n·w) count.
    """

    def test_op_count_less_than_naive(self):
        call_count = {"n": 0}

        def counting_add(a, b):
            call_count["n"] += 1
            return a + b

        n = 200
        size = 20
        xs = list(range(n))
        windows = _fixed_windows(n, size)

        list(sliding_window(counting_add, iter(xs), iter(windows)))
        smart_count = call_count["n"]

        # Naive count: (size - 1) applications per window
        naive_count = (size - 1) * len(windows)

        self.assertLess(smart_count, naive_count,
                        msg=f"smart={smart_count} naive={naive_count}")


if __name__ == "__main__":
    unittest.main(verbosity=2)
