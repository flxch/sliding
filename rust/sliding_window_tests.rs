//! Unit tests for the sliding-window aggregation library.
//!
//! Tests are organised into several groups:
//!   1. Basic correctness (sum, product, max, min)
//!   2. Edge cases (single-element windows, full-sequence window, adjacent windows)
//!   3. Operator variety (string concatenation, custom struct)
//!   4. Sliding behaviour (windows with gaps, windows that shrink or grow)
//!   5. Lazy / infinite streams

use sliding_window::{sliding_window, Window};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn w(left: usize, right: usize) -> Window {
    Window { left, right }
}

/// Compute each window aggregate by naive linear scan, for use as a reference.
fn naive<A, Op>(op: Op, xs: &[A], windows: &[Window]) -> Vec<A>
where
    Op: Fn(&A, &A) -> A,
    A:  Clone,
{
    windows
        .iter()
        .map(|win| {
            let mut acc = xs[win.left].clone();
            for i in (win.left + 1)..=win.right {
                acc = op(&acc, &xs[i]);
            }
            acc
        })
        .collect()
}

// ---------------------------------------------------------------------------
// 1. Basic correctness
// ---------------------------------------------------------------------------

#[test]
fn sum_fixed_width_3() {
    let xs = vec![1i64, 2, 3, 4, 5, 6, 7];
    let ws = vec![w(0,2), w(1,3), w(2,4), w(3,5), w(4,6)];

    let got: Vec<i64> = sliding_window(|a, b| a + b, xs.clone(), ws.clone()).collect();
    assert_eq!(got, naive(|a, b| a + b, &xs, &ws));
}

#[test]
fn product_fixed_width_2() {
    let xs = vec![1i64, 2, 3, 4, 5];
    let ws = vec![w(0,1), w(1,2), w(2,3), w(3,4)];

    let got: Vec<i64> = sliding_window(|a, b| a * b, xs.clone(), ws.clone()).collect();
    assert_eq!(got, naive(|a, b| a * b, &xs, &ws));
}

#[test]
fn max_fixed_width_3() {
    let xs = vec![3i32, 1, 4, 1, 5, 9, 2, 6];
    let ws = vec![w(0,2), w(1,3), w(2,4), w(3,5), w(4,6), w(5,7)];

    let got: Vec<i32> = sliding_window(|a, b| *a.max(b), xs.clone(), ws.clone()).collect();
    assert_eq!(got, naive(|a, b| *a.max(b), &xs, &ws));
}

#[test]
fn min_fixed_width_3() {
    let xs = vec![3i32, 1, 4, 1, 5, 9, 2, 6];
    let ws = vec![w(0,2), w(1,3), w(2,4), w(3,5), w(4,6), w(5,7)];

    let got: Vec<i32> = sliding_window(|a, b| *a.min(b), xs.clone(), ws.clone()).collect();
    assert_eq!(got, naive(|a, b| *a.min(b), &xs, &ws));
}

// ---------------------------------------------------------------------------
// 2. Edge cases
// ---------------------------------------------------------------------------

#[test]
fn single_window_full_sequence() {
    let xs = vec![10i32, 20, 30, 40, 50];
    let ws = vec![w(0, 4)];

    let got: Vec<i32> = sliding_window(|a, b| a + b, xs.clone(), ws.clone()).collect();
    assert_eq!(got, vec![150]);
}

#[test]
fn single_element_windows() {
    let xs = vec![7i32, 3, 11, 5];
    let ws = vec![w(0,0), w(1,1), w(2,2), w(3,3)];

    let got: Vec<i32> = sliding_window(|a, b| a + b, xs.clone(), ws.clone()).collect();
    assert_eq!(got, vec![7, 3, 11, 5]);
}

#[test]
fn single_window_single_element() {
    let xs = vec![42i32];
    let ws = vec![w(0, 0)];

    let got: Vec<i32> = sliding_window(|a, b| a + b, xs, ws).collect();
    assert_eq!(got, vec![42]);
}

#[test]
fn no_windows_returns_empty() {
    let xs: Vec<i32> = vec![1, 2, 3];
    let ws: Vec<Window> = vec![];

    let got: Vec<i32> = sliding_window(|a, b| a + b, xs, ws).collect();
    assert!(got.is_empty());
}

#[test]
fn two_adjacent_non_overlapping_windows() {
    // [0,2] then [3,5] – the windows share no elements.
    let xs = vec![1i32, 2, 3, 4, 5, 6];
    let ws = vec![w(0, 2), w(3, 5)];

    let got: Vec<i32> = sliding_window(|a, b| a + b, xs.clone(), ws.clone()).collect();
    assert_eq!(got, naive(|a, b| a + b, &xs, &ws));
}

// ---------------------------------------------------------------------------
// 3. Operator variety
// ---------------------------------------------------------------------------

#[test]
fn string_concatenation() {
    let xs = vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string()];
    let ws = vec![w(0,1), w(1,2), w(2,3)];

    let got: Vec<String> =
        sliding_window(|a: &String, b: &String| format!("{}{}", a, b), xs, ws).collect();
    assert_eq!(got, vec!["ab", "bc", "cd"]);
}

#[test]
fn bitwise_or() {
    let xs: Vec<u32> = vec![0b0001, 0b0010, 0b0100, 0b1000];
    let ws = vec![w(0,1), w(1,2), w(2,3), w(0,3)];

    let got: Vec<u32> = sliding_window(|a, b| a | b, xs.clone(), ws.clone()).collect();
    assert_eq!(got, naive(|a, b| a | b, &xs, &ws));
}

/// A custom associative operation: keep the element with the larger absolute value.
#[test]
fn custom_abs_max() {
    let xs = vec![-5i32, 3, -8, 2, 7];
    let ws = vec![w(0,2), w(1,3), w(2,4)];

    let abs_max = |a: &i32, b: &i32| if a.abs() >= b.abs() { *a } else { *b };

    let got: Vec<i32> = sliding_window(abs_max, xs.clone(), ws.clone()).collect();
    assert_eq!(got, naive(abs_max, &xs, &ws));
}

// ---------------------------------------------------------------------------
// 4. Variable-width and non-unit-stride windows
// ---------------------------------------------------------------------------

#[test]
fn variable_width_windows() {
    // Window widths: 1, 2, 3, 4, 5
    let xs = vec![1i32, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    let ws = vec![w(0,0), w(1,2), w(2,4), w(3,6), w(4,8)];

    let got: Vec<i32> = sliding_window(|a, b| a + b, xs.clone(), ws.clone()).collect();
    assert_eq!(got, naive(|a, b| a + b, &xs, &ws));
}

#[test]
fn windows_with_gap_between_them() {
    // [0,1] then [4,5] – elements 2 and 3 are skipped entirely.
    let xs = vec![10i32, 20, 99, 99, 30, 40];
    let ws = vec![w(0, 1), w(4, 5)];

    let got: Vec<i32> = sliding_window(|a, b| a + b, xs.clone(), ws.clone()).collect();
    assert_eq!(got, naive(|a, b| a + b, &xs, &ws));
}

#[test]
fn growing_windows() {
    // Left boundary fixed at 0, right boundary grows.
    let xs = vec![1i32, 1, 1, 1, 1];
    let ws = vec![w(0,0), w(0,1), w(0,2), w(0,3), w(0,4)];

    let got: Vec<i32> = sliding_window(|a, b| a + b, xs.clone(), ws.clone()).collect();
    assert_eq!(got, vec![1, 2, 3, 4, 5]);
}

#[test]
fn shrinking_then_growing_windows() {
    // Right boundary moves left-to-right while left boundary also advances.
    let xs = vec![1i32, 2, 3, 4, 5, 6, 7, 8];
    let ws = vec![w(0,4), w(1,5), w(2,5), w(3,6), w(4,7)];

    let got: Vec<i32> = sliding_window(|a, b| a + b, xs.clone(), ws.clone()).collect();
    assert_eq!(got, naive(|a, b| a + b, &xs, &ws));
}

#[test]
fn windows_that_advance_by_more_than_one() {
    // Left and right both jump by 2 each step.
    let xs = vec![1i32, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    let ws = vec![w(0,1), w(2,3), w(4,5), w(6,7), w(8,9)];

    let got: Vec<i32> = sliding_window(|a, b| a + b, xs.clone(), ws.clone()).collect();
    assert_eq!(got, vec![3, 7, 11, 15, 19]);
}

// ---------------------------------------------------------------------------
// 5. Large-scale stress test (correctness against naive reference)
// ---------------------------------------------------------------------------

#[test]
fn stress_random_windows_sum() {
    // Deterministic pseudo-random sequence (no rand dependency needed).
    let xs: Vec<i64> = (0..200).map(|i| (i * 31 + 7) % 97).collect();

    // Build a valid window sequence: left and right both non-decreasing,
    // left <= right.
    let mut windows = Vec::new();
    let (mut l, mut r) = (0usize, 4usize);
    while r < xs.len() {
        windows.push(w(l, r));
        // Advance: sometimes widen, sometimes shift.
        if (l + r) % 3 == 0 { l += 1; }
        r += 1;
        r = r.min(xs.len() - 1);
        l = l.min(r);
    }

    let got: Vec<i64> = sliding_window(|a, b| a + b, xs.clone(), windows.clone()).collect();
    let expected       = naive(|a, b| a + b, &xs, &windows);
    assert_eq!(got, expected, "stress test failed");
}

// ---------------------------------------------------------------------------
// 6. Lazy / streaming behaviour
// ---------------------------------------------------------------------------

#[test]
fn works_with_iterator_not_collected_upfront() {
    // Pass a lazy `map` iterator rather than a Vec.
    let xs_iter = (0i64..10).map(|x| x * x); // 0,1,4,9,16,25,36,49,64,81
    let xs: Vec<i64> = (0i64..10).map(|x| x * x).collect();

    let ws = vec![w(0,2), w(1,3), w(2,4), w(5,7)];

    let got: Vec<i64> = sliding_window(|a, b| a + b, xs_iter, ws.clone()).collect();
    assert_eq!(got, naive(|a, b| a + b, &xs, &ws));
}
