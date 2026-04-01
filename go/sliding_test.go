package sliding_test

import (
    "github.com/flxch/sliding"
)


// General declarations (types and function) for the tests of the various
// sliding algorithms.


// A test case consists of the operator (possibly also the inverse operator),
// the stream of elements, the positions of the sliding window, and the expected
// aggregations for the windows.  Here, we fix for simplicity, the type of the
// elements to int.
type testcase struct {
    op       sliding.Op[int]
    inv      sliding.Op[int]
    elems    []int
    windows  []sliding.Window
    expected []int
}
