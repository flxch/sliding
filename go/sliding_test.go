package sliding_test

import (
    "github.com/flxch/sliding"
)


type testcase struct {
    op       sliding.Op[int]
    inv      sliding.Op[int]
    elems    []int
    windows  []sliding.Window
    expected []int
}
