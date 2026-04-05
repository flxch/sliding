package sliding_test

import (
    "fmt"
    "testing"
    "slices"
    "github.com/flxch/sliding"
)


var aggregationAssocTestcases []testcase = []testcase{
    testcase{
        op:       func(s, t int) int { return s + t },
        elems:    []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
        windows:  []sliding.Window{},
        expected: []int{},
    },
    testcase{
        op:       func(s, t int) int { return s + t },
        elems:    []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
        windows:  []sliding.Window{sliding.Window{0,9}},
        expected: []int{45},
    },
    testcase{
        op:       func(s, t int) int { return s + t },
        elems:    []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
        windows:  []sliding.Window{
            sliding.Window{0,0},
            sliding.Window{1,1},
            sliding.Window{2,2},
            sliding.Window{3,3}},
        expected: []int{0, 1, 2, 3},
    },
    testcase{
        op:       func(s, t int) int { return s + t },
        elems:    []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
        windows:  []sliding.Window{
            sliding.Window{0,2},
            sliding.Window{1,3},
            sliding.Window{2,4},
            sliding.Window{3,5},
            sliding.Window{4,6},
            sliding.Window{5,7},
            sliding.Window{6,8},
            sliding.Window{7,9}},
        expected: []int{3, 6, 9, 12, 15, 18, 21, 24},
    },
    testcase{
        op:       func(s, t int) int { return s + t },
        elems:    []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
        windows:  []sliding.Window{
            sliding.Window{0,1},
            sliding.Window{4,6}},
        expected: []int{1, 15},
    },
    testcase{
        op:       func(s, t int) int { return s + t },
        elems:    []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
        windows:  []sliding.Window{
            sliding.Window{2,3},
            sliding.Window{2,3},
            sliding.Window{2,5},
            sliding.Window{9,9}},
        expected: []int{5, 5, 14, 9},
    },
}

func TestAggregateAssoc(t *testing.T) {
    for i, tc := range aggregationAssocTestcases {
        res, err := runAggregateAssocTest(tc.op, tc.elems, tc.windows)
        if err != nil {
            t.Errorf("#%d: %v", i, err)
        } else if !slices.Equal(res, tc.expected) {
            t.Errorf("#%d: expected %v, got %v", i, tc.expected, res)
        }
    }
}

func TestRandomAggregateAssoc(t *testing.T) {
    count := 0
    tc := randomTestcase(func(s, t int) int { count++; return s + t }, nil, 10000, 5000, 100)
    res, err := runAggregateAssocTest(tc.op, tc.elems, tc.windows)
    if err != nil {
        t.Errorf("%v", err)
    } else if !slices.Equal(res, tc.expected) {
        t.Errorf("expected %v, got %v", tc.expected, res)
    }
    t.Logf("number of op applications: %d", count)
}


func runAggregateAssocTest[T any](op sliding.Op[T], elems []T, windows []sliding.Window) (res []T, err error) {
    in := make(chan T, 0)
    go func() {
        for _, s := range elems {
            in <- s
        }
        close(in)
    }()

    out := make(chan T, 0)
    wait := make(chan struct{}, 0)
    go func() {
        for s := range out {
            res = append(res, s)
        }
        close(wait)
    }()

    var d int
    next := func() (sliding.Window, bool) {
        if d >= len(windows) {
            return sliding.Window{}, false
        }
        defer func() { d++ }()
        return windows[d], true
    }

    // Convert panic into error.
    defer func() {
        if r := recover(); r != nil {
            err = fmt.Errorf("panic: %v", r)
        }
    }()
    sliding.AggregateAssoc(in, out, op, next)

    close(out)
    <-wait

    return res, err
}
