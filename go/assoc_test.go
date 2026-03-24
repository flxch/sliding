package sliding

import (
    "fmt"
    "testing"
    "slices"
)


func runAggregateAssocTest[T any](op Op[T], elems []T, windows []Window) (res []T, err error) {
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
    next := func() (Window, bool) {
        if d >= len(windows) {
            return Window{}, false
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
    AggregateAssoc(in, out, op, next)

    close(out)
    <-wait

    return res, err
}

func _TestAggregateAssoc(t *testing.T) {
    type testcase struct {
        op       Op[int]
        elems    []int
        windows  []Window
        expected []int
    }
    tcs := []testcase{
        testcase{
            op:       func(s, t int) int { return s + t },
            elems:    []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
            windows:  []Window{},
            expected: []int{},
        },
        testcase{
            op:       func(s, t int) int { return s + t },
            elems:    []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
            windows:  []Window{Window{0,9}},
            expected: []int{45},
        },
        testcase{
            op:       func(s, t int) int { return s + t },
            elems:    []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
            windows:  []Window{Window{0,0}, Window{1,1}, Window{2,2}, Window{3,3}},
            expected: []int{0, 1, 2, 3},
        },
        testcase{
            op:       func(s, t int) int { return s + t },
            elems:    []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
            windows:  []Window{Window{0,1}, Window{4,6}},
            expected: []int{1, 15},
        },
        testcase{
            op:       func(s, t int) int { return s + t },
            elems:    []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
            windows:  []Window{Window{2,3}, Window{2,3}, Window{2,5}, Window{9,9}},
            expected: []int{5, 5, 14, 9},
        },
    }

    for i, tc := range tcs {
        res, err := runAggregateAssocTest(tc.op, tc.elems, tc.windows)
        if err != nil {
            t.Errorf("#%d: %v", i, err)
        } else if !slices.Equal(res, tc.expected) {
            t.Errorf("#%d: expected %v, got %v", i, tc.expected, res)
        }
    }
}
