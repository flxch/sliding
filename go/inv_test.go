package sliding_test

import (
    "bytes"
    "fmt"
    "slices"
    "testing"
    "github.com/flxch/pipeline"
    "github.com/flxch/sliding"
)


var aggregationInvTestcases []testcase =  []testcase{
    testcase{
        op:       func(s, t int) int { return s + t },
        inv:      func(s, t int) int { return s - t },
        elems:    []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
        windows:  []sliding.Window{},
        expected: []int{},
    },
    testcase{
        op:       func(s, t int) int { return s + t },
        inv:      func(s, t int) int { return s - t },
        elems:    []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
        windows:  []sliding.Window{sliding.Window{0,9}},
        expected: []int{45},
    },
    testcase{
        op:       func(s, t int) int { return s + t },
        inv:      func(s, t int) int { return s - t },
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
        inv:      func(s, t int) int { return s - t },
        elems:    []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
        windows:  []sliding.Window{
            sliding.Window{0,1},
            sliding.Window{4,6}},
        expected: []int{1, 15},
    },
    testcase{
        op:       func(s, t int) int { return s + t },
        inv:      func(s, t int) int { return s - t },
        elems:    []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
        windows:  []sliding.Window{
            sliding.Window{2,3},
            sliding.Window{2,3},
            sliding.Window{2,5},
            sliding.Window{9,9}},
        expected: []int{5, 5, 14, 9},
    },
}

func TestAggregateInv(t *testing.T) {
    for i, tc := range aggregationInvTestcases {
        res, err := runAggregateInvTest(tc.op, tc.inv, tc.elems, tc.windows)
        if err != nil {
            t.Errorf("#%d: %v", i, err)
        } else if !slices.Equal(res, tc.expected) {
            t.Errorf("#%d: expected %v, got %v", i, tc.expected, res)
        }
    }
}

func TestRandomAggregateInv(t *testing.T) {
    opcount, invcount := 0, 0
    tc := randomTestCase(func(s, t int) int { opcount++; return s + t }, func(s, t int) int { invcount++; return s - t }, 10000, 5000, 100)
    res, err := runAggregateInvTest(tc.op, tc.inv, tc.elems, tc.windows)
    if err != nil {
        t.Errorf("%v", err)
    } else if !slices.Equal(res, tc.expected) {
        t.Errorf("expected %v, got %v", tc.expected, res)
    }
    t.Logf("number of op and inv applications: %d + %d = %d", opcount, invcount, opcount + invcount)
}

func TestAggregationInv(t *testing.T) {
    for i, tc := range aggregationInvTestcases {
        res, err := runAggregationInvTest(tc.op, tc.inv, tc.elems, tc.windows)
        if err != nil {
            t.Errorf("#%d: %v", i, err)
        } else if !slices.Equal(res, tc.expected) {
            t.Errorf("#%d: expected %v, got %v", i, tc.expected, res)
        }
    }
}


// Auxiliary functions for running the tests.

func runAggregateInvTest[T any](op, inv sliding.Op[T], elems []T, windows []sliding.Window) (res []T, err error) {
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
    sliding.AggregateInv(in, out, op, inv, next)

    close(out)
    <-wait

    return res, err
}

func runAggregationInvTest(op, inv sliding.Op[int], elems []int, windows []sliding.Window) ([]int, error) {
    inbuf := bytes.NewBuffer([]byte{})
    for _, n := range elems {
        inbuf.WriteByte(byte(n))
    }
    outbuf := bytes.NewBuffer(nil)

    p := pipeline.New(nil, 1, 1, 0)
    inch := pipeline.AddSpout(p, "input", inbuf,
        func(in []byte) (byte, error) { return in[0], nil })
    w := -1
    aggreg := sliding.NewInvAggregation(inch,
        func(s, t byte) byte { return s + t },
        func(s, t byte) byte { return s - t },
        func() (sliding.Window, bool) {
            w++
            if w >= len(windows)  {
                return sliding.Window{}, false
            }
            return windows[w], true
        })
    outch := pipeline.AddStage(p, "aggregation", inch, aggreg.Step)
    pipeline.AddSink(p, "output", outch, outbuf,
        func(data byte) ([]byte, error) { return []byte{data}, nil })

    p.Run()
    for inbuf.Len() > 0 { }
    if err := p.Close(); err != nil {
        return nil, err
    }

    res := make([]int, outbuf.Len())
    for i, n := range outbuf.Bytes() {
        res[i] = int(n)
    }
    return res, nil
}
