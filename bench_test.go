package sliding_test

import (
    "fmt"
    "math/rand/v2"
    "testing"
    "github.com/flxch/sliding"
)


// Benchmark parameters.

const (
    streamlen = 10000 // stream length
    winsize   = 1000  // approximate window size
    chsize    = 10    // channel size (in and out)
    delay     = 10    // delay for aggregation operators
)

// Number of windows (equals the number of aggregated values).
var winnums []int = []int{100, 200, 400, 800}


// A function that keeps the CPU busy for some time.  Used in delay the
// aggregation operators op and inv.
func fib(n int) int {
    if n == 0 {
        return 0
    }
    if n == 1 {
        return 1
    }
    return fib(n - 1) + fib(n - 2)
}


// Measure the time for a single operation that is used in the aggregation
// benchmarks below.

var f int
func BenchmarkOp(b *testing.B) {
    b.StopTimer()
    b.ReportAllocs()
    op := func(x, y int) int { f = fib(rand.IntN(delay)); return x + y }
    for i := 0; i < b.N; i++ {
        x, y := int(rand.Int32()), int(rand.Int32())
        b.StartTimer()
        f = op(x, y)
        b.StopTimer()
    }
}

func BenchmarkInv(b *testing.B) {
    b.StopTimer()
    b.ReportAllocs()
    inv := func(x, y int) int { f = fib(rand.IntN(delay)); return x - y }
    for i := 0; i < b.N; i++ {
        x, y := int(rand.Int32()), int(rand.Int32())
        b.StartTimer()
        f = inv(x, y)
        b.StopTimer()
    }
}


// Measure the time for computing of aggregating stream elements over a sliding
// window.

type aggregFn func (<-chan int, chan<- int, sliding.Op[int], sliding.Op[int], sliding.Next[sliding.Window])

func BenchmarkAggregate(b *testing.B) {
    b.Logf("stream length: %d, window size: %d", streamlen, winsize)
    wrap := func(in <-chan int, out chan<- int, op, inv sliding.Op[int], next sliding.Next[sliding.Window]) {
        sliding.Aggregate(in, out, op, next)
    }
    op := func(x, y int) int { f = fib(rand.IntN(delay)); return x + y }
    for _, winnum := range winnums {
        b.Run(fmt.Sprintf("#win=%d", winnum), func(b *testing.B) {
            b.StopTimer()
            b.ReportAllocs()
            for i := 0; i < b.N; i++ {
                run(b, randomBenchmark(op, nil, streamlen, winnum, winsize), wrap)
            }
        })
    }
}

func BenchmarkAggregateAssoc(b *testing.B) {
    b.Logf("stream length: %d, window size: %d", streamlen, winsize)
    wrap := func(in <-chan int, out chan<- int, op, inv sliding.Op[int], next sliding.Next[sliding.Window]) {
        sliding.AggregateAssoc(in, out, op, next)
    }
    op := func(x, y int) int { f = fib(rand.IntN(delay)); return x + y }
    for _, winnum := range winnums {
        b.Run(fmt.Sprintf("#win=%d", winnum), func(b *testing.B) {
            b.StopTimer()
            b.ReportAllocs()
            for i := 0; i < b.N; i++ {
                run(b, randomBenchmark(op, nil, streamlen, winnum, winsize), wrap)
            }
        })
    }
}

func BenchmarkAggregateInv(b *testing.B) {
    b.Logf("stream length: %d, window size: %d", streamlen, winsize)
    op := func(x, y int) int { f = fib(rand.IntN(delay)); return x + y }
    inv := func(x, y int) int { f = fib(rand.IntN(delay)); return x - y }
    for _, winnum := range winnums {
        b.Run(fmt.Sprintf("#win=%d", winnum), func(b *testing.B) {
            b.StopTimer()
            b.ReportAllocs()
            for i := 0; i < b.N; i++ {
                run(b, randomBenchmark(op, inv, streamlen, winnum, winsize), sliding.AggregateInv)
            }
        })
    }
}

var global int
func run(b *testing.B, tc testcase, aggregate aggregFn) {
    in := make(chan int, chsize)
    go func() {
        for _, s := range tc.elems {
            in <- s
        }
        close(in)
    }()

    out := make(chan int, chsize)
    wait := make(chan struct{}, 0)
    go func() {
        for s := range out {
            global = s
        }
        close(wait)
    }()

    var d int
    next := func() (sliding.Window, bool) {
        if d >= len(tc.windows) {
            return sliding.Window{}, false
        }
        defer func() { d++ }()
        return tc.windows[d], true
    }

    // Convert panic into error.
    defer func() {
        if r := recover(); r != nil {
            b.Errorf("panic: %v", r)
        }
    }()

    b.StartTimer()
    aggregate(in, out, tc.op, tc.inv, next)
    b.StopTimer()

    close(out)
    <-wait
}

