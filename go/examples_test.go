package sliding

import (
    "fmt"
)


func ExampleAggregate() {
    // Associative operation: string concatenation.
    op := func(s, t string) string { return s + t }

    // Goroutine that sends the data stream elements over the channel `in`.  It
    // terminates when all data elements have been sent.
    in := make(chan string, 0)
    go func() {
        elems := []string{
            "Hello", // 0
            ", ",    // 1
            "World", // 2
            "!",     // 3
            "skip",  // 4
            "foo",   // 5
            "goo",   // 6
            "moo",   // 7
            "hoo",   // 8
            "bar",   // 9
            "baz",   // 10
        }
        for _, s := range elems {
            in <- s
        }
        close(in)
    }()

    // Goroutine that receives the aggregated values over the channel `out` and
    // prints them.  The goroutine signals with `wait` channel by closing it
    // that it has terminated.
    out := make(chan string, 0)
    wait := make(chan struct{}, 0)
    go func() {
        for s := range out {
            fmt.Println(s)
        }
        // Signal termination of the outputter.
        close(wait)
    }()

    // Function for returning the next window.
    windows := []Window{
        Window{0, 0},   // "Hello"
        Window{0, 3},   // "Hello" ", " "World" "!"
        Window{2, 3},   // "World" "!"
        Window{5, 6},   // "foo" "goo"
        Window{5, 6},   // "foo" "goo"
        Window{5, 7},   // "foo" "goo" "moo"
        Window{6, 7},   // "goo" "moo"
        Window{7, 8},   // "moo" "hoo"
        Window{9, 10},  // "bar" "baz"
        Window{10, 10}, // "baz"
    }
    var d int // index in window stream
    next := func() (Window, bool) {
        if d >= len(windows) {
            return Window{}, false
        }
        defer func() { d++ }()
        return windows[d], true
    }

    // Run the sliding window algorithm.
    Aggregate(in, out, op, next)

    // Signal the outputter goroutine to terminate and wait until all aggregated
    // values have been printed, i.e., the goroutine has terminated.
    close(out)
    <-wait

    // Output:
    // Hello
    // Hello, World!
    // World!
    // foogoo
    // foogoo
    // foogoomoo
    // goomoo
    // moohoo
    // barbaz
    // baz
}
