package sliding

import (
    "fmt"
)


// `Aggregate` agreggrates elements of a data stream over type T within windows.
// It uses the function `op` for aggregation.  No assumption are made on `op`.
// The function `elems` provides the next element of the stream.  The `wins`
// function provides the next window.  It is assumed that the window bounds are
// monotonically increasing.  The aggregated elements for the windows are
// returned over the channel `out`.
func Aggregate[T any](in <-chan T, out chan<- T, op Op[T], ws Next[Window]) {
    // Aggreates the values in ds and return the aggregated value.  It is
    // assumed that ds is nonempty.
    aggregate := func(ds []T) T {
        if len(ds) == 0 {
            panic("empty window")
        }
        if len(ds) == 1 {
            return ds[0]
        }
        r := op(ds[0], ds[1])
        for _, d := range ds[2:] {
            r = op(r, d)
        }
        return r
    }

    n := 0
    ds := []T{}
    // Read the next element from the data stream and store it in ds.  Return
    // false if the end of the stream has been reached.
    next := func(add bool) bool {
        d, ok := <-in
        if ok {
            n++
            if add {
                ds = append(ds, d)
            }
        }
        return ok
    }

    // Process windows iteratively.  For each window, read all data elements
    // first until the window is complete.  Afterwards, aggregate them.
    for {
        // Process next window.
        w, ok := ws()
        if !ok {
            // All windows have been processed.  Terminate.
            break
        }
        w.Left -= (n - len(ds))
        w.Right -= (n - len(ds))

        // Skip data elements on the left side of the window.
        if len(ds) > w.Left {
            ds = ds[w.Left:]
        } else {
            m := len(ds)
            ds = ds[:0]
            for i := m; i < w.Left; i++ {
                if !next(false) {
                    panic("incomplete window")
                }
            }
        }

        // Shift window bounds so that the left bound is 0.
        w.Right -= w.Left
        w.Left = 0

        // Read data elements until window is complete.
        for len(ds) <= w.Right {
            if !next(true) {
                panic("incomplete window")
            }
        }

        // Send aggregated value over channel.
        out <- aggregate(ds)
    }
}


type Aggregation[T any] struct {
    // Static.
    in     <-chan T
    op     Op[T]
    next   Next[Window]
    // Dynamic.
    // Data elements that must be skipped for next window.
    skip   int
    // Current window.
    window Window
    // Buffered data elements within current window.
    elems  []T
}

func NewAggregation[T any](in <-chan T, op Op[T], next Next[Window]) *Aggregation[T] {
    w, ok := next()
    if !ok {
        return nil
    }
    return &Aggregation[T]{
        in:     in,
        op:     op,
        next:   next,
        skip:   w.Left,
        window: w,
    }
}

func (aggreg *Aggregation[T]) aggregate() (T, error) {
    var r T
    if len(aggreg.elems) == 0 {
        return r, fmt.Errorf("empty window")
    }
    if len(aggreg.elems) == 1 {
        return aggreg.elems[0], nil
    }
    r = aggreg.op(aggreg.elems[0], aggreg.elems[1])
    for _, elem := range aggreg.elems[2:] {
        r = aggreg.op(r, elem)
    }
    return r, nil
}

func (aggreg *Aggregation[T]) nextWindow() (Window, error) {
    w := aggreg.window
    var ok bool
    aggreg.window, ok = aggreg.next()
    if !ok {
        return Window{}, fmt.Errorf("failed to move window")
    }
    if k := aggreg.window.Left - w.Left; k < len(aggreg.elems) {
        aggreg.elems = aggreg.elems[k:]
    } else {
        aggreg.elems = aggreg.elems[:0]
        aggreg.skip = k
    }
    return w, nil
}

// Step is called when a new element is received.
func (aggreg *Aggregation[T]) Step(elem T, out chan<- T) error {
    if aggreg.skip > 0 {
        aggreg.skip--
        return nil
    }

    // Buffer newly received element.
    aggreg.elems = append(aggreg.elems, elem)

    // If window is complete, send aggregated value over output channel.
    if len(aggreg.elems) == aggreg.window.Right - aggreg.window.Left + 1 {
        s, err := aggreg.aggregate()
        if err != nil {
            return fmt.Errorf("failed to aggregate values in window: %w", err)
        }

        // Send aggregated values as long as the right window bound does not
        // move.
        w := aggreg.window
        for aggreg.window.Right == w.Right {
            if aggreg.window.Left > w.Left {
                // The left bound has moved; recompute aggregation.
                if s, err = aggreg.aggregate(); err != nil {
                    // TODO: We return an error here. This leads to a
                    // inconsistent state.  How do we get to a safe state before
                    // we return an error.  The same problem happens above.
                    return fmt.Errorf("failed to aggregate values in window: %w", err)
                }
            }

            out <- s

            if w, err = aggreg.nextWindow(); err != nil {
                return err
            }
        }
    }

    return nil
}
