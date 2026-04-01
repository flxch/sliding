package sliding

import (
    "fmt"
)


func AggregateInv[T any](in <-chan T, out chan<- T, op, inv Op[T], ws Next[Window]) {
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
    var val T
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
        for w.Left > 0 {
            w.Left--
            w.Right--
            if len(ds) > 0 {
                val = inv(val, ds[0])
                ds = ds[1:]
            } else if !next(false) {
                panic("incomplete window")
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
            val = op(val, ds[len(ds)-1])
        }

        // Send aggregated value over channel.
        out <- val
    }
}


type InvAggregation[T any] struct {
    // Static.
    in     <-chan T
    op     Op[T]
    inv    Op[T]
    next   Next[Window]
    // Dynamic.
    skip   int
    window Window
    elems  []T
    value  T
}

func NewInvAggregation[T any](in <-chan T, op, inv Op[T], next Next[Window]) *InvAggregation[T] {
    w, ok := next()
    if !ok {
        // Special case: no window.
        return nil
    }
    return &InvAggregation[T]{
        in:     in,
        op:     op,
        inv:    inv,
        next:   next,
        skip:   w.Left,
        window: w,
    }
}

func (aggreg *InvAggregation[T]) aggregate() (T, error) {
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

func (aggreg *InvAggregation[T]) nextWindow() (Window, error) {
    w := aggreg.window
    var ok bool
    if aggreg.window, ok = aggreg.next(); !ok {
        return Window{}, fmt.Errorf("failed to move window")
    }
    if k := aggreg.window.Left - w.Left; k < len(aggreg.elems) {
        aggreg.elems = aggreg.elems[k:]
    } else {
        aggreg.skip = k - len(aggreg.elems)
        aggreg.elems = aggreg.elems[:0]
    }
    return w, nil
}

func (aggreg *InvAggregation[T]) Step(elem T, out chan<- T) error {
    if aggreg == nil {
        // Special case: no window.
        return nil
    }
    if aggreg.skip > 0 {
        // Skip element since it is not contained in the current window.
        aggreg.skip--
        return nil
    }

    // Buffer newly received element, which is contained in the current window.
    aggreg.elems = append(aggreg.elems, elem)
    // Update aggregated value with newly received element.
    aggreg.value  = aggreg.op(aggreg.value, elem)

    // If the current window is complete, send aggregated value over output
    // channel.
    if len(aggreg.elems) == aggreg.window.Right - aggreg.window.Left + 1 {
        w := aggreg.window

        // Send aggregated values as long as the right window bound does not
        // move.
        for w.Right == aggreg.window.Right {
            // Send aggregated value.
            out <- aggreg.value
            // Get next window.
            var ok bool
            if aggreg.window, ok = aggreg.next(); !ok {
                return fmt.Errorf("no next window")
            }

            // Inverse aggregated value by buffered elements that are not
            // anymore in the window.
            for w.Left < aggreg.window.Left && len(aggreg.elems) > 0 {
                aggreg.value = aggreg.inv(aggreg.value, aggreg.elems[0])
                aggreg.elems = aggreg.elems[1:]
                w.Left++
            }

            // If the buffered elements are nonempty, the current window
            // contains the remaining buffered elements.  Otherwise, some
            // element may need to be skipped before the stream reaches the
            // window's left bound.
            if len(aggreg.elems) == 0 {
                aggreg.skip = aggreg.window.Left - w.Right - 1
            }
        }
    }

    return nil
}
