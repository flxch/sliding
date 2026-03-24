package sliding


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

