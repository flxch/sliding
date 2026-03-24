package sliding


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
        /*
        for w.Left > 0 {
            w.Left--
            w.Right--
            if len(ds) > 0 {
                ds = ds[1:]
            } else if !next(false) {
                panic("incomplete window")
            }
        }
        */
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
