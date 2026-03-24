package sliding


// Type of an operator over the type T.
type Op[T any] func(T, T) T

// Window of elements within a data stream.  Bounds must be nonnegative and the
// right bound must not be smaller than the left bound.
type Window struct {
    Left  int
    Right int
}


// Type of a function that provides the next element of a data stream.  If the
// second return value is false, the stream has no next element.
type Next[T any] func() (T, bool)


// Interface of a sliding window.
type Slider[T any] interface {
    Slide(Op[T], Next[T], Next[Window], chan<- T)
}
