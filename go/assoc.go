package sliding


type label[T any] struct {
    left  int
    right int
    value T
}

type tree[T any] struct {
    // Data stored at the node.
    //data  opt.Option[label[T]]
    // Children of the node.
    left  *tree[T]
    right *tree[T]
}


func AggregateAssoc[T any](in <-chan T, out chan<- T, op Op[T], ws Next[Window]) {
   // ...
}


