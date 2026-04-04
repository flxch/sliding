package sliding


// TODO: Use extra package `container/maybe` with the Option[A] type.
// >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
// `option[A]` represents an optional value: `ok == true` means that `value` is
// valid and `ok == false` means no value is present.
type option[A any] struct {
    value A
    ok    bool
}

// `some` wraps the value `v` in an option.
func some[A any](v A) option[A] { return option[A]{value: v, ok: true} }

// `none` returns an empty option.
func none[A any]() option[A] { return option[A]{} }

// `isSome` reports whether the option holds a value.
func (o option[A]) isSome() bool { return o.ok }

// `isNone` reports whether the option holds no value.
func (o option[A]) isNone() bool { return !o.ok }

// `lift` wraps the operator `op` so it works on option values: the result is
// some(op(x, y)) when both inputs are some, and none, otherwise.
func lift[A any](op Op[A]) Op[option[A]] {
    return func(x, y option[A]) option[A] {
        if x.isNone() || y.isNone() {
            return none[A]()
        }
        return some(op(x.value, y.value))
    }
}
// <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

// `label` holds the data stored at an inner tree node.  `aggregation` is an
// option. It is `some(v)` for nodes for which the aggregation has already
// compute from the index `from` to the index `to`.  Otherwise, it is `none`,
// i.e., the aggregation has not yet been computed from the index `from` to the
// index `to`.  Indices refer to the positions of the elements in the data
// stream.
type label[A any] struct {
    from        int       // left index
    to          int       // right index
    aggregation option[A] // aggregated value [from..to]; maybe none
}

// `tree` represents a binary tree that is either a leaf or an inner node.
// Inner nodes carry data and leaves carry no data.
type tree[A any] struct {
    data  label[A] // aggregated value
    left  *tree[A] // left child
    right *tree[A] // right child
}

// Tree constructors.

// `leaf` returns the a leaf.
func leaf[A any]() tree[A] {
    return tree[A]{}
}

// `singleton` returns the tree with the aggregated value `x` at index `i`.
func singleton[A any](i int, x A) tree[A] {
    // NOTE: We need a non-nil pointer here for the left and right child of the
    // singleton tree, i.e., a singleton is a tree with two children that are
    // leaves and carry no data.  We could use the same leaf for all singletons.
    // However, since tree nodes are parametric on the type A, we cannot do this
    // with a global variable.  nil does not work since the check `t.left ==
    // nil` (i.e., are we at a leaf?) in the selectors below.  Changing the
    // representation of singletons as leaves seems tricky.  Hence, we create a
    // leaf for each singleton, which is only xshared between its left and right
    // child.  Food for thought and room for improvement.
    l := leaf[A]()
    return tree[A]{
        data:  label[A]{from: i, to: i, aggregation: some(x)},
        left:  &l,
        right: &l,
    }
}

// `combine` merges two trees under a new inner node whose aggregation is
// `op(t1.aggregation, t2.aggregation)`.  `t1` is discharged and becomes the
// left child and `t2` becomes the right child.  If either tree is a leaf the
// other is returned as is.
func combine[A any](op Op[option[A]], t1, t2 tree[A]) tree[A] {
    switch {
    case t1.isLeaf():
        return t2
    case t2.isLeaf():
        return t1
    default:
        v := op(t1.data.aggregation, t2.data.aggregation)
        t1.discharge()
        return tree[A]{
            data:  label[A]{
                from:        t1.data.from,
                to:          t2.data.to,
                aggregation: v,
            },
            left:  &t1,
            right: &t2,
        }
    }
}

// `isLeaf` returns true if `t` is a leaf.
// (Helper function in `combine` above and the selectors below to clarify the
// check whether the tree `t` is a leaf.)
func (t tree[A]) isLeaf() bool {
    return t.left == nil
}

// `discharge` returns `t` with its aggregation cleared to none.
// (Helper function in `combine`.)
func (t *tree[A]) discharge() {
    t.data.aggregation = none[A]()
}


// Tree selectors.

func (t tree[A]) leftIndex() int {
    if t.isLeaf() {
        return -1
    }
    return t.data.from
}

func (t tree[A]) rightIndex() int {
    if t.isLeaf() {
        return -1
    }
    return t.data.to
}

func (t tree[A]) extract() A {
    if t.isLeaf() || t.data.aggregation.isNone() {
        // The extract function should never be called for leaves or when
        // the aggregation has not been computed yet.
        panic("no aggregated value at the tree's root")
    }
    return t.data.aggregation.value
}

// Auxiliary tree functions.

// `news` reads `n` new elements from `ch` starting at index `i`, combines them
// into a tree right-to-left (so that the leftmost element ends up deepest on
// the left spine), and folds the result into `acc`.  It returns the updated
// accumulator and true, or acc unchanged and false if the channel was closed
// before all elements were read.
func news[A any](op Op[option[A]], ch <-chan A, i, n int, acc tree[A]) (tree[A], bool) {
    if n <= 0 {
        // Done; all elements for the windows have have been received.
        return acc, true
    }

    // Read next element from the input channel.
    v, ok := <-ch
    if !ok {
        // Input channel closed; signal termination.
        return acc, false
    }
    // Aggregate value.
    if acc, ok = news(op, ch, i + 1, n - 1, acc); !ok {
        // Input channel closed; signal termination.
        return acc, false
    }
    return combine(op, singleton(i, v), acc), true
}

// `reusables` folds every maximal subtree of `t` whose index range lies
// entirely at or after `i` into `acc` via `combine`.  The tail-recursive
// implementation was replaced by an implementation with a for loop.
func reusables[A any](op Op[option[A]], t tree[A], i int, acc tree[A]) tree[A] {
    for {
        if i > t.rightIndex() {
            return acc
        }
        if i == t.leftIndex() {
            return combine(op, t, acc)
        }
        //if t.isLeaf() {
        //    panic("reusables: unexpected leaf")
        //}
        if t1, t2 := *t.left, *t.right; i >= t2.leftIndex() {
            t = t2 // tail call: reusables(op, t2, l, acc)
        } else {
            acc = combine(op, t2, acc)
            t = t1 // tail call: reusables(op, t1, l, acc)
        }
    }
}

// `slide` advances the tree `t` by one window step.  It returns the updated
// tree by aggregating the elements in the window `w` with the operation `op`
// and true on success, or a leaf and false if the input channel was closed
// before all elements within the window `w` were received.
func slide[A any](op Op[option[A]], ch <-chan A, t tree[A], w Window) (tree[A], bool) {
    from, to := max(w.Left, 1 + t.rightIndex()), w.Right

    // Skip elements that are after the previous window and before the current
    // window.  These elements have not yet been read from the input channel.
    if ok := skip(ch, from - ( 1 + t.rightIndex())); !ok {
        // Input channel closed; signal termination.
        return leaf[A](), false
    }

    // Loop 1: Fold newly received elements directly that were not contained in
    // the previous window.
    r, ok := news(op, ch, from, max(0, to - from + 1), leaf[A]())
    if !ok {
        // Input channel closed; signal termination.
        return leaf[A](), false
    }
    // Loop 2: Fold the reusable subtrees from the previous window's tree.
    return reusables(op, t, w.Left, r), true
}

// `skip` discards the next `k` elements from the channel `ch`.  It returns
// false if the channel was closed before `k` elements were received.
// (Helper function in `slide`.)
func skip[A any](ch <-chan A, k int) bool {
    for range k {
        if _, ok := <-ch; !ok {
            return false
        }
    }
    return true
}


// `AggregateAssoc` computes the aggregations of stream elements within a
// sliding window for an associative operator.
// Arguments:
// - in:   a channel delivering x_0, x_1, x_2, ... in order (may be infinite)
//   `AggregateAssoc` reads the elements from `in` only as far as required by
//    the windows seen so far.
// - out:  a channel on which results y_0, y_1, ... are sent, where
//   y_i = x[l_i] op x[l_i+1] op ... op x[r_i].
//   Note that the caller/creator is responsible for closing the channel.
// - op:   an associative binary operator
// - next: a function returning the next window and true, or false when the
//   window sequence is exhausted; the windows must satisfy the following
//   conditions (i.e., windows always move to the right):
//   0 <= l_0 <= l_1 <= ... and 0 <= r_0 <= r_1 <= ... and l_i <= r_i
func AggregateAssoc[A any](in <-chan A, out chan<- A, op Op[A], next Next[Window]) {
    lop := lift(op)
    t := leaf[A]()

    for {
        // Get next window.
        w, ok := next()
        if !ok {
            // No windows anymore.  Stop.
            return
        }
        // Compute aggregation.
        if t, ok = slide(lop, in, t, w); !ok {
            // No more input elements.  Stop.
            // QUESTION: Should we return the elements that are in the
            // incomplete window or the partially aggregated value of them?
            return
        }
        // Send aggregated value.
        out <- t.extract()
    }
}
