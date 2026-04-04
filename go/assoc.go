package sliding


// TODO: Use extra package `container/maybe` with the Option[A] type.
// >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
// `option[A]` represents an optional value: ok == true means value holds a
// valid A; ok == false means no value is present.
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
// some(op(x, y)) when both inputs are some, and none otherwise.
func lift[A any](op Op[A]) Op[option[A]] {
    return func(x, y option[A]) option[A] {
        if x.isNone() || y.isNone() {
            return none[A]()
        }
        return some(op(x.value, y.value))
    }
}
// <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

// `label` holds the data stored at an interior tree node.  `aggregation` is an
// option: some(v) for live nodes, none for discharged nodes whose aggregate has
// been consumed and is no longer needed.
type label[A any] struct {
    from        int       // left index
    to          int       // right index
    aggregation option[A] // aggregated value; maybe none
}

// `tree` represents a binary tree that is either a leaf or an interior node.
// The ok field of the embedded option[label[A]] plays the role of the
// leaf/non-leaf flag: isNone means this is a leaf.
type tree[A any] struct {
    data  label[A] // aggregated value from to; maybe none
    left  *tree[A] // left child
    right *tree[A] // right child
}

// Tree constructors.

// `leaf` returns the empty aggregation.
func leaf[A any]() tree[A] {
    return tree[A]{}
}

// `singelton` returns the aggregated value `x` at index `i`.
func singleton[A any](i int, x A) tree[A] {
    l := leaf[A]()
    return tree[A]{
        data:  label[A]{from: i, to: i, aggregation: some(x)},
        left:  &l,
        right: &l,
    }
}

// `combine` merges two trees under a new interior node whose aggregation is
// `op(t1.aggregation, t2.aggregation)`.  `t1` is discharged and becomes the
// left child and `t2` becomes the right child.  If either tree is a leaf the
// other is returned as is.
func combine[A any](op Op[option[A]], t1, t2 tree[A]) tree[A] {
    switch {
    case t1.left == nil:
        return t2
    case t2.left == nil:
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

// `discharge` returns `t` with its aggregation cleared to none.
// (Helper function in `combine`.)
func (t *tree[A]) discharge() {
    t.data.aggregation = none[A]()
}

// Tree selectors.

func (t tree[A]) leftIndex() int {
    if t.left == nil {
        return -1
    }
    return t.data.from
}

func (t tree[A]) rightIndex() int {
    if t.left == nil {
        return -1
    }
    return t.data.to
}

func (t tree[A]) extract() A {
    if t.left == nil || t.data.aggregation.isNone() {
        panic("no aggregated value at tree's root")
    }
    return t.data.aggregation.value
}

// Auxiliary tree functions.

// `news` reads `n` new elements from `s` starting at absolute index `i`,
// combines them into a tree right-to-left (so that the leftmost element ends up
// deepest on the left spine), and folds the result into `acc`.  It returns the
// updated accumulator and true, or acc unchanged and false if the channel was
// closed before all elements were read.
func news[A any](op Op[option[A]], ch <-chan A, i, n int, acc tree[A]) (tree[A], bool) {
    if n == 0 {
        return acc, true
    }

    // Read next element from the input channel.
    v, ok := <-ch
    if !ok {
        // Input channel closed; signal termination.
        return acc, false
    }

    if acc, ok = news(op, ch, i + 1, n - 1, acc); !ok {
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
        //if t.left == nil {
        //    panic("reusables: unexpected leaf")
        //}
        t1, t2 := *t.left, *t.right
        if i >= t2.leftIndex() {
            t = t2 // tail call: reusables(op, t2, l, acc)
        } else {
            acc = combine(op, t2, acc)
            t = t1 // tail call: reusables(op, t1, l, acc)
        }
    }
}

// `slide` advances the tree by one window step.  It returns the updated tree
// and true on success, or the zero tree and false if the input channel was
// closed before all required elements were available.
func slide[A any](op Op[option[A]], ch <-chan A, t tree[A], w Window) (tree[A], bool) {
    from := max(w.Left, 1 + t.rightIndex())
    to := w.Right

    // Skip elements that are after the previous window and before the current
    // window.  These elements have not yet been read from the input channel.
    if ok := skip(ch, from - ( 1 + t.rightIndex())); !ok {
        // Input channel closed; signal termination.
        return leaf[A](), false
    }

    // Loop 1: Fold newly received elements directly into the result first that
    // were not contained in the previous window.
    r, ok := news(op, ch, from, max(0, to - from + 1), leaf[A]())
    if !ok {
        // Input channel closed; signal termination.
        return leaf[A](), false
    }
    // Loop 2: Fold the reusable subtrees from the previous window's tree.
    return reusables(op, t, w.Left, r), true
}

// `skip` discards `n` elements with the the input channel `ch`.  It returns
// false if the channel was closed before `n` elements where received.
// (Helper function in `slide`.)
func skip[A any](ch <-chan A, n int) bool {
    for range n {
        if _, ok := <-ch; !ok {
            return false
        }
    }
    return true
}


// `AggregateAssoc` computes the aggregations of each window for an associative
// operator.
// Arguments:
// - in:   a channel delivering x_0, x_1, x_2, ... in order (may be infinite)
//   `AggregateAssoc` reads the elements from `in` only as far as required by
//    the windows seen so far.
// - out:  a channel on which results y_0, y_1, ... are sent, where
//   y_i = x[l_i] op x[l_i+1] op ... op x[r_i].
//   Note that the caller/creator is responsible for closing the channel.
// - op:   an associative binary operator
// - next: a function returning the next window and true, or (zero, false)
//   when the window sequence is exhausted; windows must satisfy
//   0 <= l_0 <= l_1 <= ... and 0 <= r_0 <= r_1 <= ... and l_i <= r_i.
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
            // incomplete window or the partially aggregated value?
            return
        }
        // Send aggregated value.
        out <- t.extract()
    }
}
