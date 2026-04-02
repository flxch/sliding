package sliding


// TODO: Use extra package `container/maybe` with the Option[A] type.
// -----------------------------------------------------------------------------
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
// -----------------------------------------------------------------------------

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
    data  option[label[A]] // aggregated value from to; maybe none
    left  *tree[A]         // left child
    right *tree[A]         // right child
}


// Tree constructors.

func leaf[A any]() tree[A] {
    return tree[A]{}
}

func singleton[A any](i int, x A) tree[A] {
    return tree[A]{
        data: some(label[A]{from: i, to: i, aggregation: some(x)}),
    }
}

// `combine` merges two trees under a new interior node whose aggregation is
// `op(t1.aggregation, t2.aggregation)`.  `t1` is discharged and becomes the
// left child and `t2` becomes the right child.  If either tree is a leaf the
// other is returned as is.
func combine[A any](op Op[option[A]], t1, t2 tree[A]) tree[A] {
    switch {
    case t1.data.isNone():
        return t2
    case t2.data.isNone():
        return t1
    default:
        v := op(t1.data.value.aggregation, t2.data.value.aggregation)
        t1.discharge()
        return tree[A]{
            data:  some(label[A]{
                from:        t1.data.value.from,
                to:          t2.data.value.to,
                aggregation: v,
            }),
            left:  &t1,
            right: &t2,
        }
    }
}

// `discharge` returns `t` with its aggregation cleared to none.
func (t *tree[A]) discharge() {
    t.data.value.aggregation = none[A]()
}


// Tree selectors.

func (t tree[A]) leftIndex() int {
    if t.data.isNone() {
        return -1
    }
    return t.data.value.from
}

func (t tree[A]) rightIndex() int {
    if t.data.isNone() {
        return -1
    }
    return t.data.value.to
}

func (t tree[A]) extract() A {
    if t.data.isNone() || t.data.value.aggregation.isNone() {
        panic("no aggregated value at tree's root")
    }
    return t.data.value.aggregation.value
}

// Auxiliary tree functions.

// `news` reads `n` new elements from `s` starting at absolute index `i`,
// combines them into a tree right-to-left (so that the leftmost element ends up
// deepest on the left spine), and folds the result into `acc`.  It returns the
// updated accumulator and true, or acc unchanged and false if the channel was
// closed before all elements were read.
func news[A any](op Op[option[A]], s *input[A], n, i int, acc tree[A]) (tree[A], bool) {
    if n == 0 {
        return acc, true
    }

    // Read next element from the input channel.
    v, ok := s.read()
    if !ok {
        // Input channel closed; signal termination.
        return acc, false
    }
    if acc, ok = news(op, s, n - 1, i + 1, acc); !ok {
        return acc, false
    }
    return combine(op, singleton(i, v), acc), true
}

// `reusables` folds every maximal subtree of `t` whose index range lies
// entirely at or after `i` into `acc` via `combine`.
func reusables[A any](op Op[option[A]], t tree[A], i int, acc tree[A]) tree[A] {
    for {
        if i > t.rightIndex() {
            return acc
        }
        if i == t.leftIndex() {
            return combine(op, t, acc)
        }
        //if t.data.isNone() {
        //    panic("reusables: unexpected leaf")
        //}
        if r, s := *t.left, *t.right; i >= s.leftIndex() {
            t = s // tail call: reusables(op, s, l, acc)
        } else {
            acc = combine(op, s, acc)
            t = r // tail call: reusables(op, r, l, acc)
        }
    }
}

// `slide` advances the tree by one window step.  It returns the updated tree
// and true on success, or the zero tree and false if the input channel was
// closed before all required elements were available.
func slide[A any](op Op[option[A]], s *input[A], t tree[A], w Window) (tree[A], bool) {
    i := max(w.Left, 1 + t.rightIndex())

    // Skip elements that lie after the previous windows and before the current
    // window.  These elements have not yet been read from the input channel.
    if !s.skip(i) {
        // Input channel closed; signal termination.
        return leaf[A](), false
    }

    n := w.Right - i + 1
    if i > w.Right {
        n = 0
    }

    // Loop 1: Fold newly received elements directly into the result first that
    // were not contained in the previous window.
    r, ok := news(op, s, n, i, leaf[A]())
    if !ok {
        // Input channel closed; signal termination.
        return leaf[A](), false
    }
    // Loop 2: Fold the reusable subtrees from the previous window's tree.
    return reusables(op, t, w.Left, r), true
}


// `AggregateAssoc` computes the aggregation of each window using `op`, which is
// assumed to be an associative operator.
// Arguments:
// - in:   a channel delivering x_0, x_1, x_2, ... in order (may be infinite)
// - out:  a channel on which results y_0, y_1, ... are sent, where
//   y_i = x[l_i] op x[l_i+1] op ... op x[r_i].
//   The caller is responsible for closing the channel.
// - op:   an associative binary operator
// - next: a function returning the next window and true, or (zero, false)
//   when the window sequence is exhausted; windows must satisfy
//   0 ≤ l_0 ≤ l_1 ≤ … and 0 ≤ r_0 ≤ r_1 ≤ ... and l_i ≤ r_i.
// `AggregateAssoc` reads from `in` only as far as required by the windows seen
// so far.
func AggregateAssoc[A any](in <-chan A, out chan<- A, op Op[A], next Next[Window]) {
    lop := lift(op)
    s := newInput[A](in)
    t := leaf[A]()

    for {
        // Get next window.
        w, ok := next()
        if !ok {
            // No windows anymore.  Stop.
            return
        }
        // Compute aggregation.
        if t, ok = slide(lop, s, t, w); !ok {
            // No more input elements.  Stop.
            // Q: Should we return the elements that are in the incomplete window?
            return
        }
        // Send aggregated value.
        out <- t.extract()
    }
}


// `input` tracks the position in the input channel, allowing elements to be
// skipped or read one at a time.
type input[A any] struct {
    ch   <-chan A
    next int // Absolute index of the next element to be read from the channel `ch`.
}

func newInput[A any](ch <-chan A) *input[A] {
    return &input[A]{ch: ch}
}

// `skip` discards all elements with absolute index smaller than `n` by reading
// them from the input channel and dropping them.  It returns false if the
// channel was closed before `n` was reached.
func (s *input[A]) skip(n int) bool {
    for s.next < n {
        if _, ok := <-s.ch; !ok {
            return false
        }
        s.next++
    }
    return true
}

// `read` reads the next element from the input channel, advancing the position.
// It returns the element and true on success, or the zero value and false if
// the channel was closed.
func (s *input[A]) read() (A, bool) {
    v, ok := <-s.ch
    if ok {
        s.next++
    }
    return v, ok
}


// -----------------------------------------------------------------------------
// Old code
/*
func slide[A any](op Op[option[A]],	b *buffer[A], t tree[A], w Window) (tree[A], bool) {
    m := max(w.Left, 1 + t.rightIndex())

    if !b.skip(m) {
        // Input channel closed; signal termination.
        return leaf[A](), false
    }
    if !b.fill(w.Right) {
        // Input channel closed; signal termination.
        return leaf[A](), false
    }

    n := w.Right - m + 1
    if m > w.Right {
        n = 0
    }

    r := leaf[A]()
    // Loop 1: walk new elements right-to-left, building singletons and folding
    // them into `r`.
    for i := n - 1; i >= 0; i-- {
        r = combine(op, singleton(m + i, b.get(i)), r)
    }
    b.drop(n)
    // Loop 2: fold reusable subtrees from the previous tree via `reusables`.
    r = reusables(op, t, w.Left, r)

    return r, true
}


// `buffer` is a FIFO buffer of elements that have been read from the input
// channel but not yet consumed, i.e., they have not yet aggregated within a
// window.
type buffer[A any] struct {
    elems []A      // unconsumed elements; elems[0] has absolute index next
    next  int      // absolute index of elems[0]
    ch    <-chan A
}

func newBuffer[A any](ch <-chan A) *buffer[A] {
    return &buffer[A]{ch: ch}
}

// `get` returns the `i`th buffered element.
func(b *buffer[A]) get(i int) A {
    return b.elems[i]
}

// `drop` discards the first `n` buffered elements after they have been
// incorporated into singleton trees.
func (b *buffer[A]) drop(n int) {
    b.next += n
    b.elems = b.elems[n:]
}

// `skip` discards all elements with absolute index less than `n`, reading and
// dropping from the channel as needed for elements not yet buffered.
func (b *buffer[A]) skip(n int) bool {
    d := n - b.next
    if d <= 0 {
        return true
    }

    if k := len(b.elems); d <= k {
        b.drop(d)
    } else {
        // Exhaust the buffer first, then drain the channel for the remainder.
        b.next += k
        b.elems = b.elems[:0]
        for b.next < n {
            if _, ok := <-b.ch; !ok {
                // Input channel closed.  No more elements.  Signal termination.
                return false
            }
            b.next++
        }
    }
    return true
}

// `fill` reads from the channel until the buffer holds all elements with
// absolute indices up to `n` inclusive.
func (b *buffer[A]) fill(n int) bool {
    for b.next + len(b.elems) <= n {
        v, ok := <-b.ch
        if !ok {
            // Input channel closed.  No more elements.  Signal termination.
            return false
        }
        b.elems = append(b.elems, v)
    }
    return true
}
*/



