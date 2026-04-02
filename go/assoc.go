package sliding


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

// `liftOp` wraps the operator `op` so it works on option values: the result is
// some(op(x, y)) when both inputs are some, and none otherwise.
func liftOp[A any](op Op[A]) Op[option[A]] {
    return func(x, y option[A]) option[A] {
        if x.isNone() || y.isNone() {
            return none[A]()
        }
        return some(op(x.value, y.value))
    }
}


// `label` holds the data stored at an interior tree node.  `aggregation` is an
// option: some(v) for live nodes, none for discharged nodes whose aggregate has
// been consumed and is no longer needed.
type label[A any] struct {
    left        int
    right       int
    aggregation option[A]
}

// `tree` represents a binary tree that is either a leaf or an interior node.
// The ok field of the embedded option[label[A]] plays the role of the
// leaf/non-leaf flag: isNone means this is a leaf.
type tree[A any] struct {
    data  option[label[A]]
    left  *tree[A]
    right *tree[A]
}


// Tree constructors.

func leaf[A any]() tree[A] {
    return tree[A]{}
}

func singleton[A any](i int, x A) tree[A] {
    return tree[A]{
        data: some(label[A]{left: i, right: i, aggregation: some(x)}),
    }
}

// `combine` merges two trees under a new interior node whose aggregation is
// op(t1.aggregation, t2.aggregation).  If either tree is a leaf the other is
// returned as-is.  t1 is discharged and becomes the left child; t2 becomes
// the right child.
func combine[A any](op Op[option[A]], t1, t2 tree[A]) tree[A] {
    switch {
    case t1.data.isNone():
        return t2
    case t2.data.isNone():
        return t1
    default:
        d := some(label[A]{
            left:        t1.data.value.left,
            right:       t2.data.value.right,
            aggregation: op(t1.data.value.aggregation, t2.data.value.aggregation),
        })
        t1.discharge()
        return tree[A]{
            data:  d,
            left:  &t1,
            right: &t2,
        }
    }
}

// `discharge` returns a copy of `t` with its aggregation cleared to none.
// The subtree structure (left, right) and index bounds are preserved.
func (t *tree[A]) discharge() {
    t.data.value.aggregation = none[A]()
}


// Tree selectors.

func (t tree[A]) leftIndex() int {
    if t.data.isNone() {
        return -1
    }
    return t.data.value.left
}

func (t tree[A]) rightIndex() int {
    if t.data.isNone() {
        return -1
    }
    return t.data.value.right
}

func (t tree[A]) extract() A {
    if t.data.isNone() || t.data.value.aggregation.isNone() {
        panic("no value at tree's root")
    }
    return t.data.value.aggregation.value
}

// Auxiliary tree functions.

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

func slide[A any](op Op[option[A]],	b *buf[A], t tree[A], w Window) tree[A] {
    m := max(w.Left, 1 + t.rightIndex())

    b.skip(m)
    b.fill(w.Right)

    n := w.Right - m + 1
    if m > w.Right {
        n = 0
    }

    r := leaf[A]()
    // Loop 1: walk new elements right-to-left, building singletons and folding
    // them into `r`.
    for i := n - 1; i >= 0; i-- {
        r = combine(op, singleton(m + i, b.elems[i]), r)
    }
    // Loop 2: fold reusable subtrees from the previous tree via `reusables`.
    r = reusables(op, t, w.Left, r)

    b.consume(n)
    return r
}

// `AggregateAssoc` computes the aggregation of each window using `op`, which is
// assumed to be an associative operator.
// Arguments:
//   - in:   a channel delivering x_0, x_1, x_2, ... in order (may be infinite)
//   - out:  a channel on which results y_0, y_1, ... are sent, where
//     y_i = x[l_i] op x[l_i+1] op ... op x[r_i];
//     the caller is responsible for closing the channel.
//   - op:   an associative binary operator
//   - next: a function returning the next window and true, or (zero, false)
//     when the window sequence is exhausted; windows must satisfy
//     0 ≤ l_0 ≤ l_1 ≤ … and 0 ≤ r_0 ≤ r_1 ≤ ... and l_i ≤ r_i
// `AggregateAssoc` reads from `in` only as far as required by the windows seen
// so far.
func AggregateAssoc[A any](in <-chan A, out chan<- A, op Op[A], next Next[Window]) {
    lop := liftOp(op)
    b := newBuf[A](in)
    t := leaf[A]()

    for {
        // Get next window.
        w, ok := next()
        if !ok {
            // No windows anymore.
            return
        }
        // Compute aggregation.
        t = slide(lop, b, t, w)
        // Send aggregated value.
        out <- t.extract()
    }
}


// `buf` is a FIFO buffer of elements that have been read from the input channel
// but not yet consumed, i.e., they have not yet aggregated within a window.
type buf[A any] struct {
    elems []A     // unconsumed elements; elems[0] has absolute index next
    next  int     // absolute index of elems[0]
    ch    <-chan A
}

func newBuf[A any](ch <-chan A) *buf[A] {
    return &buf[A]{ch: ch}
}

// `skip` discards all elements with absolute index less than `n`, reading and
// dropping from the channel as needed for elements not yet buffered.
func (b *buf[A]) skip(n int) {
    drop := n - b.next
    if drop <= 0 {
        return
    }

    if k := len(b.elems); drop <= k {
        b.next += drop
        b.elems = b.elems[drop:]
    } else {
        // Exhaust the buffer first, then drain the channel for the remainder.
        b.next += k;
        b.elems = b.elems[:0]
        for b.next < n {
            if _, ok := <-b.ch; !ok {
                panic("input channel closed before receiving all required elements")
            }
            b.next++
        }
    }
}

// `fill` reads from the channel until the buffer holds all elements with
// absolute indices up to `n` inclusive.
func (b *buf[A]) fill(n int) {
    for b.next + len(b.elems) <= n {
        v, ok := <-b.ch
        if !ok {
            panic("input channel closed before receiving all required elements")
        }
        b.elems = append(b.elems, v)
    }
}

// `consume` discards the first `n` buffered elements after they have been
// incorporated into singleton trees.
func (b *buf[A]) consume(n int) {
    b.next += n
    b.elems = b.elems[n:]
}


////////////////////////////////////////////////////////////////////////////////////////////////////////
// Old code. A few improvements have been made above.

/*
type label[A any] struct {
    left  int // left index
    right int // right index
    value *A  // nil means no value
}

type tree[A any] struct {
    data  *label[A] // nil means Leaf
    left  *tree[A]  // left child
    right *tree[A]  // right child
}


// Trees constructors.

func leaf[A any]() *tree[A] {
    return nil
}

func singleton[A any](i int, x A) *tree[A] {
    v := x
    return &tree[A]{
        data:  &label[A]{
            left:  i,
            right: i,
            value: &v,
        },
        left:  leaf[A](),
        right: leaf[A](),
    }
}

func combine[A any](op func(*A, *A) *A, t1, t2 *tree[A]) *tree[A] {
    switch {
    case t2.isLeaf():
        return t1
    case t1.isLeaf():
        return t2
    default:
        return &tree[A]{
            data:  &label[A]{
                left:  t1.leftIndex(),
                right: t2.rightIndex(),
                value: op(t1.value(), t2.value()),
            },
            left:  t1.discharge(),
            right: t2,
        }
    }
}

// Tree selectors.

func (t *tree[A]) isLeaf() bool {
    return t == nil
}

func (t *tree[A]) leftIndex() int {
    if t.isLeaf() {
        return -1
    }
    return t.data.left
}

func (t *tree[A]) rightIndex() int {
    if t.isLeaf() {
        return -1
    }
    return t.data.right
}

func (t *tree[A]) value() *A {
    if t.isLeaf() {
        return nil
    }
    return t.data.value
}

func (t *tree[A]) extract() A {
    if v := t.value(); v != nil {
        return *v
    }
    panic("no value at tree's root")
}

func (t *tree[A]) children() (*tree[A], *tree[A]) {
    if !t.isLeaf() {
        return t.left, t.right
    }
    panic("no children at leaf")
}

// Tree methods.

func (t *tree[A]) discharge() *tree[A] {
    if t.isLeaf() {
        return leaf[A]()
    }
    return &tree[A]{
        data:  &label[A]{
            left:  t.data.left,
            right: t.data.right,
            value: nil,
        },
        left:  t.left,
        right: t.right,
    }
}

func (t *tree[A]) reusables(i int, op func(*A, *A) *A, acc *tree[A]) *tree[A]  {
    if i > t.rightIndex() {
        return acc
    } else if i == t.leftIndex() {
        return combine(op, t, acc)
    } else if r, s := t.children(); i >= s.leftIndex() {
        return s.reusables(i, op, acc)
    } else {
        return r.reusables(i, op, combine(op, s, acc))
    }
}

func (t *tree[A]) slide(op func(*A, *A) *A, b *buf[A], w Window) *tree[A] {
    r := leaf[A]()
    news := b.take(max(w.Left, 1 + t.rightIndex()), w.Right)
    for i := len(news) - 1; i >= 0; i-- {
        r = combine(op, singleton(news[i].index, news[i].value), r)
    }
    return t.reusables(w.Left, op, r)
}

// `AggregateAssoc` computes the aggregation of each window using `op`, which is
// assumed to be an associative operator.
// Arguments:
//   - in:   a channel delivering x_0, x_1, x_2, ... in order (may be infinite)
//   - out:  a channel on which results y_0, y_1, ... are sent, where
//     y_i = x[l_i] op x[l_i+1] op ... op x[r_i];
//     the caller is responsible for closing the channel.
//   - op:   an associative binary operator
//   - next: a function returning the next window and true, or (zero, false)
//     when the window sequence is exhausted; windows must satisfy
//     0 ≤ l_0 ≤ l_1 ≤ … and 0 ≤ r_0 ≤ r_1 ≤ ... and l_i ≤ r_i
// `AggregateAssoc` reads from `in` only as far as required by the windows seen
// so far.
func AggregateAssoc[A any](in <-chan A, out chan<- A, op Op[A], next Next[Window]) {
    lop := liftOp(op)
    buf := newBuf[A](in)
    t := leaf[A]()

    for {
        // Get next window.
        w, ok := next()
        if !ok {
            // No windows anymore.
            return
        }
        // Compute aggregation.
        t = t.slide(lop, buf, w)
        // Send aggregated value.
        out <- t.extract()
    }
}


// `liftOp` turns an associative operator into one that works on *A (option).
func liftOp[A any](op func(A, A) A) func(*A, *A) *A {
    return func(x, y *A) *A {
        if x == nil || y == nil {
            return nil
        }
        v := op(*x, *y)
        return &v
    }
}


// `buf` is a FIFO buffer of indexed elements that have been read from the input
// channel but not yet consumed, i.e., they have not yet aggregated within a
// window.
type buf[A any] struct {
    elems []elem[A] // buffered but unconsumed elements
    next  int       // index of buf[0] (or of the next channel read if buf is empty)
    ch    <-chan A  // input channel
}

// `elem` pairs a zero-based index with a data value.
type elem[A any] struct {
    index int
    value A
}

func newBuf[A any](ch <-chan A) *buf[A] {
    return &buf[A]{ch: ch}
}

// `drop` discards the first `n` buffered elements.
func (b *buf[A]) drop(n int) {
    if n > 0 {
        if n < len(b.elems) {
            b.next += n
            b.elems = b.elems[n:]
        } else {
            b.next += len(b.elems)
            b.elems = b.elems[:0]
        }
    }
}

// `read` reads from the channel until the buffer holds all elements with
// absolute indices up to `n` inclusive.
func (b *buf[A]) read(n int) {
    for i := b.next + len(b.elems); i <= n; i++ {
        v, ok := <-b.ch
        if !ok {
            panic("input channel closed before receiving all required elements")
        }
        b.elems = append(b.elems, elem[A]{index: i, value: v})
    }
}

// `take` returns the elements with absolute indices in [`from`, `to`], reading
// from the channel as needed and dropping any elements before from that are
// still sitting in the buffer.
func (b *buf[A]) take(from, to int) []elem[A] {
    if to < from {
        return nil
    }

    // Discard buffered elements before from.
    b.drop(from - b.next)
    // If `from` is still ahead of what we have buffered (because those elements
    // were never read), drain the channel up to `from-1` without keeping them,
    // then start buffering from `from`.
    for b.next < from {
        if _, ok := <-b.ch; !ok {
            panic("input channel closed before all receiving all required elements")
        }
        b.next++
    }

    // Pull elements up to `to` from the channel.
    b.read(to)

    n := to - from + 1
    // Take elements from buffer.
    result := b.elems[:n]
    // Drop elements from buffer.
    b.elems = b.elems[n:]
    b.next += n

    return result
}
*/


