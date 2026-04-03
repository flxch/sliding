//! Greedy sliding-window aggregation over an associative operator.
//!
//! Reference:
//!   D. Basin, F. Klaedtke, and E. Zalinescu.
//!   *Greedily Computing Associative Aggregations on Sliding Windows.*
//!   Information Processing Letters, 115(2):186–192, 2015.
//!
//! The algorithm performs the minimum number of operator applications needed
//! to answer every window query.

// ---------------------------------------------------------------------------
// Public types
// ---------------------------------------------------------------------------

/// A window `[left, right]` with both bounds **inclusive** and zero-based.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Window {
    pub left:  usize,
    pub right: usize,
}

// ---------------------------------------------------------------------------
// Internal aggregation tree
// ---------------------------------------------------------------------------

/// A binary aggregation tree.
///
/// Leaf nodes carry no data.  Interior nodes store the index range they cover
/// and an *optional* aggregate.  The aggregate is `Some(v)` for *live* nodes,
/// and `None` for *discharged* nodes whose value has already been merged into
/// their parent but whose subtrees may still be reused.
#[derive(Clone, Debug)]
enum Tree<A> {
    Leaf,
    Node(Box<Node<A>>),
}

#[derive(Clone, Debug)]
struct Node<A> {
    from:  usize,
    to:    usize,
    value: Option<A>,
    left:  Tree<A>,
    right: Tree<A>,
}

impl<A> Tree<A> {
    fn leaf() -> Self {
        Tree::Leaf
    }

    fn singleton(i: usize, x: A) -> Self {
        Tree::Node(Box::new(Node {
            from:  i,
            to:    i,
            value: Some(x),
            left:  Tree::Leaf,
            right: Tree::Leaf,
        }))
    }

    fn left_index(&self) -> Option<usize> {
        match self {
            Tree::Leaf    => None,
            Tree::Node(n) => Some(n.from),
        }
    }

    fn right_index(&self) -> Option<usize> {
        match self {
            Tree::Leaf    => None,
            Tree::Node(n) => Some(n.to),
        }
    }

    /// Retrieve the live aggregate at the root.  Panics if absent.
    fn extract(&self) -> &A {
        match self {
            Tree::Node(n) => n.value.as_ref().expect(
                "sliding_window: extract called on discharged or empty node",
            ),
            Tree::Leaf => panic!("sliding_window: extract called on leaf"),
        }
    }

    /// Clear the aggregate stored at this node (*discharge* it).
    fn discharge(&mut self) {
        if let Tree::Node(n) = self {
            n.value = None;
        }
    }

    fn is_leaf(&self) -> bool {
        matches!(self, Tree::Leaf)
    }
}

/// Merge `left` and `right` under a new interior node.
///
/// * If either argument is a `Leaf`, the other is returned unchanged.
/// * Otherwise `left` is discharged, becomes the left child of the new node,
///   and the aggregate of the new node is `op(left.value, right.value)`.
fn combine<A, Op>(op: &Op, mut left: Tree<A>, right: Tree<A>) -> Tree<A>
where
    Op: Fn(&A, &A) -> A,
{
    if left.is_leaf()  { return right; }
    if right.is_leaf() { return left;  }

    let combined = match (&left, &right) {
        (Tree::Node(l), Tree::Node(r)) => op(
            l.value.as_ref().expect("combine: left node is discharged"),
            r.value.as_ref().expect("combine: right node is discharged"),
        ),
        _ => unreachable!(),
    };

    let from = left.left_index().unwrap();
    let to   = right.right_index().unwrap();
    left.discharge();

    Tree::Node(Box::new(Node {
        from,
        to,
        value: Some(combined),
        left,
        right,
    }))
}

// ---------------------------------------------------------------------------
// Core algorithm helpers
// ---------------------------------------------------------------------------

/// Fold every maximal subtree of `t` that lies entirely within `[start, ∞)`
/// into `acc` using `combine`.  These are the *reusable* subtrees carried
/// over from the previous window.
fn reusables<A, Op>(op: &Op, t: Tree<A>, start: usize, acc: Tree<A>) -> Tree<A>
where
    Op: Fn(&A, &A) -> A,
{
    let mut t   = t;
    let mut acc = acc;

    loop {
        // `t` is entirely to the left of `start`, or is a leaf – nothing to reuse.
        let right = match t.right_index() {
            None    => return acc,
            Some(r) => r,
        };
        if start > right {
            return acc;
        }

        // The whole subtree is reusable.
        if t.left_index() == Some(start) {
            return combine(op, t, acc);
        }

        // Descend into the interior node.
        let (left_child, right_child) = match t {
            Tree::Node(n) => (n.left, n.right),
            Tree::Leaf    => unreachable!(),
        };

        // If `start` falls inside (or to the left of) the right subtree,
        // descend into the right subtree.  Otherwise pick up the entire right
        // subtree and recurse into the left.
        if right_child.left_index().map_or(false, |l| start >= l) {
            t = right_child;
        } else {
            acc = combine(op, right_child, acc);
            t   = left_child;
        }
    }
}

/// Advance the aggregation tree by one window step.
///
/// `elements` is a peekable iterator over `(absolute_index, value)` pairs
/// not yet consumed.  Only elements needed for window `w` are read.
fn slide<A, Op, I>(
    op:       &Op,
    elements: &mut std::iter::Peekable<I>,
    t:        Tree<A>,
    w:        Window,
) -> Tree<A>
where
    Op: Fn(&A, &A) -> A,
    I:  Iterator<Item = (usize, A)>,
{
    // Absolute index of the first element not yet covered by any previous window.
    let first_new = match t.right_index() {
        Some(r) => w.left.max(r + 1),
        None    => w.left,
    };

    // Skip elements that fall in the gap between the previous window's right
    // edge and `first_new` (they were never part of any window).
    while elements.peek().map_or(false, |(i, _)| *i < first_new) {
        elements.next();
    }

    // Collect new singletons in the range [first_new, w.right].
    // We push them into a Vec and fold right-to-left so that the leftmost
    // element ends up deepest on the left spine – matching the reference
    // implementations.
    let mut new_singletons: Vec<Tree<A>> = Vec::new();
    while elements.peek().map_or(false, |(i, _)| *i <= w.right) {
        let (i, v) = elements.next().unwrap();
        new_singletons.push(Tree::singleton(i, v));
    }

    // Fold new singletons right-to-left to build the "new" right portion.
    let mut acc = Tree::leaf();
    for node in new_singletons.into_iter().rev() {
        acc = combine(op, node, acc);
    }

    // Fold the reusable subtrees from the previous window's tree on the left.
    reusables(op, t, w.left, acc)
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// An iterator that yields one aggregated value per window.
///
/// Created by [`sliding_window`].
pub struct SlidingWindowIter<A, Op, E, W>
where
    E: Iterator<Item = A>,
{
    op:       Op,
    elements: std::iter::Peekable<std::iter::Enumerate<E>>,
    windows:  W,
    tree:     Tree<A>,
}

impl<A, Op, E, W> Iterator for SlidingWindowIter<A, Op, E, W>
where
    Op: Fn(&A, &A) -> A,
    A:  Clone,
    E:  Iterator<Item = A>,
    W:  Iterator<Item = Window>,
{
    type Item = A;

    fn next(&mut self) -> Option<Self::Item> {
        let w        = self.windows.next()?;
        let old_tree = std::mem::replace(&mut self.tree, Tree::leaf());
        self.tree    = slide(&self.op, &mut self.elements, old_tree, w);
        Some(self.tree.extract().clone())
    }
}

/// Compute the aggregate of each window for an associative operator.
///
/// # Arguments
///
/// * `op`       – An associative binary operator `(&A, &A) → A`.
/// * `elements` – An iterator over `x_0, x_1, …` (may be infinite or lazy).
///                Elements are consumed *on demand*: only those needed to
///                answer the windows seen so far are ever read.
/// * `windows`  – An iterator over [`Window`] values.  The sequence must
///                satisfy `l₀ ≤ l₁ ≤ …`, `r₀ ≤ r₁ ≤ …`, and `lᵢ ≤ rᵢ`
///                for all `i`, with all indices zero-based.
///
/// # Returns
///
/// A lazy iterator that yields
/// `yᵢ = x[lᵢ] op x[lᵢ+1] op … op x[rᵢ]`
/// for each window.
///
/// # Panics
///
/// Panics if the element stream is exhausted before all window queries can be
/// satisfied.
///
/// # Example
///
/// ```
/// use sliding_window::{sliding_window, Window};
///
/// let xs      = vec![1, 2, 3, 4, 5];
/// let windows = vec![
///     Window { left: 0, right: 2 },  // 1+2+3 = 6
///     Window { left: 1, right: 3 },  // 2+3+4 = 9
///     Window { left: 2, right: 4 },  // 3+4+5 = 12
/// ];
/// let sums: Vec<i32> = sliding_window(|a, b| a + b, xs, windows).collect();
/// assert_eq!(sums, [6, 9, 12]);
/// ```
pub fn sliding_window<A, Op, E, W>(
    op:       Op,
    elements: E,
    windows:  W,
) -> SlidingWindowIter<A, Op, E::IntoIter, W::IntoIter>
where
    Op: Fn(&A, &A) -> A,
    A:  Clone,
    E:  IntoIterator<Item = A>,
    W:  IntoIterator<Item = Window>,
{
    SlidingWindowIter {
        op,
        elements: elements.into_iter().enumerate().peekable(),
        windows:  windows.into_iter(),
        tree:     Tree::leaf(),
    }
}
