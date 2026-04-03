# Greedily Computing Associative Aggregations on Sliding Windows

This is a fun project.  One goal is to use some programming languages
that I rarely use but want to get to know a little better.  Another
goal is to compare the programming languages among each other.  Yet
another goal is to use LLMs to support coding, learn their strengths
and weaknesses, and learn some lessons here.  To this end, we will
reimplement the sliding window algorithm described in the paper

> D. Basin, F. Klaedtke, and E. Zalinescu.
> Greedily Computing Associative Aggregations on Sliding Windows.
> Information Processing Letters, 115(2):186-192, 2015.
> [publisher](https://www.sciencedirect.com/science/article/pii/S0020019014001859)
> [preprint](doc/preprint.pdf)
> [bibtex](doc/ipl.bib)

in several programming languages with the help of LLMs.  I think the
sliding window algorithm is a good choice for the goals.  It is not
too complex but not trivial.  Furthermore, a precise description
including a correctness proof is provided in the paper.  It is also
not a standard algorithm for which many implementations already exists
and which have been used in the training phase of the LLMs.

This project provides implementations of the sliding window algorithm
that are in the style of the corresponding programming language.
Understandable and clean code is the main target.  The respective
implementation should not heavily rely on third-party libraries.
Standard libraries, e.g., ones that are widely used are okay though.
Performance is not top priority, but we not want end up with
inefficient implementations.  Overall, we want code that uses the
corresponding programming language well and its coding styles.  The
implementation should be close to the Ocaml implementation provided in
the paper.  However, it is perfectly fine to deviate from the Ocaml
code to use features of the respective programming language.  The
algorithms core should stay the same though.  Finally, we draw some
conclusions, where we focus on our use of LLMs for code generation.

Note that no hard metrics are used here for comparing the different
implementations or analyzing the use of the LLMs.  Instead, we report
on our impression and the lessons learned while writing the code.  You
should keep in mind that this is subjective and debatable.  My
statements should also be taken with some grain of salt.
Nevertheless, I hope this project provides some insights and
intuition.  Furthermore, because the current development on LLMs with
their improvements is amazingly fast, some of the statements that you
find here might become quickly outdated.  I will not be able to
guarantee that everything is up to date.  Currently, I am using Claude
Sonnet 4.6.  I might later use other LLMs to make some comparison for
their coding support.

Your feedback such as suggestions for improvements or different
implementations, including implementations in a programming language
that is not yet listed below, are welcome.


## Background

Let us first explain the algorithmic problem that the sliding window
algorithm solves.  Afterwards, we provide some algorithmic details
before we present the implementations of the algorithm in different
programming languages.

### Problem Description

Let `⊕:D×D→D` be an associative operator over some domain `D`.
Consider a sequence `ā = (a_1, ... , a_n)` of elements in `D`, with `n
≥ 1`. A _window_ `w` in `ā` is a pair `(l,r)` with `1 ≤ l ≤ r ≤ n`.
We denote the _left margin_ of a window `w` by `left(w)` and the
_right margin_ by `right(w)` , i.e., `left(w)` is `w`’s first
component and `right(w)` its second component.  Furthermore, we write
`⊕w(ā)` for `a_left(w) ⊕ a_left(w)+1 ⊕ ... ⊕ a_right(w)`, i.e., the
aggregated value of the elements of the window `w`.

We consider the following problem in which the number of applications
of the `⊕` operator should be minimized.

- *Input:* A nonempty sequence `ā` of elements in `D` and a sequence
   `w̄ = (w_1, ..., w_k)` of windows in `ā`, with `left(w_1) ≤
   left(w_2) ≤ ... ≤ left(w_k)` and `right(w_1) ≤ right(w_2) ≤ ... ≤
   right(w_k)`.
- *Output:* The sequence `⊕w_1(ā), ⊕w_2(ā), ... , ⊕w_k(ā)`.

We further require that both the sequences `ā` and `w̄` are given
incrementally.  Their lengths are not known in advance, i.e., they are
potentially unbounded.

The objective of minimizing the applications of `⊕` is motivated in
settings where `⊕`’s computation is expensive, e.g., when taking the
union of large finite sets or when multiplying large matrices.  The
straightforward but suboptimal algorithm to the problem computes
`⊕w_i(ā)` for each window `w_i` separately.  It is easy to see that
this algorithm applies the `⊕` operator `SUM_i=1^k (right(w_i) −
left(w_i))` times. One can do better by sharing intermediate results
between overlapping windows.

### Algorithmic Details

[TODO: Provide intuition and describe core ideas: sliding window and
reusing partial aggregations of subsequences in previous processed
windows.  These partial aggregations are stored in trees that are
updated whenever moving the window to the right.]


## Implementations

### Ocaml

The first implementation of the sliding window algorithm was in
Ocaml. It is based on a subalgorithm within the monitoring tool
[MONPOLY](https://sourceforge.net/projects/monpoly/), which is written
in Ocaml.  The paper presents (more or less) the Ocaml implementation.

### Haskell

The implementation is very close to the Ocaml implementation and was
done while writing the paper as a simple exercise and out of
curiosity.  At that time, LLMs did not yet exist.  In general, I
prefer the Haskell syntax over the Ocaml syntax.  I find it cleaner
and more elegant.  This is subjective though.

Some new features like dealing with "infinite lists" as input directly
follow from the lazy evaluation of Haskell programs.  This is very
convenient and makes the Haskell code very succinct and clean.

### Go

Go is currently my major programming language.  This is the reason why
I started with implementing the sliding window algorithm in Go with
the support of LLMs.  As a starting point, I used the Ocaml and
Haskell implementations, which I also provided to Claude in the
prompt.

It took several iterations until I was happy with the code that
Claude generated for me based on my instructions.

### Python

### Rust

### C

### TypeScript

### Dafny


# References

1. D. Basin, F. Klaedtke, and E. Zălinescu.
   Greedily Computing Associative Aggregations on Sliding Windows.
   Information Processing Letters, 115(2):186-192, 2015.

2. D. Basin, M. Harvan, F. Klaedtke, and E. Zălinescu.
   MONPOLY: Monitoring Usage-Control Policies.
   In the Proceedings of the 2nd International Conference on Runtime
   Verification (RV). Lecture Notes in Computer Science, vol. 7186.
   Springer, 2011.


# TODOs

* Review implementations in Rust and Dafny.  Some changes might be
  necessary here.

* Provide implementations in C and TypeScript.

* Use LLMs to obtain an implementation in a programming language of
  your choice by describing the algorithm and its core building blocks
  in natural language only.  Do not provide an implementation in some
  other programming language.  You may want to use pseudo code
  snippets.
