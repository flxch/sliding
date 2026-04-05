package sliding

// TODOs:
// * Instead of the Window type with a left and right bound, one could have two
//   functions TooClose(d T) bool and TooFar(d T) bool for a window.  This is
//   strictly more general.  First, it is easy to simulate the window bounds.
//   Second, one could implement windows based timestamps and not on indices.
// * Improve API.
// * Document code and package.
// * Provide implementation that is compatible with the pipeline package.
// * We could use a pool for the tree nodes for the AggregateAssoc function.
//   Note that Go is slow in allocating memory and many nodes might get created
//   at a high rate.  It also reduces the burden on the garbage collector.
