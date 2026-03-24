package sliding

// Instead of the Window type with a left and right bound, one could have two
// functions TooClose(d T) bool and TooFar(d T bool) for a window.  This is
// strictly more general.  First, it is easy to simulate the window bounds.
// Second, one could implement windows based timestamps and not on indices.
