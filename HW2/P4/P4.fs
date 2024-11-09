module P4

/// The summation operator (Σ). "sigma f a b" must return "f a + f (a + 1) +
/// ... + f b" as a result. If a > b, then return 0.
let rec sigma (f: int -> int) (a: int) (b: int): int =
  if a > b then 0 else f a + sigma f (a + 1) b
