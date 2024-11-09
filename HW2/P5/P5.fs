module P5

/// For int list 'l' that contains decimal digits (0~9), return the integer that
/// is represented by this list. For example, "digitsToInt [1; 3; 2]" must
/// return 132 as a result. When the input list is empty, just return 0.

let rec sumHelper (l: int list) (acc: int) : int =
  match l with
  | [] -> acc
  | head :: tail -> sumHelper tail (acc * 10 + head)

let rec digitsToInt (l: int list) : int =
  sumHelper l 0
