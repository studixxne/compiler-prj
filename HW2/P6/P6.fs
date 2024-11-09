module P6

/// From list 'l', find the element that appears most frequently in the list,
/// and return how many times it appears. If the input list is empty, return 0.

let rec countElement (l: list<'a>) (m: Map<'a, int>) (max: int) : int =
  match l with
  | [] -> max
  | head :: tail ->
    match Map.tryFind head m with
    | None -> if max = 0 then countElement tail (Map.add head 1 m) 1 else countElement tail (Map.add head 1 m) max
    | Some i -> if max < i + 1 then countElement tail (Map.add head (i + 1) m) (i + 1) else countElement tail (Map.add head (i + 1) m) max

let rec countMostFrequent (l: List<'a>) : int =
  countElement l Map.empty 0

