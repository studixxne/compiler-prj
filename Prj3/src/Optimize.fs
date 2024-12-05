module Optimize

open IR
open CFG
open DFA


module ConstantFolding =
  let foldConstant instr =
    match instr with
    | UnOp (r, NegOp, Imm x) -> (true, Set (r, Imm (-x)))
    | UnOp (r, NotOp, Imm x) -> (true, Set (r, Imm (if x = 0 then 1 else 0)))
    | BinOp (r, AddOp, Imm x, Imm y) -> (true, Set (r, Imm (x + y)))
    | BinOp (r, SubOp, Imm x, Imm y) -> (true, Set (r, Imm (x - y)))
    // You may add many more cases here.
    | _ -> (false, instr)

  let run instrs =
    let results = List.map foldConstant instrs
    let flags, instrs = List.unzip results
    let isOptimized = List.contains true flags
    (isOptimized, instrs)


module ConstantPropagation =
  // Write your logic to run constant propagation with RD analysis result.
  let run instrs =
    let cfg = CFG.make instrs
    let rdMap = RDAnalysis.run cfg
    let isOptimized = false
    (isOptimized, instrs)


// You will have to run optimization iteratively, as shown below.
let rec optimizeLoop instrs =
  let cp, instrs = ConstantPropagation.run instrs
  let cf, instrs = ConstantFolding.run instrs
  if cp || cf then optimizeLoop instrs else instrs

// Optimize input IR code into faster version.
let run (ir: IRCode) : IRCode =
  let (fname, args, instrs) = ir
  (fname, args, optimizeLoop instrs)
