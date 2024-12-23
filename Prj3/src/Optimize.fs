module Optimize

open IR
open CFG
open DFA
open Helper


module ConstantFolding =
  let foldConstant instr =
    match instr with
    | UnOp (r, NegOp, Imm x) -> (true, Set (r, Imm (-x))) // Neg
    | UnOp (r, NotOp, Imm x) -> (true, Set (r, Imm (if x = 0 then 1 else 0))) // Not
    | BinOp (r, AddOp, Imm x, Imm y) -> (true, Set (r, Imm (x + y))) // Add
    | BinOp (r, SubOp, Imm x, Imm y) -> (true, Set (r, Imm (x - y))) // Sub
    // You may add many more cases here.
    | BinOp (r, MulOp, Imm x, Imm y) -> (true, Set (r, Imm (x * y))) // Mul
    | BinOp (r, DivOp, Imm x, Imm y) -> (true, Set (r, Imm (x / y))) // Div
    | BinOp (r, EqOp, Imm x, Imm y) -> (true, Set (r, Imm (if x = y then 1 else 0))) // x = y
    | BinOp (r, NeqOp, Imm x, Imm y) -> (true, Set (r, Imm (if x <> y then 1 else 0))) // x != y
    | BinOp (r, LeqOp, Imm x, Imm y) -> (true, Set (r, Imm (if x >= y then 1 else 0))) // x >= y
    | BinOp (r, LtOp, Imm x, Imm y) -> (true, Set (r, Imm (if x > y then 1 else 0))) // x > y
    | BinOp (r, GeqOp, Imm x, Imm y) -> (true, Set (r, Imm (if x <= y then 1 else 0))) // x <= y
    | BinOp (r, GtOp, Imm x, Imm y) -> (true, Set (r, Imm (if x < y then 1 else 0))) // x < y
    | _ -> (false, instr) // 최적화 하지 않는 경우

  let run instrs =
    let results = List.map foldConstant instrs
    let flags, instrs = List.unzip results
    let isOptimized = List.contains true flags
    (isOptimized, instrs)

// 2. Constant Propagtion
// #t1 = N
// #t2 = #t1일때
// t2 = N으로 대체
// RD Analysis 필요!
module ConstantPropagation =
  // Write your logic to run constant propagation with RD analysis result.
  // reg와 이름이 동일하고, 해당 reg에 constant 값이 저장되어 있으면 해당 constant 값 반환
  let rec getConstant (reg: Register) (rdSet_list: list<Instr>) : Option<int> =
    match rdSet_list with
    | [] -> None
    | head :: tail ->
      match head with
      | Set (r, o) ->
        match o with
        | Imm i when r = reg -> Some i
        | _ -> getConstant reg tail

      | _ -> getConstant reg tail

  let rec propagationConstant (rd_map: list<int * Set<Instr>>) (cfg: CFG) : list<bool * Instr>  =
    match rd_map with
    | [] -> []
    | head :: tail ->
      let (n, rdSet ) = head
      let rdSet_list = Set.toList rdSet
      let instr = CFG.getInstr n cfg
      let res = 
        match instr with
        | Set (r, Reg (reg)) ->
          // reg가 대체 가능한 지 체크
          let c = getConstant reg rdSet_list
          match c with
          | Some i -> [(true, Set (r, Imm (i)))]
          | _ -> [(false, instr)]

        | UnOp (r, op, Reg (reg)) ->
          // reg가 대체 가능한 지 체크
          let c = getConstant reg rdSet_list
          match c with
          | Some i -> [(true, UnOp (r, op, Imm (i)))]
          | _ -> [(false, instr)]

        | BinOp (r, op, o_1, o_2) ->
          // reg1과 reg2가 대체 가능한 지 체크
          let c_1 = 
            match o_1 with
            | Reg (reg_1) -> getConstant reg_1 rdSet_list
            | _ -> None

          let c_2 = 
            match o_2 with
            | Reg (reg_2) -> getConstant reg_2 rdSet_list
            | _ -> None

          match (c_1, c_2) with
          | (Some a, Some b) -> [(true, BinOp(r, op, Imm (a), Imm (b)))]
          | (Some a, None) -> [(true, BinOp(r, op, Imm (a), o_2))]
          | (None, Some b) -> [(true, BinOp (r, op, o_1, Imm (b)))]
          | _ -> [(false, instr)]

        | Store (Reg (reg_1), reg) ->
          let c = getConstant reg_1 rdSet_list
          match c with
          | Some i -> [(true, Store (Imm (i), reg))]
          | _ -> [(false, instr)]

        | GotoIf (Reg (reg), L) ->
          let c = getConstant reg rdSet_list
          match c with
          | Some i -> [(true, GotoIf (Imm (i), L))]
          | _ -> [(false, instr)]

        | GotoIfNot (Reg (reg), L) ->
          let c = getConstant reg rdSet_list
          match c with
          | Some i -> [(true, GotoIfNot (Imm (i), L))]
          | _ -> [(false, instr)]

        | Ret (Reg (reg)) ->
          let c = getConstant reg rdSet_list
          match c with
          | Some i -> [(true, Ret (Imm (i)))]
          | _ -> [(false, instr)]

        | _ -> [(false, instr)]

      res @ propagationConstant tail cfg


  let run instrs =
    let cfg = CFG.make instrs
    let rdMap = RDAnalysis.run cfg
    let results = propagationConstant (Map.toList rdMap) cfg
    let flags, instrs = List.unzip results
    let isOptimized = List.contains true flags
    (isOptimized, instrs)

// 목표 (mem2Reg 와 이건 매우 중요, 시험 끝나기 전까지 계속 깔작대기)
// 내가 만든 모듈임
module MemToReg =

  let memToreg (cfg: CFG) (rd_map) =
    let instrMap, _, _ = cfg

    let mutable regValues = Map.empty<string, Operand>

    let promotion (node: int) (instr: Instr) : bool * Instr =
      match instr with
      | LocalAlloc (reg, size) ->
        if size > 8 then (false, instr)
        else 
          regValues <- Map.add reg (Imm 0) regValues
          (true, Set (reg, Imm 0))

      | Store (operand, reg) ->
        let defs = Map.find node rd_map
        let isSafe =
            defs |> Set.exists (function
            | Store (_, r) when r = reg -> true
            | Set (r, _) when r = reg -> true
            | LocalAlloc (r, _) when r = reg -> true
            | _ -> false)
        
        if isSafe then
          let newInstr = 
            match operand with
              | Reg r -> Set (reg, Reg r)
              | Imm v -> Set (reg, Imm v)

          regValues <- Map.add reg operand regValues
          (true, newInstr)
        
        else
          (false, instr)

      | Load (reg1, reg2)->
        let defs = Map.find node rd_map
        let isSafe =
          defs |> Set.exists (function
            | Store (_, r) when r = reg2 -> true
            | _ -> false)

        if isSafe then (true, Set (reg1, Reg reg2))
        else (false, instr)
      
      | _ -> (false, instr)

    instrMap
    |> Map.toList
    |> List.map (fun (node, instr) -> promotion node instr)

  let run instrs =
    let cfg = CFG.make instrs
    let rdMap = RDAnalysis.run cfg
    let results = memToreg cfg rdMap
    let flags, instrs = List.unzip results
    let isOptimized = List.contains true flags
    (isOptimized, instrs)
  

// You will have to run optimization iteratively, as shown below.
let rec optimizeLoop instrs =
  let cp, instrs = ConstantPropagation.run instrs
  let cf, instrs = ConstantFolding.run instrs
  let mr, instrs = MemToReg.run instrs // MemToReg.run instrs
  if cp || cf || mr then optimizeLoop instrs else instrs

// Optimize input IR code into faster version.
let run (ir: IRCode) : IRCode =
  let (fname, args, instrs) = ir
  (fname, args, optimizeLoop instrs)
