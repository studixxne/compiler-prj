module Translate

open AST
open IR
open Helper

// Symbol table is a mapping from identifier to a pair of register and type.
// Register is recorded here will be containg the address of that variable.
type SymbolTable = Map<Identifier,Register * CType>

// Let's assume the following size for each data type.
let sizeof (ctyp: CType) =
  match ctyp with
  | CInt -> 4
  | CBool -> 1
  | CIntPtr -> 8
  | CBoolPtr -> 8
  | CIntArr n -> 4 * n
  | CBoolArr n -> n

// Find the register that contains pointer to variable 'vname'
let lookupVar (symtab: SymbolTable) (vname: Identifier) : Register =
  let _ = if not (Map.containsKey vname symtab) then failwith "Unbound variable"
  fst (Map.find vname symtab)

let lookupVarTyp (symtab: SymbolTable) (vname: Identifier) : CType =
  let _ = if not (Map.containsKey vname symtab) then failwith "Unbound variable"
  snd (Map.find vname symtab)

let rec transExp (symtab: SymbolTable) (e: Exp) : Register * Instr list =
  match e with
  | Null ->
    let r = createRegName ()
    (r, [Set (r, Imm 0)])

  | Num i ->
    let r = createRegName ()
    (r, [Set (r, Imm i)])

  | Boolean b ->
    let r = createRegName ()
    (r, [Set (r, Imm (if b then 1 else 0))])

  | Var vname ->
    let varReg = lookupVar symtab vname // Contains the address of 'vname'
    let r = createRegName ()
    (r, [Load (r, varReg)])

  | Deref var_name ->
    let varReg = lookupVar symtab var_name
    let r1 = createRegName ()
    let r2 = createRegName ()
    (r2, [Load (r1, varReg); Load (r2, r1)])

  | AddrOf var_name ->
    let varReg = lookupVar symtab var_name
    let r = createRegName ()
    (r, [Set (r, Reg (varReg))])

  | Arr (var_name, e) ->
    let varReg = lookupVar symtab var_name
    let (r_idx, instrs_idx) = transExp symtab e
    let r1 = createRegName ()
    let r2 = createRegName ()
    let res = createRegName ()
    let typ_size =
      match lookupVarTyp symtab var_name with
      | CIntArr _ -> sizeof CInt
      | CBoolArr _ -> sizeof CBool
      | _ -> failwith "Invalid typ"

    (res, instrs_idx @ [BinOp (r1, MulOp, Imm (typ_size), Reg r_idx); BinOp (r2, AddOp, Reg varReg, Reg r1); Load (res, r2)])

  | Neg e1 ->
    let new_r = createRegName ()
    let (r, instrs) = transExp symtab e1
    (new_r, instrs @ [UnOp (new_r, NegOp, Reg r)])

  | Add (e1, e2) ->
    newBinOpInsr symtab AddOp e1 e2

  | Sub (e1, e2) ->
    newBinOpInsr symtab SubOp e1 e2

  | Mul (e1, e2) ->
    newBinOpInsr symtab MulOp e1 e2

  | Div (e1, e2) ->
    newBinOpInsr symtab DivOp e1 e2

  | Equal (e1, e2) ->
    newBinOpInsr symtab EqOp e1 e2

  | NotEq (e1, e2) ->
    newBinOpInsr symtab NeqOp e1 e2

  | LessEq (e1, e2) ->
    newBinOpInsr symtab LeqOp e1 e2

  | LessThan (e1, e2) ->
    newBinOpInsr symtab LtOp e1 e2

  | GreaterEq (e1, e2) ->
    newBinOpInsr symtab GeqOp e1 e2

  | GreaterThan (e1, e2) ->
    newBinOpInsr symtab GtOp e1 e2

  // And와 OR 기존 프젝 2랑 다르게 수정함
  | And (e1, e2) ->
    let t1 = createRegName ()
    let t2 = createRegName ()
    let t3 = createRegName ()
    let (t1, i1) = transExp symtab e1
    let (t1, i2) = transExp symtab e2
    let L1 = createLabel ()
    (t3, i1 @ [Set (t2, Imm (0)); GotoIfNot (Reg t1, L1)] @ i2 @ [Label (L1); Set (t3, Reg t2)])

  | Or (e1, e2) ->
    let t1 = createRegName ()
    let t2 = createRegName ()
    let t3 = createRegName ()
    let (t1, i1) = transExp symtab e1
    let (t1, i2) = transExp symtab e2
    let L1 = createLabel ()
    (t3, i1 @ [Set (t2, Imm (1)); GotoIf (Reg t1, L1)] @ i2 @ [Label (L1); Set (t3, Reg t2)])

  | Not e1 ->
    let new_r = createRegName ()
    let (r, instrs) = transExp symtab e1
    (new_r, instrs @ [UnOp (new_r, NotOp, Reg r)])

and newBinOpInsr (symtab: SymbolTable) (op: BinaryOp) (e1: Exp) (e2: Exp) : Register * Instr list=
  let new_r = createRegName ()
  let (r1, i1) = transExp symtab e1
  let (r2, i2) = transExp symtab e2
  (new_r, i1 @ i2 @ [BinOp (new_r, op, Reg r1, Reg r2)])

let rec transStmt (symtab: SymbolTable) stmt : SymbolTable * Instr list =
  match stmt with
  | Declare (_, typ, vname) ->
    let r = createRegName ()
    let size = sizeof typ
    let symtab = Map.add vname (r, typ) symtab
    (symtab, [LocalAlloc (r, size)])

  | Define (_, typ, var_name, e) ->
    let r = createRegName ()
    let size = sizeof typ
    let new_symtab = Map.add var_name (r, typ) symtab
    let (expReg, expInstr) = transExp symtab e
    (new_symtab, [LocalAlloc (r, size)] @ expInstr @ [Store (Reg expReg, r)])

  | Assign (_, var_name, e) ->
    let (expReg, expInstr) = transExp symtab e
    let varReg = lookupVar symtab var_name
    (symtab, expInstr @ [Store (Reg expReg, varReg)])

  | PtrUpdate (_, var_name, e) ->
    let (expReg, expInstr) = transExp symtab e
    let varReg = lookupVar symtab var_name
    let r = createRegName ()
    (symtab, expInstr @ [Load (r, varReg)] @ [Store (Reg expReg, r)])

  | ArrUpdate (_, var_name, e1, e2) ->
    let varReg = lookupVar symtab var_name
    let (idxReg, idxInstr) = transExp symtab e1
    let (expReg, expInstr) = transExp symtab e2
    let r1 = createRegName ()
    let r2 = createRegName ()
    let typ_size =
      match lookupVarTyp symtab var_name with
      | CIntArr _ -> sizeof CInt
      | CBoolArr _ -> sizeof CBool
      | _ -> failwith "Invalid typ"
    let idxInstrList = idxInstr @ [BinOp (r1, MulOp, Imm (typ_size), Reg idxReg); BinOp (r2, AddOp, Reg varReg, Reg r1)]
    (symtab, idxInstrList @ expInstr @ [Store (Reg expReg, r2)])

  | Return (_, e) ->
    let (expReg, expInstr) = transExp symtab e
    (symtab, expInstr @ [Ret (Reg expReg)])

  | If (_, e, true_stmts, false_stmts) ->
    let (expReg, expInstr) = transExp symtab e
    let trueInstr = transStmts symtab true_stmts
    let falseInstr = transStmts symtab false_stmts
    let L1 = createLabel ()
    let L_fin = createLabel ()
    (symtab, expInstr @ [GotoIf (Reg expReg, L1)] @ falseInstr @ [Goto (L_fin); Label (L1)] @ trueInstr @ [Label (L_fin)])

  | While (line, e, true_stmts) ->
    let (expReg, expInstr) = transExp symtab e
    let trueInstr = transStmts symtab true_stmts
    let L1 = createLabel ()
    let L_fin = createLabel ()
    (symtab, [Label (L1)] @ expInstr @ [GotoIfNot (Reg expReg, L_fin)] @ trueInstr @ [Goto (L1); Label (L_fin)])

and transStmts symtab stmts: Instr list =
  match stmts with
  | [] -> []
  | headStmt :: tailStmts ->
      let symtab, instrs = transStmt symtab headStmt
      instrs @ transStmts symtab tailStmts

// This code allocates memory for each argument and records information to the
// symbol table. Note that argument can be handled similarly to local variable.
let rec transArgs accSymTab accInstrs args =
  match args with
  | [] -> accSymTab, accInstrs
  | headArg :: tailArgs ->
      // In our IR model, register 'argName' is already defined at the entry.
      let (argTyp, argName) = headArg
      let r = createRegName ()
      let size = sizeof argTyp
      // From now on, we can use 'r' as a pointer to access 'argName'.
      let accSymTab = Map.add argName (r, argTyp) accSymTab
      let accInstrs = [LocalAlloc (r, size); Store (Reg argName, r)] @ accInstrs
      transArgs accSymTab accInstrs tailArgs

let run (prog: Program) : IRCode =
  let (_, fname, args, stmts) = prog
  let argRegs = List.map snd args
  let symtab, argInstrs = transArgs Map.empty [] args
  let bodyInstrs = transStmts symtab stmts
  (fname, argRegs, argInstrs @ bodyInstrs)