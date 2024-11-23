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

let rec transExp (symtab: SymbolTable) (e: Exp) : Register * Instr list =
  match e with
  | Null ->
      let r = createRegName ()
      (r, [Set (r, Imm 0)])
  | Num i ->
      let r = createRegName ()
      (r, [Set (r, Imm i)])
  | Var vname ->
      let varReg = lookupVar symtab vname // Contains the address of 'vname'
      let r = createRegName ()
      (r, [Load (r, varReg)])
  | _ -> ("", []) // TODO: Fill in the remaining cases to complete the code.

let rec transStmt (symtab: SymbolTable) stmt : SymbolTable * Instr list =
  match stmt with
  | Declare (_, typ, vname) ->
      let r = createRegName ()
      let size = sizeof typ
      let symtab = Map.add vname (r, typ) symtab
      (symtab, [LocalAlloc (r, size)])
  | _ -> (symtab, []) // TODO: Fill in the remaining cases to complete the code.

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

// Translate input program into IR code.
let run (prog: Program) : IRCode =
  let (_, fname, args, stmts) = prog
  let argRegs = List.map snd args
  let symtab, argInstrs = transArgs Map.empty [] args
  let bodyInstrs = transStmts symtab stmts
  (fname, argRegs, argInstrs @ bodyInstrs)
