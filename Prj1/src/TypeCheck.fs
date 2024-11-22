module TypeCheck

open AST

// Symbol table is a mapping from 'Identifier' (string) to 'CType'. Note that
// 'Identifier' and 'Ctype' are defined in AST.fs file.
type SymbolTable = Map<Identifier,CType>

// For semantic analysis, you will need the following type definition. Note the
// difference between 'Ctype' and 'Typ': 'Ctype' represents the type annoted in
// the C code, whereas 'Typ' represents the type obtained during type checking.
type Typ = Int | Bool | NullPtr | IntPtr | BoolPtr | Error

// Convert 'CType' into 'Typ'.
let ctypeToTyp (ctype: CType) : Typ =
  match ctype with
  | CInt -> Int
  | CBool -> Bool
  | CIntPtr -> IntPtr
  | CBoolPtr -> BoolPtr

// Check expression 'e' and return its type. If the type of expression cannot be
// decided due to some semantic error, return 'Error' as its type.
let rec checkExp (symTab: SymbolTable) (e: Exp) : Typ =
  match e with
  | Null -> NullPtr
  | Num _ -> Int
  | Boolean _ -> Bool

  // Null Type이 너무 헷갈리는데 좀 있다 체크해보자.
  | Var var_name ->
    match Map.tryFind var_name symTab with
    | Some var_ctype-> ctypeToTyp var_ctype
    | None -> Error

  // 나중에 다시 와서 체크해보자
  | Deref var_name -> // EX) *x
    match Map.tryFind var_name symTab with
    | Some var_ctype -> 
      // IntPtr에 Int가 가르키는 지 체크도 해줘야 하는데 모르겠네 ㅎㅎ..;
      match ctypeToTyp var_ctype with
      | IntPtr -> Int
      | BoolPtr -> Bool
      | _ -> Error
    | None -> Error

  // NullPtr 어캐 계산..? 생각해보자요..
  // None일때 nullPtr로 해야하나?
  | AddrOf var_name -> // EX) &x
    match Map.tryFind var_name symTab with
    | Some var_ctype ->
      match ctypeToTyp var_ctype with
      | Int -> IntPtr
      | Bool -> BoolPtr
      | _ -> Error
    // 이거 Nullptr 반환해야 하나..? 잘모르겠네;
    | None -> Error

  | Neg e1 -> 
    match (checkExp symTab e1) with
    | Int -> Int
    | _ -> Error

  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2) -> 
    let e1_type = checkExp symTab e1
    let e2_type = checkExp symTab e2
    match (e1_type, e2_type) with
    | (Int, Int) -> Int
    | _ -> Error

  | Equal (e1, e2) | NotEq (e1, e2) ->
    let e1_type = checkExp symTab e1
    let e2_type = checkExp symTab e2
    match (e1_type, e2_type) with
    | (Int, Int) -> Bool
    | (Bool, Bool) -> Bool
    | (NullPtr, NullPtr) -> Bool
    | (IntPtr, IntPtr) | (IntPtr, NullPtr) -> Bool
    | (BoolPtr, BoolPtr) | (BoolPtr, NullPtr) -> Bool
    | _ -> Error
    
  | LessEq (e1, e2) | LessThan (e1, e2) | GreaterEq (e1, e2) | GreaterThan (e1, e2) ->
    let e1_type = checkExp symTab e1
    let e2_type = checkExp symTab e2
    match (e1_type, e2_type) with
    | (Int, Int) -> Bool
    | _ -> Error

  | And (e1, e2) | Or (e1, e2) ->
    let e1_type = checkExp symTab e1
    let e2_type = checkExp symTab e2
    match (e1_type, e2_type) with
    | (Bool, Bool) -> Bool
    | _ -> Error

  | Not e ->
    match checkExp symTab e with
    | Bool -> Bool
    | _ -> Error

// Check statement 'stmt' and return a pair of (1) list of line numbers that
// contain semantic errors, and (2) symbol table updated by 'stmt'.
let rec checkStmt (symTab: SymbolTable) (retTyp: CType) (stmt: Stmt) =
  match stmt with
  | Declare (line, ctyp, x) ->
    // If you think this statement is error-free, then return [] as error line
    // list. If you think it contains an error, you may return [line] instead.
    ([], Map.add x ctyp symTab)

  | Define (line, ctyp, x, e) ->
    let new_symTab = Map.add x ctyp symTab
    match (ctyp, checkExp symTab e) with
    | (CInt, Int) -> ([], new_symTab)
    | (CBool, Bool) -> ([], new_symTab)
    | (CIntPtr, IntPtr) | (CIntPtr, NullPtr) -> ([], new_symTab)
    | (CBoolPtr, BoolPtr) | (CBoolPtr, NullPtr) -> ([], new_symTab)
    | _ -> ([line], new_symTab) 

  | Assign (line, x, e) ->
    match Map.tryFind x symTab with
    | Some i ->
      match (i, checkExp symTab e) with
      | (CInt, Int) -> ([], symTab)
      | (CBool, Bool) -> ([], symTab)
      | (CIntPtr, IntPtr) | (CIntPtr, NullPtr) -> ([], symTab)
      | (CBoolPtr, BoolPtr) | (CBoolPtr, NullPtr) -> ([], symTab)
      | _ -> ([line], symTab)
    | None -> ([line], symTab)

  | PtrUpdate (line, x, e) ->
    let eval_e = checkExp symTab e
    match (Map.tryFind x symTab) with
    | Some i ->
      match (i, eval_e) with
      | (CIntPtr, Int) | (CBoolPtr, Bool) -> ([], symTab)
      | _ -> ([line], symTab)
    | None -> ([line], symTab)

  | Return (line, e) ->
    let eval_e = checkExp symTab e
    match (retTyp, eval_e) with
    | (CInt, Int) -> ([], symTab)
    | (CBool, Bool) -> ([], symTab)
    | (CIntPtr, IntPtr) | (CIntPtr, NullPtr) -> ([], symTab)
    | (CBoolPtr, BoolPtr) | (CBoolPtr, NullPtr) -> ([], symTab)
    | _ -> ([line], symTab)

  | If (line, e, true_stmts, false_stmts) ->
    match checkExp symTab e with
    | Bool | Int | NullPtr | IntPtr | BoolPtr ->
      let true_list = checkStmts symTab retTyp true_stmts
      let false_list = checkStmts symTab retTyp false_stmts
      (true_list @ false_list, symTab)
    | _ -> ([line], symTab)

  | While (line, e, true_stmts) ->
    match checkExp symTab e with
    | Bool ->
      let true_list = checkStmts symTab retTyp true_stmts
      (true_list, symTab)
    | _ -> ([line], symTab)

// Check the statement list and return the line numbers of semantic errors. Note
// that 'checkStmt' and 'checkStmts' are mutually-recursive (they can call each
// other). This function design will make your code more concise.
and checkStmts symTab (retTyp: CType) (stmts: Stmt list): LineNo list =
  match stmts with
  | [] -> []
  | head::tail ->
    let (cur_line, new_symTab) = checkStmt symTab retTyp head
    let next_line = checkStmts new_symTab retTyp tail
    cur_line @ next_line

// Record the type of arguments to the symbol table.
let rec collectArgTypes argDecls symTab =
  match argDecls with
  | [] -> symTab
  | (ctyp, x) :: tail ->
    let new_symTab = Map.add x ctyp symTab
    collectArgTypes tail new_symTab

// Check the program and return the line numbers of semantic errors.
let run (prog: Program) : LineNo list =
  let (retTyp, _, args, stmts) = prog
  let symTab = collectArgTypes args Map.empty
  let errorLines = checkStmts symTab retTyp stmts
  // Remove duplicate entries and sort in ascending order.
  List.sort (List.distinct errorLines)
