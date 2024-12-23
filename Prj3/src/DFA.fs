module DFA

open IR
open CFG


// You can represent a 'reaching definition' element with an instruction.
type RDSet = Set<Instr>


module RDAnalysis =
  // Write your logic to compute reaching definition set for each CFG node.
  let rd_function (rdSet : RDSet) (instr : Instr) : RDSet =
    match instr with
    | Set (reg, _) 
    | LocalAlloc (reg, _)
    | UnOp (reg, _, _)
    | BinOp (reg, _, _, _)
    | Load (reg, _)
    | Store (_, reg) ->
      let killed_set = 
        Set.filter (fun i ->
          match i with
          | Set (r, _) 
          | LocalAlloc (r, _) 
          | UnOp (r, _, _) 
          | BinOp (r, _, _, _) 
          | Store (_, r)
          | Load (r, _) -> reg <> r // reg와 r의 이름이 같으면 해당 instr를 Kill
          | _ -> true
        ) rdSet

      // 해당 Instr 추가
      Set.add instr killed_set

    // instr이 %t = ''의 형태가 아니면 그냥 반환
    | _ -> rdSet

  let fixpoint_algorithm (cfg: CFG) : Map<int, RDSet> =
    let allNode = CFG.getAllNodes cfg
    
    let init_RdSet (n: int) : Map<int, RDSet> =
      let dataList = [ for i in 0 .. n -> (i, Set.empty)]
      Map.ofList dataList

    let mutable rdOut = init_RdSet (List.length allNode - 1)
    let mutable rdIn = init_RdSet (List.length allNode - 1)
    let mutable changes = true

    let union_RDSet (pred_list : int list) : RDSet =
      pred_list
      |> List.choose (fun n -> Map.tryFind n rdOut)
      |> List.fold Set.union Set.empty

    while changes do
      changes <- false
      for n in allNode do
        // rdIn Update
        let pred_list = CFG.getPreds n cfg
        rdIn <- Map.add n (union_RDSet pred_list) rdIn
        
        // 변화가 있는 지 체크 후, rdOut Update
        let inst = CFG.getInstr n cfg
        let new_rdOut = rd_function (Map.find n rdIn) inst
        if new_rdOut <> (Map.find n rdOut) then changes <- true else ()
        rdOut <- Map.add n new_rdOut rdOut

    rdOut

  let run (cfg: CFG) : Map<int,RDSet> =
    fixpoint_algorithm cfg