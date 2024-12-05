module DFA

open IR
open CFG


// You can represent a 'reaching definition' element with an instruction.
type RDSet = Set<Instr>


module RDAnalysis =
  // Write your logic to compute reaching definition set for each CFG node.
  let run (cfg: CFG) : Map<int,RDSet> =
    Map.empty