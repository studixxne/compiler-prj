module Translate

open AST
open IR
open Helper


// Copy your "Translate.fs" file from Phase #2 (and fix it if needed).


// Translate input program into IR code.
let run (prog: Program) : IRCode =
  let (_, fname, args, stmts) = prog
  let argRegs = List.map snd args
  (fname, argRegs, [])
