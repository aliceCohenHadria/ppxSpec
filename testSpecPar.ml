open Arbolib 
open Grammar

type 'a proc = Par of 'a parproc             
              |Sum of 'a sumproc 
and 'a parproc = ('a * 'a proc list [@seq])[@z]
and 'a sumproc = 'a parproc list [@z][@seq 2]
[@@spec]

let _ = let s = string_of_grammar Tools.grammar_proc in Printf.printf "%s\n" s


