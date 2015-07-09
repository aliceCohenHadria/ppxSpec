open Arbolib
open Grammar

type 'a proc = Par of 'a parproc             
              |Sum of 'a sumproc 
              and 'a parproc = ('a * 'a proc list [@seq])[@z]
              and 'a sumproc = 'a parproc list [@z][@seq 2]
              [@@spec]

let grammar_proc =
[("proc", [Call "Par"; Call "Sum"]);
("Par", [Cons (1, [Seq "proc"])]);
("Sum", [Cons (1, [Seq "Par"; Elem "Par"; Elem "Par"])])]

let rec printPar (i,l) = 
 Printf.printf "Par ["; List.iter (fun e -> printSumPar e) l
 
and printSumPar a = 
 match a with
  |Par (id,[]) -> Printf.printf "Par [\n" ;
  |Par (id,l) -> Printf.printf "Par [\n "; List.iter (fun e -> printSumPar e) l 
  |Sum l ->  Printf.printf "Sum ["; List.iter (fun e -> printPar e) l

let f () = 1

let rec makeArbre f tree =
 match tree with 
 |Tree.Leaf (ty,id) when ty="Par"-> Par (f (), []) 
 |Tree.Node (ty,id,tree_list) when ty="Par"-> Par (f (), List.map (fun e -> makeArbre f !e) tree_list) 
 |Tree.Node (ty,id,tree_list) when ty="Sum"-> Sum (makePar f tree_list)

and makePar f l = 
 List.map (fun e -> (f(), [makeArbre f !e])) l 

let _ = 
 let res = Gen.generator grammar_proc true 123124 1 15 0.001 0.1 0.0001 0.1 100 0.8 8 0.0 "ocaml" 1 in 
  match res with 
   |None -> failwith "error"
   |Some (tree,size,state) -> let s = Tree.string_of_tree (ref tree) in Printf.printf "%s\n%!" s;
                              let a =  makeArbre f tree in printSumPar a
