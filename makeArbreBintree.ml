open Arbolib
open Grammar
 
type 'a bintree = Leaf of 'a | Node of ('a * 'a bintree * 'a bintree)

let grammar_bintree =[("bintree",[Call "Leaf"; Call "Node"]);
                      ("Leaf",[Cons (1,[])]);
                      ("Node",[Cons (2,[Elem "bintree";Elem "bintree"])])]

let f () = 1

let rec makeArbre f tree =
 match tree with
  |Tree.Leaf (ty,id) when ty="Leaf"-> Leaf (f ())
  |Tree.Node (ty,id,li) -> Node (f (), makeArbre f !(List.hd li), makeArbre f !(List.nth li 1))


let rec printBinTree a = 
 match a with
  |Leaf a -> Printf.printf "Leaf []"
  |Node (a, b1, b2) -> Printf.printf "[Node :"; printBinTree b1; printBinTree b2; Printf.printf "]"

let _ = 
 let res = Gen.generator grammar_bintree true 123124 1 15 0.001 0.1 0.0001 0.1 100 0.8 8 0.0 "ocaml" 1 in 
  match res with 
   |None -> failwith "error"
   |Some (tree,size,state) -> let s = Tree.string_of_tree (ref tree) in Printf.printf "%s\n%!" s;
                              let a =  makeArbre f tree in printBinTree a
