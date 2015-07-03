type 'a bintree = Leaf of 'a [@z0] | Node of ('a * 'a bintree * 'a bintree) [@z] [@@spec]

let l = [1;2;3]
(*let _ = Spec.affiche ()  *)
(*
   else *)
