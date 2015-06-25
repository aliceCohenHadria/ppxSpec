type 'a bintree = Leaf of 'a [@z0] | Node of ('a * 'a bintree * 'a bintree) [@z] [@@spec]

(*let _ = Spec.affiche ()  *)
