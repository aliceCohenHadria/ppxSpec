type 'a bintree = Leaf of 'a [@u] | Node of ('a * 'a bintree * 'a bintree) [@z] [@@spec]

let _ = Spec.affiche ()  
