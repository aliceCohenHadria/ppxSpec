open Arbolib
open Grammar

type 'a bintree = Leaf of 'a [@z] | Node of ('a * 'a bintree * 'a bintree) [@z2] [@@spec]


let f () = 1

let _ = Tools.gen f 

