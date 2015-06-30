type 'a bintree = Leaf of 'a [@z0] | Node of ('a * 'a bintree * 'a bintree) [@z] [@@spec]

(*let _ = Spec.affiche ()  *)
(*
 if b then 
      if c>0 then
         (e.pcd_name.txt, [Grammar.Cons (a,makeSeqSup e.pcd_name.txt c [])]) 
      else 
        (e.pcd_name.txt, [Grammar.Cons (a,[Seq "lala"])])  
    else *)
