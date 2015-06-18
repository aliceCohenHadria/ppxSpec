(** coucou *)

open Ast_mapper                                                            
open Ast_helper                                                                                      
open Asttypes                                                                                        
open Parsetree                                                                                       
open Longident                                                                                       
open Ast_mapper                                                                                      

(*
let makeModule name loc = 
 let expr = {pmod_desc = Pmod_structure [{pstr_desc = Pstr_value (Nonrecursive, {pvb_pat = {txt="affiche";loc=loc}; pvb_expr = Exp.mk (Exp.let_ Nonrecursive [] }) ;pstr_loc = loc}]; pmod_loc=loc;pmod_attribute = []} in 
 Pstr_module {pmb_name = {txt="Spec"; loc = loc} ; pmb_expr = expre ;pmb_attributes=[] pmb_loc = loc}
*)

let makeModule name loc= 
let ex = Exp.constant ~loc (Const_string (name,None)) in
 [%stri module Spec = struct
    let affiche () = Printf.printf "%s\n" [%e ex] 
 end]

let makeSpec argv =                                                                           
{ default_mapper with                                                                                
  structure_item= fun mapper type_dec ->                                                                          
    match type_dec with                                                                                  
    | {  pstr_desc =
              Pstr_type [{ptype_name; ptype_params; ptype_cstrs; ptype_kind; 
              ptype_private;ptype_attributes=[({txt="spec";loc},p)];ptype_loc}]; pstr_loc} -> makeModule ptype_name.txt loc 
        (* Delegate to the default mapper. *)                                                      
        | x -> default_mapper.structure_item mapper x;                                                       
}                                                                                                    

let () = register "ppxSpec" makeSpec
