open Ast_mapper                                                            
open Ast_helper                                                                                      
open Asttypes                                                                                        
open Parsetree                                                                                       
open Longident                                                                                       
open Ast_mapper                                                                                      

let f kind = 
match kind with
Ptype_variant l -> List.iter (fun e -> let s =  e.pcd_name.txt in Printf.printf "ici : %s\n" s) l
|_ -> raise Not_found


let makeModuleType name loc kind=
f kind;  
let ex = Exp.constant ~loc (Const_string (name,None)) in
[%stri module Spec = struct
let affiche () = Printf.printf "%s\n" [%e ex] 
end]


let rec makeModule l = 
 match l with
  [] -> ()
  |h::q -> Printf.printf "NomType : %s\n" h.ptype_name.txt; makeModule q

let makeSpec argv =                                                                           
{ default_mapper with                                                                                
  structure_item= fun mapper type_desc ->                                                                          
    match type_desc with                                                                                  
    | {  pstr_desc =
      Pstr_type l ; pstr_loc} -> 
      begin
      let rec findSpec li = 
       match li with 
       |[] -> type_desc
       |h::q -> if List.exists (fun (e,f) -> e.txt="spec") (h.ptype_attributes) then 
                 begin Printf.printf "ici\n"; 
                       makeModule l; type_desc 
                 end
                else 
                 findSpec q
       in
        findSpec l 
      end
          (* Delegate to the default mapper. *)                                                      
          | x -> default_mapper.structure_item mapper x;                                                       
}                                                                                                    

let () = register "ppxSpec" makeSpec
