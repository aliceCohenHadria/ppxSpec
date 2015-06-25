open Ast_mapper                                                            
open Ast_helper                                                                                      
open Asttypes                                                                                        
open Parsetree                                                                                       
open Longident                                                                                       
open Ast_mapper                                                                                     
open Arbolib 

let getNb payload =
 match payload with
  PStr [h] ->  begin 
   match h.pstr_desc with
    Pstr_eval (e, a) -> begin  match e.pexp_desc with 
                         Pexp_constant (Const_int i) -> print_int i; i 
                         |_ -> raise Not_found end 
    |_ -> raise Not_found
    end 
  |_ -> raise Not_found

let f aa = 
 let seq  = ref false and nbSeq = ref 0 and taille = ref (-1) in
 let rec makeTaille a = 
   print_string "MakeTaille\n";
   match a with
    [] -> ()
    |(a,b)::q -> if String.contains (a.txt) 'z' then 
               if String.length (a.txt) > 1 then
               begin
                taille :=int_of_char (String.get (a.txt) 1) - 48;
                makeTaille q;
               end 
               else 
                begin taille := 1; 
                end
            else
            begin
             if (a.txt)="seq" then 
             begin 
              seq := true; 
              try 
               nbSeq := getNb b 
              with Not_found -> ()
             end;
              makeTaille q
            end
 in
  makeTaille aa

let makeRuleCons e = 
 let args = e.pcd_args in
  List.iter (fun e -> f  e.ptyp_attributes) args 

let makeRule kind = 
 match kind with
  |Ptype_variant l -> List.iter (makeRuleCons) l  
  | _ -> raise Not_found



(*
let f kind = 
match kind with
Ptype_variant l -> List.iter (fun e -> let s =  e.pcd_name.txt in Printf.printf "ici : %s\n" s) l
|_ -> raise Not_found
*)

let makeModuleType name loc kind=
f kind;  
let ex = Exp.constant ~loc (Const_string (name,None)) in
[%stri module Spec = struct
let affiche () = Printf.printf "%s\n" [%e ex] 
end]


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
                       makeRule h.ptype_kind; type_desc 
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
