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

let f args desc = 
 let seq  = ref false and nbSeq = ref 0 and taille = ref (-1) in
 let rec makeTaille a =
   match a with
    [] -> (!taille, !seq, !nbSeq)
    |(a,b)::q -> if String.contains (a.txt) 'z' then 
               begin
                if String.length (a.txt) > 1 then
                  taille :=int_of_char (String.get (a.txt) 1) - 48
                else 
                  taille := 1; 
                makeTaille q
                end
            else
            begin
             if (a.txt)="seq" then 
             begin 
              seq := true; 
              nbSeq := getNb b 
             end;
              makeTaille q
            end
 in
  makeTaille args
   
let rec handle_tuple tu =
 match tu with
  [] -> []
  |h::q -> begin match h.ptyp_desc with 
                  |Ptyp_constr (a,b) ->  let longident = a.txt in
                  begin match longident with
                        |Lident name -> 
                         Printf.printf "%s\n" name; (Grammar.Elem name)::(handle_tuple q) 
                        | _ -> raise Not_found
                  end 
                  |_ -> handle_tuple q 
           end

let makeRule e = 
Printf.printf "nom %s    " e.pcd_name.txt;
 let args = e.pcd_args in
 Printf.printf "taille : %d     " (List.length args);
  let l = List.map (fun e -> f  e.ptyp_attributes e.ptyp_desc) args in
   List.iter (fun (a,b,c) -> Printf.printf "taille : %d, seq : %d\n" a c) l;
   let (a,b,c) as elt = List.hd l and arg = List.hd args in
   let grammar_element = 
    match arg.ptyp_desc with
     Ptyp_var n -> [] 
     |Ptyp_tuple tu -> Printf.printf "ici\n"; handle_tuple tu  
     in
   (* if b then 
      if b>0 then
        makeSeqSup e elt 
      else 
        (e.ptyp_name.txt, Cons (a,[Seq '']))  *)
   (e.pcd_name.txt,[Grammar.Cons (a,grammar_element)])
   

let makeGrammar kind name = 
 match kind with
  |Ptype_variant l -> let g = (name, List.map (fun e-> Grammar.Call e.pcd_name.txt) l)::(List.map (makeRule) l ) in let s = Grammar.string_of_grammar g in Printf.printf "Grammaire : \n %s \n" s 
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
                 begin 
                  List.map (fun e -> makeGrammar e.ptype_kind e.ptype_name.txt) li; type_desc 
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
