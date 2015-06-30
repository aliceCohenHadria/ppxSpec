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
    Pstr_eval (e, a)  -> begin  match e.pexp_desc with 
                         Pexp_constant (Const_int i) -> i 
                         |_ -> failwith"getNb failed" end 
    |_ -> failwith "getNb failed"
  end 
  |_ -> failwith "getNb failed"

let f args desc = 
 let seq  = ref false and nbSeq = ref 0 and taille = ref (-1) in
 let rec makeTaille a =
   match a with
    [] when !taille <> -1-> (!taille, !seq, !nbSeq)
    |[] -> (0,!seq, !nbSeq)
    |(a,b)::q when String.contains (a.txt) 'z'  ->  
                if String.length (a.txt) > 1 then
                  taille :=int_of_char (String.get (a.txt) 1) - 48
                else 
                  taille := 1; 
                makeTaille q
    |(a,b)::q ->if (a.txt)="seq" then 
                begin 
                  seq := true; 
                  nbSeq := getNb b 
                end;
                makeTaille q
  in
   makeTaille args
   
let rec handle_tuple tu bo =
 match tu with
  [] -> []
  |h::q -> begin match h.ptyp_desc with 
                  |Ptyp_constr (a,b) ->  let longident = a.txt in
                  begin match longident with
                        |Lident name -> 
                          (Grammar.Elem name)::(handle_tuple q bo) 
                        | _ -> failwith "handle_tuple failed"
                  end 
                  |_ -> handle_tuple q bo 
           end



let rec aux name (a,b,c) arg l hash=
 let grammar_element = 
    match arg.ptyp_desc with
     Ptyp_var n -> [] 
     |Ptyp_tuple tu ->  handle_tuple tu b 
     |Ptyp_constr ({txt = Lident s;_}, core_type_l) when s ="list" -> [] 
     |Ptyp_constr (loc,li) -> replaceConstr (loc,li) (a,b,c) name l hash 
     |_ -> failwith "makeRule failed"
     in    
     (name,[Grammar.Cons (a,grammar_element)])
(*
let makeConsManifest manifest = 
 match manifest with
  |Some e -> 
  *) 
and handleConstr l name hash manifest =
 match manifest with 
 |Some c_t -> let cple = f c_t.ptyp_attributes c_t.ptyp_desc in 
               aux name cple c_t l hash                  
 |_ -> failwith "handleConstre failed" 

and  replaceConstr (loc, _) elt  name l hash = 
 let nameConstr = 
  match loc.txt with
   |Lident s -> s
   |_ -> failwith "replaceConstr failed"
 in
  let constr = List.find (fun e-> e.ptype_name.txt=nameConstr) l
   in
    Hashtbl.add hash nameConstr true; handleConstr l name hash constr.ptype_manifest 


let rec makeSeqSup name c l = 
 match c with
  |0 -> ( Grammar.Seq name)::l
  |n -> makeSeqSup name (c-1) ((Grammar.Elem name)::l)

let makeRule e l hash= 
 let args = e.pcd_args in
  let li = List.map (fun e -> f  e.ptyp_attributes e.ptyp_desc) args in
   let (a,b,c) as elt = List.hd li and arg = List.hd args in
     aux e.pcd_name.txt elt arg l hash

let makeGrammar l kind name manifest hash = 
 match kind with
  |Ptype_variant lv -> let g = (name, List.map (fun e-> Grammar.Call e.pcd_name.txt) lv)::
                             (List.map (fun e -> makeRule e l hash) lv ) in g
  |Ptype_abstract -> begin try 
                        ignore(Hashtbl.find hash name); []
                      with Not_found -> handleConstr l name hash manifest end 
  | _ -> failwith "makeGrammar failed"



(*
let f kind = 
match kind with
Ptype_variant l -> List.iter (fun e -> let s =  e.pcd_name.txt in Printf.printf "ici : %s\n" s) l
|_ -> raise Not_found
*)
(*
let makeModuleType name loc kind=
f kind;  
let ex = Exp.constant ~loc (Const_string (name,None)) in
[%stri module Spec = struct
let affiche () = Printf.printf "%s\n" [%e ex] 
end]
*)

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
                  let hash = Hashtbl.create (List.length l) in 
                  let la = List.map (fun e -> makeGrammar l e.ptype_kind e.ptype_name.txt e.ptype_manifest hash) l
                   in type_desc
                (*  makeGrammar (List.hd l).ptype_kind (List.hd l).ptype_name.txt (List.hd l).ptype_manifest; type_desc*) 
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
