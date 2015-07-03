open Ast_mapper                                                            
open Ast_helper                                                                                      
open Asttypes                                                                                        
open Parsetree                                                                                       
open Longident                                                                                       
open Ast_mapper                                                                                     
open Arbolib 

let rec makeSeqSup name c l = 
 match c with
  |0 -> ( Grammar.Seq name)::l
  |n -> makeSeqSup name (c-1) ((Grammar.Elem name)::l)

let makeNameList l hash= 
 let fst = List.hd l in
  match fst.ptyp_desc with
   |Ptyp_constr (a,b) -> let n = a.txt in
                         begin match n with
                               |Lident s -> 
                               begin
                                try 
                                 Hashtbl.find hash s
                                with Not_found -> s
                               end
                               |_ -> failwith "makeNameList failed"
                         end 
   |_ -> failwith "makeNameList failed"

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
   
let rec handle_tuple tu bo c hash =
 match tu with
  [] -> []
  |h::q -> begin match h.ptyp_desc with 
                  |Ptyp_constr (a,b) ->  let longident = a.txt in
                  begin match longident with
                        |Lident name ->
                           if name="list" then
                            if c>0 then
                             makeSeqSup (makeNameList b hash) c []
                            else 
                             [Grammar.Seq (makeNameList b hash )] 
                        else
                          (Grammar.Elem name)::(handle_tuple q bo c hash) 
                        | _ -> failwith "handle_tuple failed"
                  end 
                  |_ -> handle_tuple q bo c hash 
           end



let rec aux name (a,b,c) arg l hash=
 let taille = ref a in
 let grammar_element = 
    match arg.ptyp_desc with
     Ptyp_var n -> [] 
     |Ptyp_tuple tu ->  handle_tuple tu b c hash
     |Ptyp_constr ({txt = Lident s;_}, core_type_l) when s ="list" -> [] 
     |Ptyp_constr (loc,li) -> let (li,t) = replaceConstr (loc,li) (a,b,c) name l hash in 
                                taille := t; li
     |_ -> failwith "makeRule failed"
     in    
     (name,[Grammar.Cons (!taille,grammar_element)])

and handleConstr l name hash manifest =
 match manifest with 
 |Some c_t -> let cple = f c_t.ptyp_attributes c_t.ptyp_desc in 
               aux name cple c_t l hash                  
 |_ -> failwith "handleConstr failed" 

and  replaceConstr (loc, _) elt  name l hash = 
 let nameConstr = 
  match loc.txt with
   |Lident s -> s
   |_ -> failwith "replaceConstr failed"
 in
  let constr = List.find (fun e-> e.ptype_name.txt=nameConstr) l
   in
    Hashtbl.add hash nameConstr name; 
    let taille = ref 0 in
     let elem_list = 
    match constr.ptype_manifest with 
     |Some c_t -> let (a,b,c) as cple = f c_t.ptyp_attributes c_t.ptyp_desc in
                    taille := a;
                    begin
                    match c_t.ptyp_desc with
                    Ptyp_var n -> [] 
                    |Ptyp_tuple tu ->  handle_tuple tu b c hash 
                    |Ptyp_constr ({txt = Lident s;_}, core_type_l) when s ="list" ->  
                        let nameL = makeNameList core_type_l hash in
                        if b then 
                         if c>0 then
                          makeSeqSup nameL c []
                         else 
                           [Grammar.Seq nameL] 
                        else
                          []
                    |_ -> failwith "makeRule failed"
                    end
     |_ -> failwith "replaceConstr failed" 
     in (elem_list,!taille)

let makeRule e l hash= 
 let args = e.pcd_args in
  let li = List.map (fun e -> f  e.ptyp_attributes e.ptyp_desc) args in
   let (a,b,c) as elt = List.hd li and arg = List.hd args in
     aux e.pcd_name.txt elt arg l hash

let makeGrammar l kind name manifest hash = 
 match kind with
  |Ptype_variant lv -> let g = (name, List.map (fun e-> Grammar.Call e.pcd_name.txt) lv)::
                             (List.map (fun e -> makeRule e l hash) lv ) in 
                        let s = Grammar.string_of_grammar g in
                         Printf.printf "%s \n" s;g
  |Ptype_abstract -> begin try 
                        ignore(Hashtbl.find hash name); []
                      with Not_found -> [handleConstr l name hash manifest] end 
  | _ -> failwith "makeGrammar failed"


let grammarToAst l name= 
 let grammar_name  = Exp.constant  (Const_string ("grammar"^name,None)) in 
 match l with
  |[] -> []
  |h::q -> []




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
  structure = fun mapper structure ->
  let type_desc =  (List.hd structure) in  
   match type_desc with                                                                                  
    | {  pstr_desc =
      Pstr_type l ; pstr_loc} -> 
      begin
      let rec findSpec li = 
       match li with 
       |[] -> structure
       |h::q -> if List.exists (fun (e,f) -> e.txt="spec") (h.ptype_attributes) then 
                 begin
                  let hash = Hashtbl.create (List.length l) in 
                  let la = List.map (fun e -> makeGrammar l e.ptype_kind e.ptype_name.txt e.ptype_manifest hash) l
                   in
                   structure@(grammarToAst la "bintree") 
                   (*  makeGrammar (List.hd l).ptype_kind (List.hd l).ptype_name.txt (List.hd l).ptype_manifest; type_desc*) 
                 end
                else 
                 findSpec q
       in
        findSpec l 
      end
          (* Delegate to the default mapper. *)                                                      
    | x -> [default_mapper.structure_item mapper x];                                                       
}                                                                                                    

let () = register "ppxSpec" makeSpec
