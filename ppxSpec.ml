open Ast_mapper                                                            
open Ast_helper                                                                                      
open Asttypes                                                                                        
open Parsetree                                                                                       
open Longident                                                                                       
open Ast_mapper                                                                                     
open Arbolib 


let rec makeSeqSup (name:string) (sizeSeq:int) (l:Grammar.elem list) = 
 match sizeSeq with
  |0 -> ( Grammar.Seq name)::l
  |n -> makeSeqSup name (sizeSeq-1) ((Grammar.Elem name)::l)

let makeNameList (l:core_type list) (hash:(string,string) Hashtbl.t)= 
 let fst = List.hd l in
  match fst.ptyp_desc with
   |Ptyp_constr (loc,_) -> let n = loc.txt in
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

let getNb (payload:payload) =
 match payload with
  PStr [h] ->  
   begin 
   match h.pstr_desc with
    Pstr_eval (expression, _)  -> begin  match expression.pexp_desc with 
                                           Pexp_constant (Const_int i) -> i 
                                           |_ -> failwith"getNb failed" end 
    |_ -> failwith "getNb failed"
   end 
  |_ -> failwith "getNb failed"

let decode_attributes (args:attributes) = 
 let seq  = ref false and nbSeq = ref 0 and taille = ref (-1) in
  let rec makeTaille attr =
   match attr with
    [] when !taille <> -1-> (!taille, !seq, !nbSeq)
    |[] -> (0,!seq, !nbSeq)
    |(string_loc,payload)::q when String.contains (string_loc.txt) 'z'  ->  
                if String.length (string_loc.txt) > 1 then
                  taille :=int_of_char (String.get (string_loc.txt) 1) - 48
                else 
                  taille := 1; 
                makeTaille q
    |(string_loc,payload)::q ->
                if (string_loc.txt)="seq" then 
                begin 
                  seq := true; 
                  nbSeq := getNb payload 
                end;
                makeTaille q
  in
   makeTaille args
   
let rec handle_tuple (tu:core_type list) (isSeq:bool) (sizeSeq:int) (hash:(string,string) Hashtbl.t) =
 match tu with
  [] -> []
  |h::q -> begin match h.ptyp_desc with 
                  |Ptyp_constr (longIdent_loc,core_type_list) ->  let longident = longIdent_loc.txt in
                  begin match longident with
                        |Lident name ->
                           if name="list" then
                            if isSeq then
                             makeSeqSup (makeNameList core_type_list hash) sizeSeq []
                            else 
                             [Grammar.Seq (makeNameList core_type_list hash )] 
                        else
                          (Grammar.Elem name)::(handle_tuple q isSeq sizeSeq hash) 
                        | _ -> failwith "handle_tuple failed"
                  end 
                  |_ -> handle_tuple q isSeq sizeSeq hash 
           end



let rec handleCoreType (name:string) ((size,isSeq,sizeSeq):(int*bool*int)) 
                       (arg:core_type) (l:type_declaration list) (hash:(string,string) Hashtbl.t)=
 let taille = ref size in
  let grammar_element = 
   match arg.ptyp_desc with
    Ptyp_var n -> [] 
    |Ptyp_tuple tu ->  handle_tuple tu isSeq sizeSeq hash
    |Ptyp_constr ({txt = Lident s;_}, core_type_l) when s ="list" -> [] 
    |Ptyp_constr (loc,li) -> let (li,t) = replaceConstr (loc,li) name l hash in 
                                taille := t; li
    |_ -> failwith "makeRule failed"
    in    
     (name,[Grammar.Cons (!taille,grammar_element)])


and handleConstr (l:type_declaration list) (name:string) (hash:(string,string) Hashtbl.t) (manifest:core_type option) =
 match manifest with 
 |Some c_t -> let cple = decode_attributes c_t.ptyp_attributes in 
               handleCoreType name cple c_t l hash                  
 |_ -> failwith "handleConstr failed" 


and replaceConstr ((loc, _):(Longident.t Asttypes.loc * Parsetree.core_type list)) 
                     (name:string) (l:type_declaration list) (hash: (string,string) Hashtbl.t) = 
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
       |Some c_t -> let (size, isSeq, sizeSeq) as cple = decode_attributes c_t.ptyp_attributes  in
                     taille := size;
                     begin
                     match c_t.ptyp_desc with
                      Ptyp_var n -> [] 
                      |Ptyp_tuple tu ->  handle_tuple tu isSeq sizeSeq hash 
                      |Ptyp_constr ({txt = Lident s;_}, core_type_l) when s ="list" ->  
                          let nameL = makeNameList core_type_l hash in
                            if isSeq then 
                             if sizeSeq>0 then
                              makeSeqSup nameL sizeSeq []
                             else 
                              [Grammar.Seq nameL] 
                            else
                             []
                      |_ -> failwith "makeRule failed"
                    end
       |_ -> failwith "replaceConstr failed" 
     in (elem_list,!taille)

let makeRule (e:constructor_declaration) (l:type_declaration list) (hash:(string,string) Hashtbl.t)= 
 let args = e.pcd_args in
  let li = List.map (fun e -> decode_attributes  e.ptyp_attributes) args in
   let elt = List.hd li and arg = List.hd args in
    handleCoreType  e.pcd_name.txt elt arg l hash

let makeGrammar (l:type_declaration list) (kind:type_kind) (name:string) 
                (manifest:core_type option) (hash:(string,string) Hashtbl.t) = 
 match kind with
  |Ptype_variant lv -> (name, List.map (fun e-> Grammar.Call e.pcd_name.txt) lv)::
                             (List.map (fun e -> makeRule e l hash) lv )  
  |Ptype_abstract -> begin 
                      try 
                        ignore(Hashtbl.find hash name); []
                      with Not_found -> [handleConstr l name hash manifest] end 
  | _ -> failwith "makeGrammar failed"


let elemToAst (loc:Location.t) (elem:Grammar.elem)=
let nameConstr, s =
 match elem with
  |Grammar.Seq t -> "Seq",t  
  |Grammar.Elem t -> "Elem",t
 in
  Exp.construct {txt = Lident nameConstr; loc = loc} 
                (Some (Exp.constant (Const_string (s,None))))

let emptyList (loc:Location.t) = Exp.construct {txt = Lident "[]";loc=loc} None

let listToAst (l:'a list) (loc:Location.t) (f:'a -> expression) =
 let rec aux li = 
  match li with 
   |[] -> failwith "empty list" 
   |h::[] -> Exp.tuple [(f h); emptyList loc]
   |h::q -> Exp.tuple [(f h); Exp.construct {txt = Lident "::"; loc = loc} (Some (aux q))]
 in
  if List.length l > 0 then
   Exp.construct  {txt = Lident "::"; loc = loc } (Some (aux l))
  else 
   emptyList loc

let componentToAst (loc:Location.t) (comp:Grammar.component) = 
 match comp with
 |Grammar.Call r -> Exp.construct {txt = Lident "Call"; loc = loc} 
                       (Some (Exp.constant (Const_string (r,None))))
 |Grammar.Cons (i,el) ->  Exp.construct {txt = Lident "Cons"; loc = loc} 
                           (Some (Exp.tuple [Exp.constant (Const_int i); listToAst el loc (elemToAst loc)]))
  
let ruleToAst (loc:Location.t) ((st,cl):(string*'a list)) = 
 Exp.tuple [Exp.constant (Const_string (st,None)); listToAst cl loc (componentToAst loc)]
(*
let makeModuleExpr loc structure_item_list 

let makeModule loc name structure_item_list =
 let expr = makeModuleExpr structure_item_list in
 let module_binding = {pmb_name={txt=name;loc=loc; pmb_expr = expr;  pmb_attributes=[] ; pmb_loc=loc}
 Str.module_ module_binding

*)
let grammarToAst (grammar:Grammar.grammar) (name:string) (loc:Location.t) = 
 let grammar_name  = Pat.var {txt = "grammar_"^name;loc = loc} in 
  let grammar_ast = listToAst grammar loc (ruleToAst loc) in
   let module_name = Pat.var {txt="Tools_"^name;loc=loc} in
  (*  makeModule name structure_item_list
*)
  [%stri module Tools = struct
     let [%p grammar_name] = [%e grammar_ast]
     let gen f =  let res = Gen.generator [%e grammar_ast] true 123124 10 50 0.001 0.1 0.0001 0.1 100 0.8 8 0.0 "ocaml" 1 in 
                    match res with
                     |None -> failwith "ici"
                     |Some (tree,size,state) ->      
                    Tree.file_of_tree true false tree stdout
                 (* ;
                  let rec aux t =
                   match t with
                    |Tree.Leaf (ty,id) ->
                     (*[%e Exp.construct {txt = Lident ty; loc=loc}
                                          (Some (Exp.apply (Exp.ident {txt=Lident "f"; loc=loc})
                                                [(Nolabel,Exp.construct {txt=Lident "()"; loc=loc} None)])) ] in () *)
                               let j =  ty (f ()) in ()
                    |Tree.Node (ty,id,tree_list) ->  ()
                  in aux tree
*)     end]



let rec findSpec (structure_item:structure_item) =
 match structure_item with
  |{pstr_desc = Pstr_type l ; pstr_loc} -> 
    begin
     let rec findSpec li = 
      match li with 
       |[] -> false,[] 
       |h::q -> if List.exists (fun (e,f) -> e.txt="spec") (h.ptype_attributes) then 
                 begin
                  let hash = Hashtbl.create (List.length l) in 
                   let la = List.map (fun e -> makeGrammar l e.ptype_kind e.ptype_name.txt e.ptype_manifest hash) l
                    in
                     true,[grammarToAst (List.flatten la) (List.hd l).ptype_name.txt pstr_loc] 
                 end
                else 
                 findSpec q
      in
        findSpec l 
      end                                                     
  |x -> false,[]

let rec traversListStruct (str:structure) = 
 match str with
  |[] -> []
  |h::q-> let b,nl = findSpec h in
           if b then 
            h::(nl@(traversListStruct q ))
           else 
            h::(traversListStruct q) 

     
let makeSpec (argv:string list) =                                                                           
 {default_mapper with structure = fun mapper structure -> traversListStruct structure}                                                                                                    

let () = register "ppxSpec" makeSpec
