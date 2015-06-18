open Ast_mapper                                                            
open Ast_helper                                                                                      
open Asttypes                                                                                        
open Parsetree                                                                                       
open Longident                                                                                       
open Ast_mapper                                                                                      


let handle ~loc ab=                                                                                  
match ab with                                                                                       
|[%expr let [%p? pa] = [%e? e1] in [%e? e2]] ->                                                    
[%expr let [%p pa] = List.fold_left (+) 0 [%e e1] in [%e e2]]                                          
|_ -> raise Not_found                                                                          


let getenv s = try Sys.getenv s with Not_found -> ""                                                 

  let getenv_mapper argv =                                                                           
(* Our getenv_mapper only overrides the handling of expressions in the default mapper. *)            
{ default_mapper with                                                                                
  expr = fun mapper expr ->                                                                          
    match expr with                                                                                  
    (* Is this an extension node? *)                                                                 
    | { pexp_desc =                                                                                  
      (* Should have name "add". *)                                                                  
        Pexp_extension ({ txt = "add"; loc }, pstr)} ->                                              
        begin match pstr with                                                                        
        | (* Should have a single structure item, which is evaluation of a constant string. *)       
        PStr [{ pstr_desc = Pstr_eval (e,_)}] -> handle ~loc e                                       
        (* Replace with a constant string with the value from the environment. *)                    
        | _ ->                                                                                  
        raise (Location.Error (                                                                    

              Location.error ~loc "[%getenv] accepts a string, e.g. [%getenv \"USER\"]"))          
        end                                                                                        
        (* Delegate to the default mapper. *)                                                      
        | x -> default_mapper.expr mapper x;                                                       
}                                                                                                    

let () = register "getenv" getenv_mapper  
