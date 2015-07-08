val makeSeqSup :
  string -> int -> Arbolib.Grammar.elem list -> Arbolib.Grammar.elem list
val makeNameList :
  Parsetree.core_type list -> (string, string) Hashtbl.t -> string
val getNb : Parsetree.payload -> int
val decode_attributes : Parsetree.attributes -> int * bool * int
val handle_tuple :
  Parsetree.core_type list ->
  bool -> int -> (string, string) Hashtbl.t -> Arbolib.Grammar.elem list
val handleCoreType :
  string ->
  int * bool * int ->
  Parsetree.core_type ->
  Parsetree.type_declaration list ->
  (string, string) Hashtbl.t -> string * Arbolib.Grammar.component list
val handleConstr :
  Parsetree.type_declaration list ->
  string ->
  (string, string) Hashtbl.t ->
  Parsetree.core_type option -> string * Arbolib.Grammar.component list
val replaceConstr :
  Longident.t Asttypes.loc * Parsetree.core_type list ->
  string ->
  Parsetree.type_declaration list ->
  (string, string) Hashtbl.t -> Arbolib.Grammar.elem list * int
val makeRule :
  Parsetree.constructor_declaration ->
  Parsetree.type_declaration list ->
  (string, string) Hashtbl.t -> string * Arbolib.Grammar.component list
val makeGrammar :
  Parsetree.type_declaration list ->
  Parsetree.type_kind ->
  string ->
  Parsetree.core_type option ->
  (string, string) Hashtbl.t ->
  (string * Arbolib.Grammar.component list) list
val elemToAst : Location.t -> Arbolib.Grammar.elem -> Parsetree.expression
val emptyList : Location.t -> Parsetree.expression
val listToAst :
  'a list ->
  Location.t -> ('a -> Parsetree.expression) -> Parsetree.expression
val componentToAst :
  Location.t -> Arbolib.Grammar.component -> Parsetree.expression
val ruleToAst :
  Location.t ->
  string * Arbolib.Grammar.component list -> Parsetree.expression
val grammarToAst :
  Arbolib.Grammar.grammar -> string -> Location.t -> Parsetree.structure_item
val findSpec :
  Parsetree.structure_item -> bool * Parsetree.structure_item list
val traversListStruct : Parsetree.structure -> Parsetree.structure_item list
val makeSpec : string list -> Ast_mapper.mapper
