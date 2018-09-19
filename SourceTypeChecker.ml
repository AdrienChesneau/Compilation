open CommonAST
open SourceLocalisedAST

exception Type_error of typ * typ * (int * int)

type binaryOp =
  |None
  |Some of typ
      
let rec type_expression context e = match e.expr with
  | Literal (Int _) -> typInt
  | Literal(Bool _) -> typBool
  | Location(Identifier(Id name)) -> Symb_Tbl.find name context.identifier_types
  | UnaryOp(o, e) ->
     let type_attendu = if o = Minus then TypInt else TypBool in
     let type_expr = type_expression context e in
     if type_attendu = type_expression then type_attendu else raise(Type_error(type_attendu, type_expr, e.e_pos))
  | BinaryOp(o, e1, e2) ->
     let type_attendu = if o = Eq || o = Neq then None else if o = Add || o = Sub || o = Div || o = Mod then Some(TypeInt) else Some(TypBool) in
     let type_e1 = type_expression context e1 in
     let type_e2 = type_expression context e2 in
     if type_attendu = type_expression then type_attendu else raise(Type_error(type_attendu, type_expr, e.e_pos))
  

let rec typecheck_instruction context i = match i.instr with
  | Print(e) ->
     if type_expression context e = TypInt
     then ()
     else raise (Type_error(TypeInt, , i.i_pos)) (* A compléter *)
  | Set (id, e) ->
     if type_expression context id = type_expression context e then ()
     else raise (Type_error(type_expression 
  | Nop -> ()

  | _ -> failwith "Not implemented"
    
let extract_context p =
  { identifier_types = p.globals; }
    
let typecheck_program p =
  let type_context = extract_context p in
  typecheck_instruction type_context p.main;
    
