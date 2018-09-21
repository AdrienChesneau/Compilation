open CommonAST
open SourceLocalisedAST

exception Type_error of typ * typ * (int * int)

type binaryOp =
  |None
  |Some of (typ*typ)
      
let rec type_expression context e = match e.expr with
  | Literal (Int _) -> TypInt
  | Literal(Bool _) -> TypBool
  | Location(Identifier(Id name)) -> Symb_Tbl.find name context.identifier_types
  | UnaryOp(o, e) ->
     let type_o = if o = Minus then TypInt else TypBool in
     let type_e = type_expression context e in
     if type_o = type_e then type_o else begin Printf.printf("Erreur unary op"); raise(Type_error(type_o, type_e, e.e_pos)) end
  | BinaryOp(o, e1, e2) ->
     let type_o = if o = Eq || o = Neq then None
       else if o = Add || o = Sub || o = Mult || o = Div || o = Mod then Some(TypInt, TypInt)
       else if o = Lt || o = Le || o = Gt || o = Ge then Some(TypInt, TypBool) 
       else Some(TypBool, TypBool) in
     let type_e1 = type_expression context e1 in
     let type_e2 = type_expression context e2 in
     match type_o with
     |None -> if type_e1 = type_e2 then TypBool else raise (Type_error(type_e1, type_e2, e1.e_pos))
     |Some(TypInt, TypInt) -> if type_e1 = TypInt && type_e2 = TypInt then TypInt else begin Printf.printf("Erreur binary ti, ti");raise (Type_error(TypInt, TypBool, e1.e_pos)) end
     |Some(TypInt, TypBool) -> if type_e1 = TypInt && type_e2 = TypInt then TypBool else begin Printf.printf("Erreur binary ti, tb");raise (Type_error(TypInt, TypBool, e1.e_pos)) end
     |Some(TypBool, TypBool) -> if type_e1 = TypBool && type_e2 = TypBool then TypBool else begin Printf.printf("Erreur binary tb tb");raise (Type_error(TypBool, TypInt, e1.e_pos)) end
     |Some(_, _) -> failwith "Unhandled case"
	 
let rec typecheck_instruction context i = match i.instr with
  | Print(e) ->
     if type_expression context e = TypInt
     then ()
     else begin Printf.printf("Erreur print"); raise (Type_error(TypInt, type_expression context e, i.i_pos)) end
  | Set(Identifier(Id id), e) ->
     if Symb_Tbl.find id context.identifier_types = type_expression context e then ()
     else begin Printf.printf("Erreur set"); raise (Type_error(Symb_Tbl.find id context.identifier_types, type_expression context e, i.i_pos)) end
  | Conditional(e, i1, i2) ->
     if type_expression context e = TypBool then begin typecheck_instruction context i1; typecheck_instruction context i2 end
     else begin Printf.printf("Erreur conditional"); raise (Type_error(TypBool, type_expression context e, i.i_pos)) end
  | Loop(e, i) ->
     if type_expression context e = TypBool then typecheck_instruction context i
     else begin Printf.printf("Erreur loop"); raise (Type_error(TypBool, type_expression context e, i.i_pos)) end
  | Sequence(i1, i2) -> typecheck_instruction context i1; typecheck_instruction context i2
  | Nop -> ()
    
let extract_context p =
  { identifier_types = p.globals; }
    
let typecheck_program p =
  let type_context = extract_context p in
  typecheck_instruction type_context p.main;
    
