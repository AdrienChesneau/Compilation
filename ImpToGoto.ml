module Imp = ImpAST
module Gto = GotoAST
open CommonAST

let (++) = Gto.(++)
  
let new_label =
  let cpt = ref 0 in
  fun () -> incr cpt; CommonAST.Lab (Printf.sprintf "_label_%i" !cpt)

let rec strip_location = function
  |Imp.Identifier id -> Gto.Identifier id
     

let rec translate_expression = function
  |Imp.Literal(l) -> Gto.Literal(l)
  |Imp.Location(l)-> Gto.Location(strip_location l)
  |Imp.UnaryOp(u,e)-> Gto.UnaryOp(u,translate_expression e)
  |Imp.BinaryOp(b,e1,e2)-> Gto.BinaryOp(b,translate_expression e1,translate_expression e2)

let rec translate_instruction = function
  | Imp.Conditional (e,i1,i2) ->
     let then_label = new_label()
     and else_label = new_label()
     and end_label = new_label()
     in
     Gto.ConditionalGoto(then_label,translate_expression e)
     ++ translate_instruction(i1)
     ++ Gto.Goto(end_label) (*Fin bloc else go to end*)
     ++ Gto.Label(then_label) (*Bloc then*)
     ++ translate_instruction(i2)
     ++ Gto.Label(end_label)

  |Imp.Print (e) -> Gto.Print (translate_expression e)
  |Imp.Sequence (i1,i2) -> Gto.Sequence(translate_instruction i1, translate_instruction i2)
  |Imp.Set(l,e) -> Gto.Set(strip_location l,translate_expression e)
  |Imp.Loop(e,i) ->
     let if_label = new_label()
     and end_label= new_label()
     in
     Gto.Label(if_label)
     ++Gto.ConditionalGoto(end_label,translate_expression e)
     ++translate_instruction(i)
     ++Gto.Goto(if_label)
     ++Gto.Label(end_label)				   
  | Imp.Nop ->Gto.Nop
  | _ -> failwith "Not implemented"

let translate_program p = Gto.({
  main = translate_instruction Imp.(p.main);
  globals = Imp.(p.globals);
})
