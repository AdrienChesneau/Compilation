module Imp = ImpAST
module Gto = GotoAST
open CommonAST

let (++) = Gto.(++)
  
let new_label =
  let cpt = ref 0 in
  fun () -> incr cpt; CommonAST.Lab (Printf.sprintf "_label_%i" !cpt)

let rec translate_instruction = function
  | Imp.Nop ->
    Gto.Nop

  | _ -> failwith "Not implemented"

let translate_program p = Gto.({
  main = translate_instruction Imp.(p.main);
  globals = Imp.(p.globals);
})
