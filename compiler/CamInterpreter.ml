open Base
open Cam

let rec instr_for_var (n : int) =
  if phys_equal n 0 then [ ISnd ] else IFst :: instr_for_var (n - 1)

let rec interept (stack : valueCAM list) (program : instructionCAM list) =
  (* let insts = dump_instructions program in
     let _ = Stdio.print_string (insts ^ "\n=======\n") in *)
  match program with
  | [] -> (
      match stack with [] -> failwith "error stack, no value" | e :: _ -> e)
  | inst :: others -> (
      match inst with
      | IFst -> (
          match stack with
          | VPair { e; f = _ } :: s -> interept (e :: s) others
          | _ -> failwith "error stack for Fst")
      | ISnd -> (
          match stack with
          | VPair { e = _; f } :: s -> interept (f :: s) others
          | _ -> failwith "error stack for Snd")
      | IQuote { v } -> (
          match stack with
          | _ :: s -> interept (v :: s) others
          (* TODO: check this case *)
          | [] -> interept [ v ] others)
      | IPush -> (
          match stack with
          | e :: s -> interept (e :: e :: s) others
          | _ -> failwith "error stack for Push")
      | ISwap -> (
          match stack with
          | e :: f :: s -> interept (f :: e :: s) others
          | _ -> failwith "error stack for Swap")
      | ICons -> (
          match stack with
          | e' :: f' :: s -> interept (VPair { e = f'; f = e' } :: s) others
          | _ -> failwith "error stack for Cons")
      | ICur { prog } -> (
          match stack with
          | e :: s -> interept (VClos { e; p = prog } :: s) others
          | _ -> failwith "error stack for Cur")
      | ICurRec { prog } -> (
          match stack with
          | e :: s -> interept (VClosRec { e; p = prog } :: s) others
          | _ -> failwith "error stack for CurRec")
      | IBranch { cond; c1; c2 } -> (
          match interept stack cond with
          | VNum { n } ->
              if phys_equal n 0 then interept stack (c1 @ others)
              else interept stack (c2 @ others)
          | _ -> failwith "only nat supports pattern-mathcing")
      | IApp -> (
          match stack with
          | VPair { e = VClos { e = e'; p = p' }; f } :: s ->
              interept (VPair { e = e'; f } :: s) (p' @ others)
          | VPair { e = VClosRec { e = e'; p = p' }; f } :: s ->
              let inner_pair =
                VPair { e = e'; f = VClosRec { e = e'; p = p' } }
              in
              interept (VPair { e = inner_pair; f } :: s) (p' @ others)
          | _ ->
              let _ =
                Option.map
                  (Option.map (List.hd stack) ~f:dump_value)
                  ~f:Stdio.print_string
              in
              failwith "error stack for App")
      | IVar { i } ->
          let code = instr_for_var i in
          interept stack (code @ others)
      | IPlus -> (
          match stack with
          (* only Nat-type is supported for now  *)
          | VPair { e = VNum { n = ne }; f = VNum { n = nf } } :: s ->
              interept (VNum { n = ne + nf } :: s) others
          | _ -> failwith "error stack for Plus")
      | IMinus -> (
          match stack with
          (* only Nat-type is supported for now  *)
          | VPair { e = VNum { n = ne }; f = VNum { n = nf } } :: s ->
              interept (VNum { n = ne - nf } :: s) others
          | _ -> failwith "error stack for Minus")
      | IMul -> (
          match stack with
          (* only Nat-type is supported for now  *)
          | VPair { e = VNum { n = ne }; f = VNum { n = nf } } :: s ->
              interept (VNum { n = ne * nf } :: s) others
          | _ -> failwith "error stack for Mul")
      | IDiv -> (
          match stack with
          (* only Nat-type is supported for now  *)
          | VPair { e = VNum { n = ne }; f = VNum { n = nf } } :: s ->
              interept (VNum { n = ne / nf } :: s) others
          | _ -> failwith "error stack for Div"))
