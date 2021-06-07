open Base
open Base.Result.Let_syntax
open Cam

type error = [ `InterpretationError of string ]

let rec interept (stack : valueCAM list) (program : instructionCAM list) =
  match program with
  | [] -> (
      match stack with
      | [] -> failwith "error stack, no value"
      | e :: _ -> return e)
  | inst :: others -> (
      match inst with
      | IFst -> (
          match stack with
          | VPair { e; f = _ } :: s -> interept (e :: s) others
          | _ ->
              Result.fail
              @@ `InterpretationError "Inconsistent stack for `Fst` command")
      | ISnd -> (
          match stack with
          | VPair { e = _; f } :: s -> interept (f :: s) others
          | _ ->
              Result.fail
              @@ `InterpretationError "Inconsistent stack for `Snd` command")
      | IQuote { v } -> (
          match stack with
          | _ :: s -> interept (v :: s) others
          (* TODO: check this case *)
          | [] -> interept [ v ] others)
      | IPush -> (
          match stack with
          | e :: s -> interept (e :: e :: s) others
          | _ ->
              Result.fail
              @@ `InterpretationError "Inconsistent stack for `Push` command")
      | ISwap -> (
          match stack with
          | e :: f :: s -> interept (f :: e :: s) others
          | _ ->
              Result.fail
              @@ `InterpretationError "Inconsistent stack for `Swap` command")
      | ICons -> (
          match stack with
          | e' :: f' :: s -> interept (VPair { e = f'; f = e' } :: s) others
          | _ ->
              Result.fail
              @@ `InterpretationError "Inconsistent stack for `Cons` command")
      | ICur { prog } -> (
          match stack with
          | e :: s -> interept (VClos { e; p = prog } :: s) others
          | _ ->
              Result.fail
              @@ `InterpretationError "Inconsistent stack for `Cur` command")
      | ICurRec { prog } -> (
          match stack with
          | e :: s -> interept (VClosRec { e; p = prog } :: s) others
          | _ ->
              Result.fail
              @@ `InterpretationError "Inconsistent stack for `CurRec` command")
      | IBranch { cond; c1; c2 } -> (
          let%bind value = interept stack cond in
          match value with
          | VNum { n } ->
              if phys_equal n 0 then interept stack (c1 @ others)
              else interept stack (c2 @ others)
          | _ ->
              Result.fail
              @@ `InterpretationError "Inconsistent stack for `Branch` command")
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
              Result.fail
              @@ `InterpretationError "Inconsistent stack for `App` command")
      | IVar _ ->
          Result.fail
          @@ `InterpretationError
               "This command is needed only for correctness index at compile \
                time"
      | IPlus -> (
          match stack with
          (* only Nat-type is supported for now  *)
          | VPair { e = VNum { n = ne }; f = VNum { n = nf } } :: s ->
              interept (VNum { n = ne + nf } :: s) others
          | _ ->
              Result.fail
              @@ `InterpretationError "Inconsistent stack for `Plus` command")
      | IMinus -> (
          match stack with
          (* only Nat-type is supported for now  *)
          | VPair { e = VNum { n = ne }; f = VNum { n = nf } } :: s ->
              interept (VNum { n = ne - nf } :: s) others
          | _ ->
              Result.fail
              @@ `InterpretationError "Inconsistent stack for `Minus` command")
      | IMul -> (
          match stack with
          (* only Nat-type is supported for now  *)
          | VPair { e = VNum { n = ne }; f = VNum { n = nf } } :: s ->
              interept (VNum { n = ne * nf } :: s) others
          | _ ->
              Result.fail
              @@ `InterpretationError "Inconsistent stack for `Mul` command")
      | IDiv -> (
          match stack with
          (* only Nat-type is supported for now  *)
          | VPair { e = VNum { n = ne }; f = VNum { n = nf } } :: s ->
              interept (VNum { n = ne / nf } :: s) others
          | _ ->
              Result.fail
              @@ `InterpretationError "Inconsistent stack for `Div` command"))
