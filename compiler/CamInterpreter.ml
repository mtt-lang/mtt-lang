open Base
open Cam

let rec interept (stack : Cam.Values.t list) (program : Instructions.t list) =
  (* let insts = Cam.dump_instructions program in
     let _ = Stdio.print_string (insts ^ "\n=======\n") in *)
  match program with
  | [] -> (
      match stack with [] -> failwith "error stack, no value" | e :: _ -> e)
  | inst :: others -> (
      match inst with
      | Fst -> (
          match stack with
          | Values.VPair { e; f = _ } :: s -> interept (e :: s) others
          | _ -> failwith "error stack for Fst")
      | Snd -> (
          match stack with
          | Values.VPair { e = _; f } :: s -> interept (f :: s) others
          | _ -> failwith "error stack for Snd")
      | Num { n } -> (
          match stack with
          | _ :: s ->
              interept (Values.VNum { n = Mtt.Nat.to_int n } :: s) others
          | _ -> failwith "error stack for Num")
      | Push -> (
          match stack with
          | e :: s -> interept (e :: e :: s) others
          | _ -> failwith "error stack for Push")
      | Swap -> (
          match stack with
          | e :: f :: s -> interept (f :: e :: s) others
          | _ -> failwith "error stack for Swap")
      | Cons -> (
          match stack with
          | e' :: f' :: s ->
              interept (Values.VPair { e = f'; f = e' } :: s) others
          | _ -> failwith "error stack for Cons")
      | Cur { prog } -> (
          match stack with
          | e :: s -> interept (Values.VClos { e; p = prog } :: s) others
          | _ -> failwith "error stack for Cur")
      | App -> (
          match stack with
          | Values.VPair { e = Values.VClos { e = e'; p = p' }; f } :: s ->
              interept (Values.VPair { e = e'; f } :: s) (p' @ others)
          | _ -> failwith "error stack for App")
      | Plus -> (
          match stack with
          (* only Nat-type is supported for now  *)
          | Values.VPair
              { e = Values.VNum { n = ne }; f = Values.VNum { n = nf } }
            :: s ->
              interept (Values.VNum { n = ne + nf } :: s) others
          | _ -> failwith "error stack for Plus"))

let rec val2malfunction (v : Values.t) : Malfunction.t =
  let open Malfunction in
  let _ = Stdio.print_string @@ Cam.dump_value v ^ "\n" in
  match v with
  | VUnit -> failwith "malfunction doesn't support unit"
  | VClos { e = _; p = _ } -> failwith "some problems with clos"
  | VNum { n } -> Mnum (`Int n)
  | VPair { e; f } -> Mblock (2, [ val2malfunction e; val2malfunction f ])
