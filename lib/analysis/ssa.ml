open Lua_bytecode

type var = {
  reg : int;
  version : int;
}

type operand =
  | Var of var
  | Const of Proto.constant

type phi = {
  dest : var;
  args : (Cfg.block_id * var) list;
}

type insn =
  | Assign of var * operand
  | Binop of Instruction.opcode * var * operand * operand
  | Unop of Instruction.opcode * var * operand
  | Return of var list
  | Other of Instruction.t

type block = {
  id : Cfg.block_id;
  phis : phi list;
  instructions : insn list;
}

type t = {
  proto_name : string;
  blocks : block list;
  edges : Cfg.edge list;
}

(* pretty printers *)

let pp_var ppf (v : var) =
  Format.fprintf ppf "R%d_v%d" v.reg v.version

let pp_operand ppf = function
  | Var v -> pp_var ppf v
  | Const c ->
      match c with
      | Proto.Nil -> Format.fprintf ppf "nil"
      | Proto.Boolean b -> Format.fprintf ppf "%b" b
      | Proto.Number n -> Format.fprintf ppf "%g" n
      | Proto.String s -> Format.fprintf ppf "%S" s

let pp_phi ppf (p : phi) =
  let args_str =
    List.map (fun (b_id, v) -> Format.asprintf "B%d: %a" b_id pp_var v) p.args
    |> String.concat ", "
  in
  Format.fprintf ppf "%a = phi(%s)" pp_var p.dest args_str

let pp_insn ppf = function
  | Assign (dst, src) ->
      Format.fprintf ppf "%a = %a" pp_var dst pp_operand src
  | Binop (op, dst, lhs, rhs) ->
      Format.fprintf ppf "%a = %a %a %a"
        pp_var dst Instruction.pp_opcode op pp_operand lhs pp_operand rhs
  | Unop (op, dst, arg) ->
      Format.fprintf ppf "%a = %a %a"
        pp_var dst Instruction.pp_opcode op pp_operand arg
  | Return vars ->
      let vars_str = List.map (Format.asprintf "%a" pp_var) vars |> String.concat ", " in
      Format.fprintf ppf "return %s" vars_str
  | Other insn ->
      Instruction.pp ppf insn

(* SSA Construction Algorithm (Braun et al. 2013) *)

let of_cfg (proto : Proto.t) (cfg : Cfg.t) : t =
  let reg_count = max 256 (proto.max_stack_size + 1) in
  let var_counters = Array.make reg_count 0 in

  let new_var reg =
    let ver = var_counters.(reg) in
    var_counters.(reg) <- ver + 1;
    { reg; version = ver }
  in

  (* current_defs.(block_id).(reg) tracks the current ssa_var in that block *)
  let num_blocks = List.length cfg.blocks in
  let current_defs = Array.make_matrix (num_blocks + 1) reg_count None in

  let get_preds target_b =
    List.filter_map (fun (e : Cfg.edge) ->
      if e.dst = target_b then Some e.src else None
    ) cfg.edges
  in

  (* helper to resolve an RK operand to SSA operand *)
  let resolve_rk (current_def_table : var option array) (rk_val : int) : operand =
    match Instruction.decode_rk rk_val with
    | Instruction.Reg r ->
        let v =
          match current_def_table.(r) with
          | Some var -> var
          | None -> { reg = r; version = 0 }
        in
        Var v
    | Instruction.Const idx ->
        if idx >= 0 && idx < Array.length proto.constants then
          Const proto.constants.(idx)
        else
          Const Proto.Nil
  in

  let ssa_blocks =
    List.map (fun (b : Cfg.basic_block) ->
      let b_id = b.id in
      let block_def = current_defs.(b_id) in
      let phis = ref [] in

      (* handle block entry and Phi-node placement *)
      if b_id = cfg.entry then begin
        for r = 0 to proto.num_params - 1 do
          let v = new_var r in
          block_def.(r) <- Some v
        done
      end else begin
        let preds = get_preds b_id in
        match preds with
        | [] -> ()
        | [p] ->
            for r = 0 to reg_count - 1 do
              block_def.(r) <- current_defs.(p).(r)
            done
        | _ ->
            (* multiple predecessors: generate Phi-nodes if versions differ *)
            for r = 0 to reg_count - 1 do
              let pred_vars =
                List.filter_map (fun p ->
                  match current_defs.(p).(r) with
                  | Some v -> Some (p, v)
                  | None -> None
                ) preds
              in
              match pred_vars with
              | [] -> ()
              | (_, first_v) :: rest ->
                  let all_same = List.for_all (fun (_, v) -> v.version = first_v.version) rest in
                  if all_same then
                    block_def.(r) <- Some first_v
                  else begin
                    let phi_dest = new_var r in
                    phis := { dest = phi_dest; args = pred_vars } :: !phis;
                    block_def.(r) <- Some phi_dest
                  end
            done
      end;

      (* translate instructions into SSA *)
      let ssa_insns = ref [] in

      Array.iter (fun (insn : Instruction.t) ->
        match insn.op with
        | Instruction.OP_MOVE ->
            let src = resolve_rk block_def insn.b in
            let dst = new_var insn.a in
            block_def.(insn.a) <- Some dst;
            ssa_insns := Assign (dst, src) :: !ssa_insns

        | Instruction.OP_LOADK ->
            let src =
              if insn.bx >= 0 && insn.bx < Array.length proto.constants then
                Const proto.constants.(insn.bx)
              else Const Proto.Nil
            in
            let dst = new_var insn.a in
            block_def.(insn.a) <- Some dst;
            ssa_insns := Assign (dst, src) :: !ssa_insns

        | Instruction.OP_LOADBOOL ->
            let src = Const (Proto.Boolean (insn.b <> 0)) in
            let dst = new_var insn.a in
            block_def.(insn.a) <- Some dst;
            ssa_insns := Assign (dst, src) :: !ssa_insns

        | Instruction.OP_LOADNIL ->
            for r = insn.a to insn.b do
              let dst = new_var r in
              block_def.(r) <- Some dst;
              ssa_insns := Assign (dst, Const Proto.Nil) :: !ssa_insns
            done

        | Instruction.OP_ADD
        | Instruction.OP_SUB
        | Instruction.OP_MUL
        | Instruction.OP_DIV
        | Instruction.OP_MOD
        | Instruction.OP_POW ->
            let lhs = resolve_rk block_def insn.b in
            let rhs = resolve_rk block_def insn.c in
            let dst = new_var insn.a in
            block_def.(insn.a) <- Some dst;
            ssa_insns := Binop (insn.op, dst, lhs, rhs) :: !ssa_insns

        | Instruction.OP_UNM
        | Instruction.OP_NOT
        | Instruction.OP_LEN ->
            let arg = resolve_rk block_def insn.b in
            let dst = new_var insn.a in
            block_def.(insn.a) <- Some dst;
            ssa_insns := Unop (insn.op, dst, arg) :: !ssa_insns

        | Instruction.OP_RETURN ->
            let vars =
              if insn.b > 1 then
                let lst = ref [] in
                for r = insn.a to insn.a + insn.b - 2 do
                  match block_def.(r) with
                  | Some v -> lst := v :: !lst
                  | None -> lst := { reg = r; version = 0 } :: !lst
                done;
                List.rev !lst
              else []
            in
            ssa_insns := Return vars :: !ssa_insns

        | _ ->
            ssa_insns := Other insn :: !ssa_insns
      ) b.instructions;

      { id = b_id; phis = List.rev !phis; instructions = List.rev !ssa_insns }
    ) cfg.blocks
  in

  { proto_name = cfg.proto_name; blocks = ssa_blocks; edges = cfg.edges }

  (* export SSA to Graphviz DOT *)

  let to_dot (ssa : t) : string =
    let buf = Buffer.create 1024 in
    let fmt = Format.formatter_of_buffer buf in

    Format.fprintf fmt "digraph \"SSA_%s\" {\n" ssa.proto_name;
    Format.fprintf fmt "  node [shape=box, fontname=\"Courier\"];\n";
    Format.fprintf fmt "  edge [fontname=\"Helvetica\", fontsize=10];\n\n";

    List.iter (fun (b : block) -> 
      let phi_lines =
        List.map (fun phi -> Format.asprintf "%a" pp_phi phi) b.phis
      in
      let insn_lines =
        List.map (fun insn -> Format.asprintf "%a" pp_insn insn) b.instructions
      in
      let all_lines = phi_lines @ insn_lines |> String.concat "\\l" in
      Format.fprintf fmt "  block_%d [label=\"Block %d\\l%s\\l\"];\n" b.id b.id all_lines
      ) ssa.blocks;

      Format.fprintf fmt "\n";

      List.iter (fun (e : Cfg.edge) ->
        let style =
          match e.kind with
          | Cfg.Fallthrough -> "label=\"fallthrough\", color=gray"
          | Cfg.Jump -> "label=\"jump\", color=blue"
          | Cfg.Cond_true -> "label=\"true\", color=green"
          | Cfg.Cond_false -> "label=\"false\", color=red"
        in
        Format.fprintf fmt "  block_%d -> block_%d [%s];\n" e.src e.dst style
        ) ssa.edges;

        Format.fprintf fmt "}\n";
        Format.pp_print_flush fmt ();
        Buffer.contents buf