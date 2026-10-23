open Lua_bytecode

module RegSet = Set.Make(Int)

type use_def = {
    uses : RegSet.t;
    defs : RegSet.t;
}

type uninitialized_read = {
    pc : int;
    reg : int;
    insn : Instruction.t;
}

type report = {
    proto_id : string;
    violations : uninitialized_read list;
}

(** helper to add RK opperand (only if is a Register) *)
let add_rk (rk_val : int) (set : RegSet.t) : RegSet.t =
    match Instruction.decode_rk rk_val with
    | Instruction.Reg r -> RegSet.add r set
    | Instruction.Const _ -> set

(** helper to add a range of registers [a .. b] *)
let add_range (start_r : int) (end_r : int) (set : RegSet.t) :
RegSet.t = 
    let s = ref set in
    for r = start_r to end_r do
        s := RegSet.add r !s
    done;
    !s

let insn_use_def (insn : Instruction.t) : use_def =
    let empty = RegSet.empty in
    match insn.op with
    | Instruction.OP_MOVE ->
        { defs = RegSet.singleton insn.a; uses = RegSet.singleton insn.b }

    | Instruction.OP_LOADK
    | Instruction.OP_LOADBOOL ->
        { defs = RegSet.singleton insn.a; uses = empty }
    
    | Instruction.OP_LOADNIL ->
        { defs = add_range insn.a insn.b empty; uses = empty }
    | Instruction.OP_ADD
    | Instruction.OP_SUB
    | Instruction.OP_MUL
    | Instruction.OP_DIV
    | Instruction.OP_MOD
    | Instruction.OP_POW ->
        let uses = empty |> add_rk insn.b |> add_rk insn.c in 
        { defs = RegSet.singleton insn.a; uses }
    | Instruction.OP_UNM
    | Instruction.OP_NOT
    | Instruction.OP_LEN ->
        { defs = RegSet.singleton insn.a; uses = RegSet.singleton insn.b }

    | Instruction.OP_EQ
    | Instruction.OP_LT
    | Instruction.OP_LE ->
        let uses = empty |> add_rk insn.b |> add_rk insn.c in
        { defs = empty; uses }

    | Instruction.OP_TEST ->
        { defs = empty; uses = RegSet.singleton insn.a }

    | Instruction.OP_TESTSET ->
        { defs = RegSet.singleton insn.a; uses = RegSet.singleton insn.b }

    | Instruction.OP_RETURN ->
        let uses =
            if insn.b > 1 then add_range insn.a (insn.a + insn.b - 2) empty
            else empty
        in
        { defs = empty; uses }

    | Instruction.OP_FORPREP ->
        { defs = RegSet.singleton insn.a; uses = RegSet.singleton (insn.a + 2) }

    | Instruction.OP_FORLOOP ->
        let uses = add_range insn.a (insn.a + 2) empty in
        let defs = RegSet.add (insn.a + 3) (RegSet.singleton insn.a) in
        { defs; uses }

    | _ ->
        { defs = empty; uses = empty }

(** finds all predecessor block IDs for a given block in the CFG *)
let get_preds (edges : Cfg.edge list) (target_block : Cfg.block_id) : Cfg.block_id list =
    List.filter_map (fun (e : Cfg.edge) ->
        if e.dst = target_block then Some e.src else None
    ) edges

let transfer_block (in_set : RegSet.t) (b : Cfg.basic_block)
    : RegSet.t * uninitialized_read list =
    let cur = ref in_set in
    let violations = ref [] in

    Array.iteri (fun idx insn ->
        let pc = b.start_pc + idx in
        let ud = insn_use_def insn in
        (* check for uninitialized reads *)
        RegSet.iter (fun r ->
            if not (RegSet.mem r !cur) then
                violations := { pc; reg = r; insn } :: !violations
            ) ud.uses;
                (* add newly defined registers *)
                cur := RegSet.union !cur ud.defs
    ) b.instructions;
    (!cur, List.rev !violations)

let verify_registers ?(name = "main") (p : Proto.t) : report =
    let cfg = Cfg.build ~name p in
    let initial_params = add_range 0 (p.num_params - 1) RegSet.empty in

    let universe = add_range 0 (max 255 (p.max_stack_size + 32)) RegSet.empty in
    let out_sets = Hashtbl.create 16 in
    List.iter (fun (b : Cfg.basic_block) ->
        Hashtbl.replace out_sets b.id universe
    ) cfg.blocks;
    
    let compute_in_set (b : Cfg.basic_block) : RegSet.t =
        if b.id = cfg.entry then initial_params
        else
            let preds = get_preds cfg.edges b.id in
            match preds with
            | [] -> initial_params
            | p0 :: rest ->
                let s0 = try Hashtbl.find out_sets p0 with Not_found -> universe in
                List.fold_left (fun acc pred_id -> 
                    let sp = try Hashtbl.find out_sets pred_id with Not_found -> universe in
                    RegSet.inter acc sp
                ) s0 rest
    in

    (* fixed point loop *)
    let rec iterate () =
        let changed = ref false in
        List.iter (fun (b : Cfg.basic_block) ->
            let in_set = compute_in_set b in
            let new_out, _ = transfer_block in_set b in
            let old_out = try Hashtbl.find out_sets b.id with Not_found -> RegSet.empty in
            if not (RegSet.equal new_out old_out) then begin
                changed := true;
                Hashtbl.replace out_sets b.id new_out
            end
        ) cfg.blocks;
        if !changed then iterate ()
    in
    iterate ();

    (* final pass to collect all violations *)
    let all_violations =
        List.map (fun (b : Cfg.basic_block) ->
            let in_set = compute_in_set b in
            let _, violations = transfer_block in_set b in
            violations
        ) cfg.blocks
        |> List.flatten
    in

    { proto_id = name; violations = all_violations }

let pp_report ppf (r : report) =
    Format.fprintf ppf "Liveness and Register Verification for [%s]\n" r.proto_id;
    if r.violations = [] then
        Format.fprintf ppf "    Status: PASSED (all register reads are safelu initialized)\n\n"
    else begin
        Format.fprintf ppf "    Status: FAILED (%d uninitialized reads detected):\n"
            (List.length r.violations);
        List.iter (fun v ->
            Format.fprintf ppf "    [BUG] at pc %d: instruction '%a' reads uninitialized register R(%d)\n"
                v.pc Instruction.pp v.insn v.reg
        ) r.violations;
        Format.fprintf ppf "\n"
    end