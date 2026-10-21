open Lua_bytecode

type block_id = int

type edge_kind = 
    | Fallthrough
    | Jump
    | Cond_true
    | Cond_false

type edge = {
    src : block_id;
    dst : block_id;
    kind : edge_kind;
}

type basic_block = {
    id : block_id;
    start_pc : int;
    end_pc : int;
    instructions : Instruction.t array;
}

type t = {
    proto_name : string;
    blocks : basic_block list;
    edges : edge list;
    entry : block_id;
}

let find_leaders (code : Instruction.t array) : int list =
  let code_len = Array.length code in
  let leaders = ref [0] in (* pc 0 is always a leader *)

  let add_leader pc =
    if pc >= 0 && pc < code_len then
      leaders := pc :: !leaders
  in

  Array.iteri (fun pc (insn : Instruction.t) ->
    match insn.op with
    | Instruction.OP_JMP
    | Instruction.OP_FORPREP
    | Instruction.OP_FORLOOP ->
      let target = pc + 1 + insn.sbx in
      add_leader target; (* jump target is a leader *)
      add_leader (pc + 1) (* instruction after jump is a leader *)
    | Instruction.OP_RETURN
    | Instruction.OP_TAILCALL ->
      add_leader (pc + 1) (* instruction after return is a leader *)
    | Instruction.OP_EQ
    | Instruction.OP_LT
    | Instruction.OP_LE
    | Instruction.OP_TEST
    | Instruction.OP_TESTSET ->
      add_leader (pc + 1); (* false branch *)
      add_leader (pc + 2); (* true branch (skips next instruction) *)
    | _ -> ()
    ) code;

    (* sort and remove duplicates *)
    List.sort_uniq Int.compare !leaders

let build_blocks (code : Instruction.t array) (leaders : int list) : basic_block list =
  let code_len = Array.length code in
  let rec make_blocks id = function
    | [] -> []
    | [start_pc] ->
      let end_pc = code_len - 1 in
      let count = end_pc - start_pc + 1 in
      let instructions =  Array.sub code start_pc count in
      [{ id; start_pc; end_pc; instructions }]
    | start_pc :: (next_pc :: _ as rest) -> 
      let end_pc =  next_pc - 1 in
      let count = end_pc - start_pc + 1 in
      let instructions = Array.sub code start_pc count in
      { id; start_pc; end_pc; instructions } :: make_blocks (id + 1) rest
    in
    make_blocks 0 leaders

(** finds block ID that contains instruction [target_pc] *)
let find_block_by_pc (blocks : basic_block list) (target_pc : int) : block_id option =
  List.find_map (fun b ->
    if target_pc >= b.start_pc && target_pc <= b.end_pc then Some b.id
    else None
    ) blocks

let build_edges (blocks : basic_block list) : edge list =
  let edges = ref [] in

  let add_edge src dst kind =
    edges := { src; dst; kind } :: !edges
  in

  List.iter (fun (b : basic_block) ->
    let last_insn = b.instructions.(Array.length b.instructions - 1) in
    let last_pc = b.end_pc in
    
    match last_insn.op with
    | Instruction.OP_JMP
    | Instruction.OP_FORPREP ->
      let target_pc = last_pc + 1 + last_insn.sbx in
      (match find_block_by_pc blocks target_pc with
      | Some dst -> add_edge b.id dst Jump
      | None -> ())
      
    | Instruction.OP_FORLOOP ->
      let target_pc = last_pc + 1 + last_insn.sbx in
      (match find_block_by_pc blocks target_pc with
      | Some dst -> add_edge b.id dst Jump
      | None -> ());
      (match find_block_by_pc blocks (last_pc + 1) with
      | Some dst -> add_edge b.id dst Fallthrough
      | None -> ())
      
    | Instruction.OP_EQ
    | Instruction.OP_LT
    | Instruction.OP_LE
    | Instruction.OP_TEST
    | Instruction.OP_TESTSET ->
      (* in Lua 5.1, condition jumps to pc+2 (true branch) or pc+1 (false branch) *)
      (match  find_block_by_pc blocks (last_pc + 2) with
      | Some dst -> add_edge b.id dst Cond_true
      | None -> ());
      (match find_block_by_pc blocks (last_pc + 1) with
      | Some dst -> add_edge b.id dst Cond_false
      | None -> ())
      
    | _ ->
      (* normal instruction falls through to the next block *)
      (match find_block_by_pc blocks (last_pc + 1) with
      | Some dst -> add_edge b.id dst Fallthrough
      | None -> ())
  ) blocks;

  List.rev !edges


let build ?(name = "main") (p : Proto.t) : t =
  if Array.length p.code = 0 then
    { proto_name = name; blocks = []; edges = []; entry = 0 }
  else
    let leaders = find_leaders p.code in
    let blocks = build_blocks p.code leaders in
    let edges = build_edges blocks in
    { proto_name = name; blocks; edges; entry = 0 }

let pp_dot ppf (cfg : t) =
  Format.fprintf ppf "digraph \"%s\" {\n" cfg.proto_name;
  Format.fprintf ppf "  node [shape=box, fontname=\"Courier\"];\n";
  Format.fprintf ppf "  edge [fontname=\"Helvetica\", fontsize=10];\n\n";

  (* render blocks *)
  List.iter (fun (b : basic_block) ->
    let insn_strings =
      Array.to_list b.instructions
      |> List.map (fun insn -> Format.asprintf "%a" Instruction.pp insn)
      |> String.concat "\\l"
    in
    Format.fprintf ppf "  block_%d [label=\"Block %d (pc %d..%d)\\l%s\\l\"];\n"
      b.id b.id b.start_pc b.end_pc insn_strings
  ) cfg.blocks;

  Format.fprintf ppf "\n";

  (* render edges *)
  List.iter (fun (e : edge) ->
    let style =
      match e.kind with
      | Fallthrough -> "label=\"fallthrough\", color=gray"
      | Jump -> "label=\"jump\", color=blue"
      | Cond_true -> "label=\"true\", color=green"
      | Cond_false -> "label=\"false\", color=red"
    in
    Format.fprintf ppf " block_%d -> block_%d [%s];\n" e.src e.dst style
    ) cfg.edges;

    Format.fprintf ppf "}\n"

let to_dot (cfg : t) : string =
  Format.asprintf "%a" pp_dot cfg