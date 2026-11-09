open Llvm
module Env = Map.Make (String)

type env = llvalue Env.t
type t = { context : llcontext; the_module : llmodule; builder : llbuilder }

let init module_name =
  let context = global_context () in
  let the_module = create_module context module_name in
  let builder = builder context in
  { context; the_module; builder }

type loop_ctx = { cond_bb : llbasicblock; end_bb : llbasicblock }

let rec codegen_expr state env (expr : Ast.expr) =
  let i64_t = i64_type state.context in
  match expr.node with
  | IntLit n -> const_of_int64 i64_t n true
  | BoolLit b -> const_of_int64 i64_t (if b then 1L else 0L) true
  | Id name -> (
      match Env.find_opt name env with
      | Some p -> build_load i64_t p name state.builder
      | None -> failwith ("undefined variable " ^ name))
  | UnaryMinus e ->
      let e_val = codegen_expr state env e in
      build_neg e_val "" state.builder
  | UnaryNot e ->
      let e_val = codegen_expr state env e in
      let zero = const_int i64_t 0 in
      let cmp = build_icmp Icmp.Eq e_val zero "" state.builder in
      build_zext cmp i64_t "" state.builder
  | BinOp (lhs, op, rhs) -> (
      match op with
      | And | Or ->
          let func = block_parent (insertion_block state.builder) in
          let lhs_val = codegen_expr state env lhs in
          let zero = const_int i64_t 0 in
          let lhs_bool = build_icmp Icmp.Ne lhs_val zero "" state.builder in
          let rhs_bb = append_block state.context "" func in
          let merge_bb = append_block state.context "" func in
          let current_bb = insertion_block state.builder in

          if op = And then
            ignore (build_cond_br lhs_bool rhs_bb merge_bb state.builder)
          else ignore (build_cond_br lhs_bool merge_bb rhs_bb state.builder);

          position_at_end rhs_bb state.builder;
          let rhs_val = codegen_expr state env rhs in
          let rhs_bool = build_icmp Icmp.Ne rhs_val zero "" state.builder in
          let rhs_end_bb = insertion_block state.builder in
          ignore (build_br merge_bb state.builder);

          position_at_end merge_bb state.builder;
          let phi =
            build_phi
              [ (lhs_bool, current_bb); (rhs_bool, rhs_end_bb) ]
              "" state.builder
          in
          build_zext phi i64_t "" state.builder
      | _ -> (
          let lhs_val = codegen_expr state env lhs in
          let rhs_val = codegen_expr state env rhs in
          match op with
          | Add -> build_add lhs_val rhs_val "" state.builder
          | Sub -> build_sub lhs_val rhs_val "" state.builder
          | Mul -> build_mul lhs_val rhs_val "" state.builder
          | Div -> build_sdiv lhs_val rhs_val "" state.builder
          | Eq ->
              build_zext
                (build_icmp Icmp.Eq lhs_val rhs_val "" state.builder)
                i64_t "" state.builder
          | Ne ->
              build_zext
                (build_icmp Icmp.Ne lhs_val rhs_val "" state.builder)
                i64_t "" state.builder
          | Lt ->
              build_zext
                (build_icmp Icmp.Slt lhs_val rhs_val "" state.builder)
                i64_t "" state.builder
          | Gt ->
              build_zext
                (build_icmp Icmp.Sgt lhs_val rhs_val "" state.builder)
                i64_t "" state.builder
          | Le ->
              build_zext
                (build_icmp Icmp.Sle lhs_val rhs_val "" state.builder)
                i64_t "" state.builder
          | Ge ->
              build_zext
                (build_icmp Icmp.Sge lhs_val rhs_val "" state.builder)
                i64_t "" state.builder
          | _ -> failwith "unreachable"))
  | ErrorExpr -> failwith "cannot codegen ErrorExpr"

let rec codegen_stmt state env loop_ctx (stmt : Ast.statement) =
  let i64_t = i64_type state.context in
  match stmt.node with
  | Decl (id, init_expr, _) ->
      let var_name = id.node in
      let init_val = codegen_expr state env init_expr in
      let alloca = build_alloca i64_t var_name state.builder in
      ignore (build_store init_val alloca state.builder);
      Env.add var_name alloca env
  | Assignment (id, e) ->
      let new_val = codegen_expr state env e in
      let var_ptr = Env.find id.node env in
      ignore (build_store new_val var_ptr state.builder);
      env
  | Ret e ->
      let ret_val = codegen_expr state env e in
      ignore (build_ret ret_val state.builder);
      env
  | Expr e ->
      ignore (codegen_expr state env e);
      env
  | Block body ->
      ignore
        (List.fold_left (fun e s -> codegen_stmt state e loop_ctx s) env body);
      env
  | If (cond, then_bod, else_bod) ->
      let cond_val = codegen_expr state env cond in
      let zero = const_int i64_t 0 in
      let cond_bool =
        build_icmp Icmp.Ne cond_val zero "if_cond" state.builder
      in

      let func = block_parent (insertion_block state.builder) in
      let then_bb = append_block state.context "then" func in
      let merge_bb = append_block state.context "if_merge" func in
      let else_bb =
        match else_bod with
        | Some _ -> append_block state.context "else" func
        | None -> merge_bb
      in

      ignore (build_cond_br cond_bool then_bb else_bb state.builder);

      position_at_end then_bb state.builder;
      ignore (codegen_stmt state env loop_ctx then_bod);
      if block_terminator (insertion_block state.builder) = None then
        ignore (build_br merge_bb state.builder);

      (match else_bod with
      | Some e ->
          position_at_end else_bb state.builder;
          ignore (codegen_stmt state env loop_ctx e);
          if block_terminator (insertion_block state.builder) = None then
            ignore (build_br merge_bb state.builder)
      | None -> ());

      position_at_end merge_bb state.builder;
      env
  | While (cond, body) ->
      let func = block_parent (insertion_block state.builder) in
      let cond_bb = append_block state.context "while_cond" func in
      let body_bb = append_block state.context "while_body" func in
      let end_bb = append_block state.context "while_end" func in

      ignore (build_br cond_bb state.builder);
      position_at_end cond_bb state.builder;

      let cond_val = codegen_expr state env cond in
      let zero = const_int i64_t 0 in
      let cond_bool =
        build_icmp Icmp.Ne cond_val zero "while_cond_bool" state.builder
      in
      ignore (build_cond_br cond_bool body_bb end_bb state.builder);

      position_at_end body_bb state.builder;
      let new_loop_ctx = Some { cond_bb; end_bb } in
      ignore (codegen_stmt state env new_loop_ctx body);
      if block_terminator (insertion_block state.builder) = None then
        ignore (build_br cond_bb state.builder);

      position_at_end end_bb state.builder;
      env
  | Break ->
      (match loop_ctx with
      | Some ctx -> ignore (build_br ctx.end_bb state.builder)
      | None -> ());
      env
  | Continue ->
      (match loop_ctx with
      | Some ctx -> ignore (build_br ctx.cond_bb state.builder)
      | None -> ());
      env
  | ErrorStmt -> failwith "cannot codegen ErrorStmt"

let codegen_program (prog : Ast.program) =
  let state = init "main" in
  let i32_t = i32_type state.context in
  let i64_t = i64_type state.context in
  let main_t = function_type i64_t [||] in
  let main_f = declare_function "__sysploc_main" main_t state.the_module in
  let entry = append_block state.context "entry" main_f in
  position_at_end entry state.builder;
  let empty_env = Env.empty in
  let _final_env =
    List.fold_left (fun e s -> codegen_stmt state e None s) empty_env prog
  in
  let cur_block = insertion_block state.builder in
  if block_terminator cur_block = None then
    ignore (build_ret (const_int i32_t 0) state.builder);
  let c_main_t = function_type i32_t [||] in
  let c_main_f = declare_function "main" c_main_t state.the_module in
  let c_main_entry = append_block state.context "entry" c_main_f in
  position_at_end c_main_entry state.builder;
  let user_ret_val = build_call main_t main_f [||] "user_ret" state.builder in
  let trunc_val = build_trunc user_ret_val i32_t "exit_code" state.builder in
  ignore (build_ret trunc_val state.builder);
  state.the_module
