open Llvm
module Env = Map.Make (String)

type env = llvalue Env.t
type t = { context : llcontext; the_module : llmodule; builder : llbuilder }

let init module_name =
  let context = global_context () in
  let the_module = create_module context module_name in
  let builder = builder context in
  { context; the_module; builder }

let rec codegen_expr state env (expr : Ast.expr) =
  let i64_t = i64_type state.context in
  match expr.node with
  | IntLit n -> const_of_int64 i64_t n true
  | Id name -> (
      match Env.find_opt name env with
      | Some p -> build_load i64_t p name state.builder
      | None -> failwith ("undefined variable " ^ name))
  | UnaryMinus e ->
      let e_val = codegen_expr state env e in
      build_neg e_val "" state.builder
  | BinOp (lhs, op, rhs) -> (
      let lhs_val = codegen_expr state env lhs in
      let rhs_val = codegen_expr state env rhs in
      match op with
      | Add -> build_add lhs_val rhs_val "" state.builder
      | Sub -> build_sub lhs_val rhs_val "" state.builder
      | Mul -> build_mul lhs_val rhs_val "" state.builder
      | Div -> build_sdiv lhs_val rhs_val "" state.builder)
  | ErrorExpr -> failwith "cannot codegen ErrorExpr"

let rec codegen_stmt state env (stmt : Ast.statement) =
  let int_t = i64_type state.context in
  match stmt.node with
  | Decl (id, init_expr, _decltype) ->
      let var_name = id.node in
      let init_val = codegen_expr state env init_expr in
      let alloca = build_alloca int_t var_name state.builder in
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
  let _final_env = List.fold_left (codegen_stmt state) empty_env prog in
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
