type sema_error = Parser.error

module Env = Map.Make (String)

type env = Ast.decltype Env.t

let rec pass_expr env expr =
  match expr.Ast.node with
  | Ast.IntLit _ | Ast.ErrorExpr -> []
  | Ast.Id name -> (
      match Env.find_opt name env with
      | Some _ -> []
      | None ->
          [ { Parser.pos = expr.pos; msg = "Undeclared identifier: " ^ name } ])
  | Ast.UnaryMinus e -> pass_expr env e
  | Ast.BinOp (lhs, _, rhs) -> pass_expr env lhs @ pass_expr env rhs

let pass_stmt env stmts =
  let rec walk stmts env errors =
    match stmts with
    | [] -> errors
    | (stmt : Ast.statement) :: rest -> (
        match stmt.node with
        | Ast.Decl (name, expr, decl_type) ->
            let expr_errs = pass_expr env expr in
            let next_env = Env.add name.node decl_type env in
            walk rest next_env (errors @ expr_errs)
        | Ast.Assignment (name, expr) ->
            let expr_errs = pass_expr env expr in
            let assign_errs =
              match Env.find_opt name.node env with
              | None ->
                  [
                    {
                      Parser.pos = stmt.pos;
                      msg = "Assignment to undeclared variable: " ^ name.node;
                    };
                  ]
              | Some Ast.Const ->
                  [
                    {
                      Parser.pos = stmt.pos;
                      msg = "Assignment to immutable variable: " ^ name.node;
                    };
                  ]
              | Some Ast.Mut -> []
            in
            walk rest env (errors @ expr_errs @ assign_errs)
        | Ast.Ret expr | Ast.Expr expr ->
            let expr_errors = pass_expr env expr in
            walk rest env (errors @ expr_errors)
        | Ast.ErrorStmt -> walk rest env errors)
  in
  walk stmts env []

let pass_at_least_one_statement = function
  | [] ->
      [
        {
          Parser.pos = Parser.eof_loc;
          msg = "Program must contain at least one statement";
        };
      ]
  | _ -> []

let rec pass_implicit_main_return program =
  let rec get_last = function
    | [] -> None
    | [ stmt ] -> Some stmt
    | _ :: rest -> get_last rest
  in
  match get_last program with
  | Some Ast.{ node = Ast.Ret _; _ } -> []
  | Some _ ->
      [
        {
          Parser.pos = Parser.eof_loc;
          msg = "Last statement of a program must be a return!";
        };
      ]
  | None -> []

let sema program =
  let structure_errs =
    pass_at_least_one_statement program @ pass_implicit_main_return program
  in
  let logic_errs = pass_stmt Env.empty program in
  structure_errs @ logic_errs
