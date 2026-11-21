type sema_error = Parser.error

module Env = Map.Make (String)

type env = Ast.decltype Env.t

let rec pass_expr env expr =
  match expr.Ast.node with
  | Ast.IntLit _ | Ast.BoolLit _ | Ast.ErrorExpr -> []
  | Ast.Id name -> (
      match Env.find_opt name env with
      | Some _ -> []
      | None ->
          [ { Parser.pos = expr.pos; msg = "Undeclared identifier: " ^ name } ])
  | Ast.UnaryMinus e | Ast.UnaryNot e -> pass_expr env e
  | Ast.BinOp (lhs, _, rhs) -> pass_expr env lhs @ pass_expr env rhs

let pass_stmt env stmts =
  let rec walk stmts env loops errors =
    match stmts with
    | [] -> errors
    | (stmt : Ast.statement) :: rest -> (
        match stmt.node with
        | Ast.Decl (name, expr, decl_type) ->
            let expr_errs = pass_expr env expr in
            let decl_errs =
              if Env.mem name.node env then
                [
                  {
                    Parser.pos = stmt.pos;
                    msg = "Variable redefinition: " ^ name.node;
                  };
                ]
              else []
            in
            let next_env = Env.add name.node decl_type env in
            walk rest next_env loops (errors @ expr_errs @ decl_errs)
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
            walk rest env loops (errors @ expr_errs @ assign_errs)
        | Ast.Ret expr | Ast.Expr expr ->
            walk rest env loops (errors @ pass_expr env expr)
        | Ast.Block body ->
            let block_errs = walk body env loops [] in
            walk rest env loops (errors @ block_errs)
        | Ast.If (cond, then_bod, else_bod) ->
            let cond_errs = pass_expr env cond in
            let then_errs = walk [ then_bod ] env loops [] in
            let else_errs =
              match else_bod with
              | Some e -> walk [ e ] env loops []
              | None -> []
            in
            walk rest env loops (errors @ cond_errs @ then_errs @ else_errs)
        | Ast.While (cond, body) ->
            let cond_errs = pass_expr env cond in
            let body_errs = walk [ body ] env (loops + 1) [] in
            walk rest env loops (errors @ cond_errs @ body_errs)
        | Ast.Break ->
            let errs =
              if loops > 0 then []
              else [ { Parser.pos = stmt.pos; msg = "Break outside of loop" } ]
            in
            walk rest env loops (errors @ errs)
        | Ast.Continue ->
            let errs =
              if loops > 0 then []
              else
                [ { Parser.pos = stmt.pos; msg = "Continue outside of loop" } ]
            in
            walk rest env loops (errors @ errs)
        | Ast.ErrorStmt -> walk rest env loops errors)
  in
  walk stmts env 0 []

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
