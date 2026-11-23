type sema_error = Parser.error

module Env = Map.Make (String)
module StrSet = Set.Make (String)

type func_info = { arity : int; is_extern : bool }
type func_env = func_info Env.t
type var_env = Ast.decltype Env.t

let check_duplicate_params (params : Ast.param list) =
  let _, errs =
    List.fold_left
      (fun (seen, errs) (p : Ast.param) ->
        if StrSet.mem p.node seen then
          ( seen,
            errs
            @ [
                {
                  Parser.pos = p.pos;
                  msg = "Duplicate parameter name: " ^ p.node;
                };
              ] )
        else (StrSet.add p.node seen, errs))
      (StrSet.empty, []) params
  in
  errs

let collect_funcs (program : Ast.program) =
  List.fold_left
    (fun (fenv, errs) (decl : Ast.top_decl) ->
      match decl.node with
      | Ast.ExternDecl (name, params) | Ast.FuncDecl (name, params, _) ->
          let is_extern =
            match decl.node with Ast.ExternDecl _ -> true | _ -> false
          in
          let param_errs = check_duplicate_params params in
          let dup_errs, next_fenv =
            if Env.mem name.node fenv then
              ( [
                  {
                    Parser.pos = name.pos;
                    msg = "Function redefinition: " ^ name.node;
                  };
                ],
                fenv )
            else
              ( [],
                Env.add name.node { arity = List.length params; is_extern } fenv
              )
          in
          (next_fenv, errs @ dup_errs @ param_errs)
      | Ast.ErrorDecl -> (fenv, errs))
    (Env.empty, []) program

let check_main fenv =
  match Env.find_opt "main" fenv with
  | None ->
      [
        {
          Parser.pos = Parser.eof_loc;
          msg = "Missing required 'main' function";
        };
      ]
  | Some { is_extern = true; _ } ->
      [
        {
          Parser.pos = Parser.eof_loc;
          msg = "'main' must be defined, not extern";
        };
      ]
  | Some { arity; is_extern = false } when arity <> 0 ->
      [
        {
          Parser.pos = Parser.eof_loc;
          msg = "'main' function must take no parameters";
        };
      ]
  | Some _ -> []

let rec pass_expr fenv env (expr : Ast.expr) =
  match expr.Ast.node with
  | Ast.IntLit _ | Ast.BoolLit _ | Ast.ErrorExpr -> []
  | Ast.Id name -> (
      match Env.find_opt name env with
      | Some _ -> []
      | None ->
          [ { Parser.pos = expr.pos; msg = "Undeclared identifier: " ^ name } ])
  | Ast.UnaryMinus e | Ast.UnaryNot e -> pass_expr fenv env e
  | Ast.BinOp (lhs, _, rhs) -> pass_expr fenv env lhs @ pass_expr fenv env rhs
  | Ast.Call (callee, args) ->
      let args_errs = List.concat_map (pass_expr fenv env) args in
      let callee_errs =
        match callee.node with
        | Ast.Id fn_name -> (
            match Env.find_opt fn_name fenv with
            | None ->
                [
                  {
                    Parser.pos = callee.pos;
                    msg = "Call to undeclared function: " ^ fn_name;
                  };
                ]
            | Some { arity; _ } ->
                let got = List.length args in
                if got <> arity then
                  [
                    {
                      Parser.pos = expr.pos;
                      msg =
                        Format.sprintf
                          "Function '%s' expects %d argument(s), got %d" fn_name
                          arity got;
                    };
                  ]
                else [])
        | Ast.ErrorExpr -> []
        | _ ->
            pass_expr fenv env callee
            @ [
                { Parser.pos = callee.pos; msg = "Expression is not callable" };
              ]
      in
      callee_errs @ args_errs

let pass_stmt fenv env stmts =
  let rec walk stmts env loops errors =
    match stmts with
    | [] -> errors
    | (stmt : Ast.statement) :: rest -> (
        match stmt.node with
        | Ast.Decl (name, expr, decl_type) ->
            let expr_errs = pass_expr fenv env expr in
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
            let expr_errs = pass_expr fenv env expr in
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
            walk rest env loops (errors @ pass_expr fenv env expr)
        | Ast.Block body ->
            let block_errs = walk body env loops [] in
            walk rest env loops (errors @ block_errs)
        | Ast.If (cond, then_bod, else_bod) ->
            let cond_errs = pass_expr fenv env cond in
            let then_errs = walk [ then_bod ] env loops [] in
            let else_errs =
              match else_bod with
              | Some e -> walk [ e ] env loops []
              | None -> []
            in
            walk rest env loops (errors @ cond_errs @ then_errs @ else_errs)
        | Ast.While (cond, body) ->
            let cond_errs = pass_expr fenv env cond in
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

let rec always_returns (stmt : Ast.statement) =
  match stmt.node with
  | Ast.Ret _ -> true
  | Ast.Block stmts -> block_always_returns stmts
  | Ast.If (_, then_bod, Some else_bod) ->
      always_returns then_bod && always_returns else_bod
  | Ast.If (_, _, None) -> false
  | Ast.While _ -> false
  | Ast.Decl _ | Ast.Assignment _ | Ast.Expr _ | Ast.Break | Ast.Continue
  | Ast.ErrorStmt ->
      false

and block_always_returns = function
  | [] -> false
  | stmt :: rest -> (
      if always_returns stmt then true
      else
        match stmt.node with
        | Ast.Break | Ast.Continue -> false
        | _ -> block_always_returns rest)

let init_param_env (params : Ast.param list) : var_env =
  List.fold_left
    (fun env (p : Ast.param) -> Env.add p.node Ast.Mut env)
    Env.empty params

let check_decl fenv (decl : Ast.top_decl) =
  match decl.node with
  | Ast.ExternDecl _ | Ast.ErrorDecl -> []
  | Ast.FuncDecl (name, params, body) ->
      let var_env = init_param_env params in
      let body_errs = pass_stmt fenv var_env [ body ] in
      let ret_errs =
        if always_returns body then []
        else
          [
            {
              Parser.pos = decl.pos;
              msg =
                "Function '" ^ name.node
                ^ "' does not return on all control-flow paths";
            };
          ]
      in
      body_errs @ ret_errs

let sema (program : Ast.program) =
  let fenv, decl_errs = collect_funcs program in
  let main_errs = check_main fenv in
  let body_errs = List.concat_map (check_decl fenv) program in
  decl_errs @ main_errs @ body_errs
