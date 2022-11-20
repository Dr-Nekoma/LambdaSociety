﻿namespace CalculusOfConstructions

(*
    References: 
    - https://en.wikipedia.org/wiki/Calculus_of_constructions
*)

module AST =

    type Variable = {| Label: string |}

    type Environment = (Variable * Term) list

    and Abstraction =
        {| Variable: Variable
           Annotation: Term
           Body: Term |}

    and Term =
        | Variable of Variable
        | Application of Abstraction: Term * Argument: Term
        | Lambda of Abstraction
        | Forall of Abstraction
        | Integer of Value: int
        | TypeInteger
        | Unit
        | TypeUnit
        | Proposition
        | Universe

        member this.typeCheck(env: Environment) : Term =
            match this with
            | Integer _ -> TypeInteger
            | Unit -> TypeUnit
            | TypeInteger
            | TypeUnit -> Proposition
            | Variable var ->
                List.tryFind (fst >> ((=) var)) env
                |> function
                    | Some(_, term) -> term
                    | None -> failwith $"Unbound variable {var.Label}"
            | Application(abs, arg) ->
                match abs.typeCheck (env) with
                | Forall rule ->
                    // maybe we will need to use .Equals instead of "="
                    if arg.typeCheck (env) = rule.Annotation then
                        rule.Body.substitution rule.Variable arg
                    else
                        failwith ""
                | _ -> failwith ""
            | Lambda abs ->
                abs.Annotation.typeCheck (env)
                |> function
                    | Proposition
                    | Universe ->
                        let newContext: Environment = (abs.Variable, abs.Annotation) :: env in
                        let bodyType = abs.Body.typeCheck (newContext)
                        Forall {| abs with Body = bodyType |}
                    | _ -> failwith ""
            | Forall abs ->
                abs.Annotation.typeCheck (env)
                |> function
                    | Proposition
                    | Universe ->
                        let newContext: Environment = (abs.Variable, abs.Annotation) :: env in
                        match abs.Body.typeCheck (newContext) with
                        | (Proposition | Universe) as term -> term
                        | _ -> failwith ""
                    | _ -> failwith ""
            | Proposition -> Universe
            | Universe -> failwith ""

        member this.equality(target: Term): bool = failwith ""

        member this.normalization() = failwith ""

        member this.substitution (variable: Variable) (argument: Term) : Term = failwith ""


module EntryPoint =

    [<EntryPointAttribute>]
    let main _ = 0