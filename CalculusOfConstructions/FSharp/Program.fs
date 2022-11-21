namespace CalculusOfConstructions

(*
    References: 
    - https://en.wikipedia.org/wiki/Calculus_of_constructions
*)

module AST =

    type Variable = Variable of {| Label: string; Order : int |}
        with
            member this.refresh(): Variable = 
                match this with
                | Variable this -> Variable {| this with Order = this.Order + 1 |}

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
            | Variable ((Variable.Variable recordVar) as var) ->
                List.tryFind (fst >> ((=) var)) env
                |> function
                    | Some(_, term) -> term
                    | None -> failwith $"Unbound variable {recordVar.Label}"
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

        member this.normalization(context: Environment) =
            let normalizeAbstraction (abs: Abstraction): Abstraction =
                let annotation = abs.Annotation.normalization(context) in
                {| abs with Variable = abs.Variable; Annotation = annotation; Body = abs.Body.normalization((abs.Variable, annotation)::context) |}

            match this with
            | Integer _ 
            | Unit
            | TypeInteger
            | TypeUnit
            | Proposition
            | Universe -> this
            | Variable var ->
                List.tryFind (fst >> ((=) var)) context
                |> Option.map snd
                |> Option.defaultValue (failwith "")
            | Application (abs, arg) ->
                let arg' = arg.normalization(context) in
                match abs.normalization(context) with
                | Lambda abs -> 
                    let varInArg = abs.Body.substitution([(abs.Variable, arg')]) in
                    varInArg.normalization(context)
                | expr -> Application (expr, arg')
            | Lambda abs -> 
                Lambda (normalizeAbstraction abs)
            | Forall abs -> 
                Forall (normalizeAbstraction abs)

        member this.substitution (bindings: Environment) : Term = 
            let substituteAbstraction (abs: Abstraction) = 
                let x' = abs.Variable.refresh() in
                {| abs with 
                    Variable = x'; 
                    Annotation = abs.Annotation.substitution(bindings);
                    Body = abs.Body.substitution((abs.Variable, Variable x')::bindings) |}

            match this with
            | Integer _ 
            | Unit
            | TypeInteger
            | TypeUnit
            | Proposition
            | Universe -> this
            | Variable var ->
                List.tryFind (fst >> ((=) var)) bindings
                |> Option.map snd
                |> Option.defaultValue this
            | Application (abs, arg) ->
                Application (abs.substitution(bindings), arg.substitution(bindings))
            | Lambda abs -> 
                Lambda (substituteAbstraction abs)
            | Forall abs -> 
                Forall (substituteAbstraction abs)

module EntryPoint =

    [<EntryPointAttribute>]
    let main _ = 0
