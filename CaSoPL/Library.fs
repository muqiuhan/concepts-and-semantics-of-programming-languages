namespace CaSoPL

module Lang1 =
    type X = string
    and V = int
    and Binding = X * V
    and Environment = List<Binding>
    and Domain = List<X>

    let rec Env: Environment -> X -> Option<V> =
        fun env x ->
            match env with
            | [] -> None
            | (x1, v1) :: t -> if x = x1 then Some v1 else Env t x

    let rec Dom: Environment -> Option<Domain> =
        fun env ->
            match env with
            | [] -> None
            | (x, _) :: t -> Option.bind (fun dom -> Some(x :: dom)) (Dom t)

    let AddBinding: Environment -> Binding -> Environment =
        fun env binding -> binding :: env
