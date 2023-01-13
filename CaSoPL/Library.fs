namespace CaSoPL

module Lang1 =
    type X = string
    and R = int
    and V = int

    and EnvironmentBinding = X * R
    and Environment = EnvironmentBinding list
    and EnvironmentDomain = X list

    and MemoryBinding = R * V
    and Memory = MemoryBinding list
    and MemoryDomain = R list

    /// Env(x)
    let rec Env: Environment -> X -> R option =
        fun env x ->
            match env with
            | [] -> None
            | (x1, r1) :: t -> if x = x1 then Some r1 else Env t x

    /// EnvDom(Env_n)
    let rec EnvDom: Environment -> EnvironmentDomain option =
        fun env ->
            match env with
            | [] -> None
            | (x, _) :: t -> Option.bind (fun dom -> Some(x :: dom)) (EnvDom t)

    /// (x, r) ⊕ Env_n
    let AddEnvBinding: Environment -> EnvironmentBinding -> Environment =
        fun env binding -> binding :: env

    /// Mem(r)
    let rec Mem: Memory -> R -> V option =
        fun mem r ->
            match mem with
            | [] -> None
            | (r1, v1) :: t -> if r = r1 then Some v1 else Mem t r

    /// MemDom(Mem_n)
    let rec MemDom: Memory -> MemoryDomain option =
        fun mem ->
            match mem with
            | [] -> None
            | (r, _) :: t -> Option.bind (fun dom -> Some(r :: dom)) (MemDom t)

    /// Mem[r := v]
    let rec AddMemBinding: Memory -> MemoryBinding -> Memory =
        fun mem (r, v) ->
            match mem with
            | [] -> [ (r, v) ]
            | (r1, v1) :: t ->
                if r = r1 then
                    (r1, v) :: t
                else
                    (r1, v1) :: (AddMemBinding t (r, v))
