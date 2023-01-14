module CaSoPL.Tests

open NUnit.Framework
open CaSoPL

module TestLang1 =
    open Lang1

    [<SetUp>]
    let Setup () = ()

    [<Test>]
    let Test_Env () =
        let env: Environment = [ ("x1", 1); ("x2", 2); ("x3", 3) ] in

        Option.bind
            (fun r2 ->
                Assert.AreEqual(r2, 2)
                None)
            (Env env "x2")
        |> ignore

    [<Test>]
    let Test_EnvDom () =
        let env: Environment = [ ("x1", 1); ("x2", 2); ("x3", 3) ] in

        Option.bind
            (fun domain ->
                Assert.AreEqual(domain, [ "x1"; "x2"; "x3" ])
                None)
            (EnvDom env)
        |> ignore

    [<Test>]
    let Test_AddEnvBinding () =
        let env: Environment = [ ("x1", 1); ("x2", 2); ("x3", 3) ] in
        let newEnv: Environment = [ ("x_new", 0); ("x1", 1); ("x2", 2); ("x3", 3) ]

        Assert.AreEqual(newEnv, AddEnvBinding env ("x_new", 0))

    [<Test>]
    let Test_Mem () =
        let mem: Memory = [ (0, 0); (1, 1); (2, 2); (3, 3) ] in

        Option.bind
            (fun v2 ->
                Assert.AreEqual(v2, 2)
                None)
            (Mem mem 2)
        |> ignore

    [<Test>]
    let Test_MemDom () =
        let mem: Memory = [ (0, 0); (1, 1); (2, 2); (3, 3) ] in

        Option.bind
            (fun domain ->
                Assert.AreEqual(domain, [ 0; 1; 2; 3 ])
                None)
            (MemDom mem)
        |> ignore

    [<Test>]
    let Test_AddMemBinding () =
        let mem: Memory = [ (0, 0); (1, 1); (2, 2); (3, 3) ] in
        let newMem: Memory = [ (0, 0); (1, 10); (2, 2); (3, 3) ] in
        Assert.AreEqual(newMem, AddMemBinding mem (1, 10))

        let mem: Memory = [ (0, 0); (2, 2); (3, 3) ] in
        let newMem: Memory = [ (0, 0); (2, 2); (3, 3); (1, 10) ] in
        Assert.AreEqual(newMem, AddMemBinding mem (1, 10))
