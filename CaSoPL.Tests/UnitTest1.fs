module CaSoPL.Tests

open NUnit.Framework

module TestLang1 =
    open CaSoPL.Lang1

    [<SetUp>]
    let Setup () = ()


    [<Test>]
    let Env () =
        let env: Environment = [ ("x1", 1); ("x2", 2); ("x3", 3) ] in

        Option.bind
            (fun v2 ->
                Assert.AreEqual(v2, 2)
                None)
            (Env env "x2")
        |> ignore

    [<Test>]
    let Dom () =
        let env: Environment = [ ("x1", 1); ("x2", 2); ("x3", 3) ] in

        Option.bind
            (fun domain ->
                Assert.AreEqual(domain, [ "x1"; "x2"; "x3" ])
                None)
            (Dom env)
        |> ignore

    [<Test>]
    let AddBinding () =
        let env: Environment = [ ("x1", 1); ("x2", 2); ("x3", 3) ] in
        let newEnv: Environment = [ ("x_new", 0); ("x1", 1); ("x2", 2); ("x3", 3) ]

        Assert.AreEqual(newEnv, AddBinding env ("x_new", 0))
