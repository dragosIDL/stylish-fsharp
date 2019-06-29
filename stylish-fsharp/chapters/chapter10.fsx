(*
    10-1 Making some code asynchronous
    In the following code the Server module contains a simulater server endpoint that returns a random string, 
    taking half a second to do so. In the COnsumer module, we call the server multiple times to build up 
    an array of strings, which we then sort to produce a final result.
*)

open System

module Random = 
    let private random = System.Random()
    let string() = 
        let len = random.Next(0, 10)
        Array.init len (fun _ -> random.Next(0, 255) |> char)
        |> String

module Server = 
    let AsyncGetString (id) =
        async {
            do! Async.Sleep 500
            return Random.string()
        }

module Consumer =
    let GetData (count: int) =
        let strings = 
            Array.init count (fun i ->
                Server.AsyncGetString i 
                |> Async.RunSynchronously)
        strings |> Array.sort

    let GetDataParallel (count: int) =
        async {
            let! strings = 
                Array.init count Server.AsyncGetString
                |> Async.Parallel
            return strings |> Array.sort
        }

(*
    10-2 Returning Tasks
    How woufld your solution to Exercise 10-1 change if COnsumer.GetData() needed to return a c# style task
*)

    let GetDataTask (count: int) =
        async {
            let! strings = 
                Array.init count Server.AsyncGetString
                |> Async.Parallel
            return strings |> Array.sort
        } |> Async.StartAsTask


let demo () = 
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()

    Consumer.GetData 10 |> Array.iter (printfn "%s")

    printfn "That took %ims" sw.ElapsedMilliseconds

    sw.Restart()

    Consumer.GetDataParallel 10  
    |> Async.RunSynchronously 
    |> Array.iter (printfn "%s")

    printfn "That took %ims" sw.ElapsedMilliseconds

demo()