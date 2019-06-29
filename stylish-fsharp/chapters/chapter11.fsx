(*
    11-1 Reproducing maperror
    Can you implement a passThroughRejects function, with the same behavior as the build-in mapError function?
*)

type Outcome<'TSuccess, 'TFailure> = 
    | Success of 'TSuccess
    | Failure of 'TFailure

let adapt func = function
| Success x -> func x
| Failure f -> Failure f

let passThrough func = function
| Success x -> func x |> Success
| Failure x -> Failure x

let passThroughRejects func = function
| Success x -> Success x 
| Failure x -> func x |> Failure 

(*
    11-2 Writing a ROP pipeline
    You are working on a project to handle some incoming messages, each containing a filename and some data. 
    The filename is a string representation of a DateTimeOffset when the data was captured. The data is an array of
    floating-point values. The process should attempt to parse the filename as a DateTimeOffset (some might fail due
    to spurious messages), and should also reject any messages where the data array contains any NaN ("not-a-number") 
    values. Any rejects need to be logged.
*)

open System
open Result

type Message = { 
    FileName : string
    Content : float[]
}

type Reading = {
    TimeStamp : DateTimeOffset
    Data : float[]
}

let example = [|
    { FileName = "2019-02-23T02:00:00-05:00"
      Content = [|1.;2.;3.;4.|]}
    { FileName = "2019-02-23T02:00:10-05:00"
      Content = [|5.;6.;7.;8.|]}
    { FileName = ""; Content = [||]}
    { FileName = "2019-02-23T02:00:10-05:00" 
      Content = [|1.;2.;Double.NaN|]}
|]

let log = printfn "Logging: %s"

type MessageError = 
    | InvalidFileName of fileName: string
    | DataContainsNaN of fileName: string * index: int

let getReading message = 
    match DateTimeOffset.TryParse message.FileName with
    | true, dt ->
        let reading = { TimeStamp = dt; Data = message.Content }
        Ok (message.FileName, reading)
    | false, _ ->
        Error (InvalidFileName message.FileName)

let validateData (fileName, reading) = 
    let nanIndex = reading.Data |> Array.tryFindIndex Double.IsNaN
    match nanIndex with 
    | Some i -> Error (DataContainsNaN (fileName, i))
    | None -> Ok reading

let logError = function 
| InvalidFileName f -> log (sprintf "invalid filename: %s" f)
| DataContainsNaN (f, index) -> log (sprintf "found NaN in %s at position %i" f index)


open Result

let processMessage = 
    getReading 
    >> bind validateData 
    >> mapError logError

let processData data = 
    data 
    |> Array.map processMessage
    |> Array.choose (fun result ->
        match result with 
        | Ok reading -> reading |> Some
        | Error _ -> None)

let demo () = 
    example 
    |> processData
    |> Array.iter (printfn "%A")    

demo()