(*
    7-1 Records and performance

    You need to store several million items, each consisting of X, Y, and Z positions (single precision) and 
    a DateTime instance. For performance reasons, you want to store them on the stack. 
    How might you model this using an F# record?
    How can you prove, in the simple case, that instantiating a million recods works faster when the items
    are placed on the stack than when they are allowed to go on the heap?
*)
open System

[<Struct>]
type Point = {
    X: float32
    Y: float32
    Z: float32
    Time: DateTime }

#time

let test = 
    Array.init 1_000_000 (fun i ->
        { X = float32 i
          Y = float32 i
          Z = float32 i
          Time = DateTime.UtcNow})

#time "off"

(*
    7.4 Modifying records
    Start off with the struct record from Exercise 7-1. write a function called translate that takes a Position record 
    and produces a new instance with the X, Y, and Z positions altered by specified amounts, but the Time value unchanged.
*)

let translate (x, y, z) p =
    { p with 
        X = p.X + x
        Y = p.Y + y
        Z = p.Z + z }