open System.Drawing
open System

(*
    8-1 A simple class
    Make a class that takes three byte values caller r, g, and b, and providers a byte property called Level,
    which contains a grayscale value calculated from the incoming red, green, and blue values.
*)

type GrayScale (r: byte, g: byte, b: byte) =

    let level = (int r + int g + int b) / 3 |> byte // casting to int to avoid byte overflow
    let eq (that: GrayScale) = level = that.Level

    member __.Level = level
(*
    8-2 Secondary constructors
    Add a secondary constructor for the GrayScale class from Exercise 9-1. It should take a System.Drawing.Color instance 
    and construct a GrayScale instance from the color's R, G, and B properties.
*)
    new (color: System.Drawing.Color) =
        GrayScale(color.R, color.G, color.B)

(*
    8-3 Overrides
    Override the ToString() method of GrayScale so that it produces output like this, 
    where the number is the Level value: Grayscale(140)
*)

    override __.ToString() =
        sprintf "GrayScale(%i)" __.Level 

(*
    8-4 Equality
    Implement equality for the GrayScale class by overriding GetHashCode() and Equals(), and implementing the generic 
    version of IEquatable. The GrayScale class should not be nullable (don't add the [<AllowNullLiteral>] attrible).

*)
    override __.GetHashCode() = 
        hash __.Level

    override __.Equals other =
        match other with
        | :? GrayScale as that -> eq that
        | _  -> false

    interface IEquatable<GrayScale> with
        member __.Equals(that: GrayScale) = eq that
(*
    Prove that GrayScale(Color.Orange) is equal to GrayScale(0xFFuy, 0xA5uy, 0x00uy).
*)
let x = GrayScale(Color.Orange) = GrayScale(0xFFuy, 0xA5uy, 0x00uy)
(*
  Prove that GrayScale(Color.Orange) is not equal to GrayScale(Color.Blue).
*)
let y = GrayScale(Color.Orange) = GrayScale(Color.Blue)
(*
    What happens if you check equality for Grey Scale(0xFFuy, 0xA5uy, 0xx0uy) and GreyScale(0xFFuy, 0xA5uy, 0x01uy). Why is this?
*)
let z = GrayScale(0xFFuy, 0xA5uy, 0x00uy) = GrayScale(0xFFuy, 0xA5uy, 0x01uy)

// this is why!
let fst = (int 0xFFuy+ int 0xA5uy+ int 0x00uy) / 3 |> byte
let snd = (int 0xFFuy+ int 0xA5uy + int 0x01uy) / 3 |> byte

let result = fst = snd

let demo() =
    GrayScale(Color.Brown) |> printfn "%A"

demo()