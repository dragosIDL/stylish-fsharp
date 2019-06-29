open System.Text.RegularExpressions
open System.IO

(*
    13-1 Making code readable
    The following working code searches for files below a certain path, and returns those files whose names match 
    a regular expression, and which have the ReadOnly attribute set.
*)

let find pattern dir = 
    
    let re = Regex(pattern)
    Directory.EnumerateFiles (dir, "*.*", SearchOption.AllDirectories)
    |> Seq.filter (re.IsMatch << Path.GetFileName)
    |> Seq.map FileInfo
    |> Seq.filter (fun fi -> fi.Attributes.HasFlag FileAttributes.ReadOnly)
    |> Seq.map (fun fi -> fi.Name)

(*
    Make it more readable
*)

module FileSearch =

    module private FileName =
        let isMatch pattern = 
            let re = Regex(pattern)
            re.IsMatch << Path.GetFileName
    
    module private FileAttributes =
        let hasFlag flag filePath = 
            (FileInfo filePath).Attributes.HasFlag flag

    let findReadonly pattern dir = 
        Directory.EnumerateFiles(dir, "*,*", SearchOption.AllDirectories)
        |> Seq.filter (FileName.isMatch pattern)
        |> Seq.filter (FileAttributes.hasFlag FileAttributes.ReadOnly)