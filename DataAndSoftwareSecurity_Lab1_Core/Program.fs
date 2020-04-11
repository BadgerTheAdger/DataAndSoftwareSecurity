open System
      
[<EntryPoint>]
let main argv = 
    Formatting.textToBinary("а")
    |> ignore
    0 // return an integer exit code    