module Formatting

open System

let padAtStart n = Seq.append (Seq.replicate n '0')

let ensureLength length seq =
    let diff = length - (Seq.length seq) in
        if diff = 0 then seq
        else padAtStart diff seq            

let hexToB hex = Convert.ToString(System.Uri.FromHex(hex),2) |> ensureLength 4

let hexToBinary hexkey =
    hexkey 
    |> Seq.map (fun i -> hexToB i)
    |> Seq.concat

let charSize = 16

let charToBinaryString c = 
    Convert.ToString(int c, 2)
    |> ensureLength charSize 

let textToBinary (msg:seq<char>) = 
    msg 
    |> Seq.map charToBinaryString    
    |> Seq.concat

let binaryToText a =
    if Seq.length a % charSize <> 0 then raise (new ArgumentException( (Seq.length >> string) a ))
    
    Seq.chunkBySize charSize a
    |> Seq.map (fun chunk -> Convert.ToInt32(chunk |> String, 2) |> Convert.ToChar)