// Learn more about F# at http://fsharp.org

open System

let seqToString = Seq.toArray >> String 

let padTimes n = Seq.append (Seq.replicate n '0')

let ensureLength length seq = 
    match length - Seq.length seq with
    | 0 -> seq
    | a -> padTimes a seq  
    
let hexToB hex = Convert.ToString(System.Uri.FromHex(hex),2) |> ensureLength 4 |> seqToString

let toBinary hexkey =
    hexkey 
    |> Seq.map (fun i -> hexToB i)
    |> Seq.concat |> seqToString
                 
let permute table (key:string) =
    table
    |> Seq.map (fun i -> key.[i - 1])
    |> Seq.toArray |> String
        
let PC1 =  [57;49;41;33;25;17;09;
           01;58;50;42;34;26;18;
           10;02;59;51;43;35;27;
           19;11;03;60;52;44;36;
           63;55;47;39;31;23;15;
           07;62;54;46;38;30;22;
           14;06;61;53;45;37;29;
           21;13;05;28;20;12;04]
           
let permutePC1 = permute PC1

let halfOf f (key:string) = f (key.Length / 2) key |> seqToString
let leftOf = halfOf Seq.take
let rightOf = halfOf Seq.skip
 
let inline shiftToLeft times seq =
    Seq.append (Seq.skip times seq) (Seq.take times seq)

let inline leftShiftingIteration (key:seq<char>) = 
    [1;1;2;2;2;2;2;2;1;2;2;2;2;2;2;1]
    |> Seq.scan (fun acc elem1 -> shiftToLeft elem1 acc) key
    |> Seq.tail 
    |> Seq.map seqToString |> Seq.toArray

let PC2 = [14;17;11;24;01;05
           03;28;15;06;21;10
           23;19;12;04;26;08
           16;07;27;20;13;02
           41;52;31;37;47;55
           30;40;51;45;33;48
           44;49;39;56;34;53
           46;42;50;36;29;32]
    
let permutePC2 = permute PC2

let charToBinaryString c = 
    Convert.ToString(int c, 2)
    |> ensureLength 8 

let textToBinary (msg:string) = 
    msg 
    |> Seq.map charToBinaryString    
    |> Seq.concat |> seqToString

let CRLN = "0000110100001010"

let padded binary = 
    match String.length binary % 64 with
    | 0 -> binary
    | _ -> padTimes ((binary + CRLN).Length % 64) (binary + CRLN) |> seqToString

let IP = [58;50;42;34;26;18;10;02;
          60;52;44;36;28;20;12;04;
          62;54;46;38;30;22;14;06;
          64;56;48;40;32;24;16;08;
          57;49;41;33;25;17;09;01;
          59;51;43;35;27;19;11;03;
          61;53;45;37;29;21;13;05;
          63;55;47;39;31;23;15;07]

let permuteIP = permute IP

let EBit = [32;01;02;03;04;05;
            04;05;06;07;08;09;
            08;09;10;11;12;13;
            12;13;14;15;16;17;
            16;17;18;19;20;21;
            20;21;22;23;24;25;
            24;25;26;27;28;29;
            28;29;30;31;32;01]

let expand = permute EBit

let xor a b =
    Seq.zip a b 
    |> Seq.map (fun (x,y) -> if x <> y then '1' else '0')
    |> Seq.toArray |> String
    
let S1 = [
    [14;4;13;1;2;15;11;8;3;10;6;12;5;9;0;7];
    [0;15;7;4;14;2;13;1;10;6;12;11;9;5;3;8];
    [4;1;14;8;13;6;2;11;15;12;9;7;3;10;5;0];
    [15;12;8;2;4;9;1;7;5;11;3;14;10;0;6;13];
]

let S2 = [
    [15;1;8;14;6;11;3;4;9;7;2;13;12;0;5;10];
    [3;13;4;7;15;2;8;14;12;0;1;10;6;9;11;5];
    [0;14;7;11;10;4;13;1;5;8;12;6;9;3;2;15];
    [13;8;10;1;3;15;4;2;11;6;7;12;0;5;14;9];
]

let S3 = [
   [10;0;9;14;6;3;15;5;1;13;12;7;11;4;2;8];
   [13;7;0;9;3;4;6;10;2;8;5;14;12;11;15;1];
   [13;6;4;9;8;15;3;0;11;1;2;12;5;10;14;7];
   [1;10;13;0;6;9;8;7;4;15;14;3;11;5;2;12];
]

let S4 = [
   [7;13;14;3;0;6;9;10;1;2;8;5;11;12;4;15];
   [13;8;11;5;6;15;0;3;4;7;2;12;1;10;14;9];
   [10;6;9;0;12;11;7;13;15;1;3;14;5;2;8;4];
   [3;15;0;6;10;1;13;8;9;4;5;11;12;7;2;14];
]

let S5 = [
   [2;12;4;1;7;10;11;6;8;5;3;15;13;0;14;9];
   [14;11;2;12;4;7;13;1;5;0;15;10;3;9;8;6];
   [4;2;1;11;10;13;7;8;15;9;12;5;6;3;0;14];
   [11;8;12;7;1;14;2;13;6;15;0;9;10;4;5;3];
]

let S6 = [
   [12;1;10;15;9;2;6;8;0;13;3;4;14;7;5;11];
   [10;15;4;2;7;12;9;5;6;1;13;14;0;11;3;8];
   [9;14;15;5;2;8;12;3;7;0;4;10;1;13;11;6];
   [4;3;2;12;9;5;15;10;11;14;1;7;6;0;8;13];
]

let S7 = [
   [4;11;2;14;15;0;8;13;3;12;9;7;5;10;6;1];
   [13;0;11;7;4;9;1;10;14;3;5;12;2;15;8;6];
   [1;4;11;13;12;3;7;14;10;15;6;8;0;5;9;2];
   [6;11;13;8;1;4;10;7;9;5;0;15;14;2;3;12];
]

let S8 = [
   [13;2;8;4;6;15;11;1;10;9;3;14;5;0;12;7];
   [1;15;13;8;10;3;7;4;12;5;6;11;0;14;9;2];
   [7;11;4;1;9;12;14;2;0;6;10;13;15;3;5;8];
   [2;1;14;7;4;10;8;13;15;12;9;0;3;5;6;11];
]

let Ss = [S1;S2;S3;S4;S5;S6;S7;S8]    

let SApply (s: int list list) (key:string) =
    let row = Convert.ToInt32( (key.[0].ToString() + key.[key.Length - 1].ToString()), 2 )
    let column = Convert.ToInt32 ( key.Substring(1, key.Length - 2 - 1 + 1), 2)

    let output = Convert.ToString(s.[row].[column], 2)
    
    ensureLength 4 output |> seqToString

let sbox key = 
    key 
    |> Seq.chunkBySize 6 
    |> Seq.map (fun i -> String i) |> Seq.toArray 
    |> Seq.zip Ss  
    |> Seq.map (fun (sbox, chunk) -> SApply sbox chunk)
    |> Seq.concat |> seqToString

let P = [
   16;07;20;21;
   29;12;28;17;
   01;15;23;26;
   05;18;31;10;
   02;08;24;14;
   32;27;03;09;
   19;13;30;06;
   22;11;04;25;
]

let permuteP = permute P

let func block key = 
    let expanded = expand block
    let xored = key |> xor <| expanded
    let output = (sbox >> permuteP) xored
    output
    
let IPminus1 = [
   40;08;48;16;56;24;64;32;
   39;07;47;15;55;23;63;31;
   38;06;46;14;54;22;62;30;
   37;05;45;13;53;21;61;29;
   36;04;44;12;52;20;60;28;
   35;03;43;11;51;19;59;27;
   34;02;42;10;50;18;58;26;
   33;01;41;09;49;17;57;25;
] 

let permuteIPminus1 = permute IPminus1
   
let encrypt key message =
    let binaryKey = key |> toBinary    
    let permuted = permutePC1 binaryKey
    let leftHalf = leftOf permuted
    let rightHalf = rightOf permuted
    let shiftsL = leftShiftingIteration leftHalf
    let shiftsR = leftShiftingIteration rightHalf
    let keysK = Seq.zip shiftsL shiftsR 
                |> Seq.map (fun (x,y) -> x+y) |> Seq.map (fun i -> permutePC2 i)
                |> Seq.toArray
    
    // step 2
    let permutedMsg = (toBinary >> padded >> permuteIP) message  // textToBinary
    let mutable leftHalf1 = leftOf permutedMsg
    let mutable rightHalf1 = rightOf permutedMsg
    
    for i in 0..15 do
        let newRightHalf = leftHalf1 |> xor <| func rightHalf1 keysK.[i]
        leftHalf1 <- rightHalf1 
        rightHalf1 <- newRightHalf
    
    let reversed = rightHalf1 + leftHalf1
    let permuted1 = permuteIPminus1 reversed

    permuted1

   
[<EntryPoint>]
let main argv = 
    encrypt "133457799BBCDFF1" "0123456789ABCDEF"
    |> printfn "%s"
    0 // return an integer exit code    