let rec fact = function
  | 1 -> 1
  | n -> n * fact (n-1)

let fact2 n = List.fold (*) 1 [1..n]


// https://www.codewars.com/kata/56a4872cbb65f3a610000026/train/fsharp

let split = 
  let rec split' = function
    | 0 -> []
    | x -> x % 10 :: split' (x / 10)
  split' >> List.rev 

let collect xs = 
  [0..(List.length xs)-1] 
  |> List.rev
  |> List.map (pown 10) 
  |> List.zip xs 
  |> List.map (fun (x,y) -> x * y) 
  |> List.sum

let rot f n xs =
  let l = xs |> List.length
  xs |> List.permute (f n l)

let rotr = 
  rot (fun n l i -> ( i + n ) % l)
  
let rotl = 
  rot (fun n l i -> ( l - 2 +  i + n ) % l)
  
let rotr1 = rotr 1
let rotl1 = rotl 1

let rotpd p d xs = 
  let s = xs |> Seq.take d |> List.ofSeq
  let e = xs |> Seq.skip d |> List.ofSeq |> p
  s @ e

let rotrd = rotpd rotr1
let rotld = rotpd rotl1 

printfn "%A" (rotrd 1 [1; 2; 3; 4; 5; 6])
printfn "%A" (rotld 1 [1; 2; 3; 4; 5; 6])
    
let combine = Seq.reduce (>>)

let rotla xs = 
  let fs = [0..(List.length xs)-1] |> List.map (fun x -> rotld x)
  let res = [1..(List.length xs)] 
            |> List.map (fun x -> List.take x fs |> combine)
            |> List.map (fun f -> f xs)
  xs :: res |> List.max

let maxRot = split >> rotla >> collect

  
printfn "%A" (maxRot 896219342)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
