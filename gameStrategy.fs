(* The while loop represents the game. *)
(* Each iteration represents a turn of the game *)
(* where you are given inputs (the heights of the mountains) *)
(* and where you have to print an output (the index of the mountain to fire on) *)
(* The inputs you are given are automatically updated according to your last actions. *)
open System

let mountains = 
    Seq.initInfinite (fun _ -> int(Console.In.ReadLine()))
    |> Seq.mapi (fun i el -> el, i)
    |> Seq.take 8
    |> Seq.sortBy (fun el i -> (-el))
    |> Seq.map snd
    
(* game loop *)
while true do
    (* Write an action using printfn *)
    (* To debug: Console.Error.WriteLine("Debug message") *)
    mountains |> Seq.iter (printfn "%i") 
    (* The index of the mountain to fire on. *)
    ()