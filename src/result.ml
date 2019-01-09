type ('a,'b) t = OK of 'a | Error of 'b

let (>>=) x f = match x with
  | OK a -> f a
  | Error b -> Error b

let (>>>) x f = match x with OK a -> OK (f a) | Error b -> Error b



module H = BatHashtbl 
module L = BatList
module A = BatArray
module Y = Yojson
module YB = Yojson.Basic
module YU =  struct
  let drop_assoc = function `Assoc xs -> xs | _ -> failwith "Bad argument"
  let drop_string = function `String s -> s | _ -> failwith "Bad argument"
  let drop_int = function `Int n -> n | _ -> failwith "Bad argument"
  let drop_list = function `List xs -> xs | _ -> failwith "Bad argument"
  let unwrap_res x = x |> drop_assoc |> List.assoc "data"
end




class neoResult = object(self)
        val mutable columnList = []
        val mutable hColumns   = H.create 345
        val mutable linesMeta  = [||]
        val mutable lines      = [||]

        method parseResult res = let json  = res |> YB.from_string in
                              let start = L.at (json |> YU.drop_assoc) 1 |> snd |> YU.drop_list |> L.hd |> YU.drop_assoc in
                              let cols, mylines = L.hd start, L.at start 1 in
                              columnList <- L.map (fun n ->  YU.drop_string n) (snd cols |> YU.drop_list);
                              linesMeta <- snd mylines |> YU.drop_list |> L.map (fun el -> L.at (YU.drop_assoc el) 1 |> snd ) |> A.of_list;
                              lines     <- snd mylines |> YU.drop_list |> L.map (fun el -> YU.drop_assoc el |> L.hd  |> snd ) |> A.of_list;
                              L.iteri (fun idx -> fun col -> A.iter (fun ligne -> H.add hColumns col (L.at (ligne|> YU.drop_list) idx)) lines )
                              columnList;
                              ()

        method getAll = columnList, hColumns, linesMeta, lines
end
