open Printf
module Http_client = Nethttp_client
open Http_client.Convenience


let http_get  = Http_client.Convenience.http_get
let http_post = Http_client.Convenience.http_post
let to_json = Yojson.Safe.from_string
let print_json = Yojson.Safe.pretty_to_channel stdout

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



let match_regexp regexpr chaine =
  let rex = Netstring_pcre.regexp regexpr in
  let results = Netstring_pcre.full_split rex chaine in
    match results with
      | [Netstring_pcre.Text s]       -> []
      | (Netstring_pcre.Delim s)::q   -> List.map (fun el -> match el with
                                                 | Netstring_pcre.Group (n,s) -> s
                                                 | _ -> "") q
      | _                            -> [];;


let talkToNeo ?(verbose=false) server port login passwd postData = (*On créé une transaction*)
                let url = sprintf "http://%s:%d/db/data/transaction/" server port in
                let passneo =  sprintf "neo4j:%s" passwd |> BatBase64.str_encode in
                if verbose then begin print_endline url; print_endline passneo;  end;
                let req = new Http_client.post_raw url postData in
                if verbose then print_endline "connexion ok...";
                let pipeline = new Http_client.pipeline in
                if verbose then print_endline "pipe...";
                req#set_req_header "Authorization" passneo;
                req#set_req_header "Content-type" "application/json";
                if verbose then print_endline "headers..";
                pipeline#add_with_callback req @@
                (fun call -> match call#response_status with
                | `Ok -> ()
                | `Bad_request -> 
                                if verbose then print_endline call#response_body#value;
                    let j = to_json call#response_body#value in
                    if verbose then j |> YU.drop_assoc |> List.assoc "message"
                      |> YU.drop_string |> print_endline;
                 | _ -> 
                                (* print_endline call#response_status_text;
                                 print_endline call#response_body#value;
                                 print_endline "callback";*)
                                 ()
                 );
                 pipeline#run ();
                 let (ans: string) = req#get_resp_body () in
                 (*let urls = match_regexp ".+?commit\\\":\\\"http:.+?db.data.transaction.(\d+)\/commit\\\".*" ans in
                 let res = if BatList.length urls > 0 then BatList.hd urls else "" in*)
                 if verbose then print_endline ans;
                 ans;;



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

        method getColumn col = H.find_all hColumns col
        method getLine   n   = lines.(n)
        method getLineMeta n = linesMeta.(n)
        method getColumnForLine col n = let l = H.find_all hColumns col in L.at l n
end


class neo4jConnector server port login passwd  = object(self)
        val mutable idTransaction = 0
        method initTransaction  = let result = talkToNeo ~verbose:false server port login passwd "" in
                                  let urls = match_regexp ".+?commit\":\"http:.+?db.data.transaction.(\\d+)\\/commit\".*" result in
                                  let res = if BatList.length urls > 0 then BatList.hd urls else "" in
                                  idTransaction <- (int_of_string res);
                                  int_of_string res

        method endTransaction = true
        method cypher req params = (*let buildParams params : (string * YojsonBasic.json) = "parameters",  `Assoc (L.map (fun (a,b) -> (a,`String b)) params) in*)
                                   let args = `Assoc [("statements", `List [`Assoc [("statement", `String req); ("parameters", `Assoc params) ]])] in
                                   let req  = YB.to_string args in
                                   let result = talkToNeo ~verbose:false server port login passwd req in
                                   (*if verbose then print_endline result;*)
                                   result;

end







