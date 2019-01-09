open Helpers
open Neorest
open CalendarLib
open Printf

(*
let () = Http_client.Convenience.http_verbose ~verbose_status:true ()
 *)
type 'a event = { event_desc: 'a; when_: Calendar.t };;

let _ = print_endline "Création transaction";;

let neo = new neo4jConnector "127.0.0.1" 7474 "neo4j" "123";;

(*
let next_year : int -> (int option,_) Neoresult.t = fun cur ->
  let next_cmd = "MATCH (years:YEAR) WHERE years.year > {y} RETURN min(years.year)" in
  let (ans: string) = neo#cypher next_cmd ["y", `Int cur] in
  match to_json ans |> YU.drop_assoc |> List.assoc "data" with
  | `List[`List[`Null]] -> OK None
  | `List[`List[`Int x]]->
	Printf.printf "next year %d\n%!" x;
	OK (Some x)
  | _ -> Error ()

let prev_year : int -> (int option,_) Neoresult.t = fun cur ->
  let next_cmd = "MATCH (years:YEAR) WHERE years.year < {y} RETURN max(years.year)" in
  let (ans: string) = neo#cypher next_cmd ["y", `Int cur] in
  match to_json ans |> YU.drop_assoc |> List.assoc "data" with
  | `List[`List[`Null]] -> OK None
  | `List[`List[`Int x]]->
	Printf.printf "prev year %d\n%!" x;
	OK (Some x)
  | _ -> Error ()

let connect_years from dest =
  let cmd = "MATCH (l:YEAR{year: {ll} }), (r:YEAR{year: {rr} })
             WITH l as l, r as r
             CREATE (l)-[edge:NEXT_YEAR]->(r)
             RETURN edge
             " in
  let (ans: string) = neo#cypher cmd [ ("ll", `Int from); ("rr",`Int dest) ] in
  print_endline ans;
  match to_json ans |> YU.drop_assoc |> List.assoc "data" with
  | `List[`List[_]] -> OK ()
  | _ -> Error ()


let make_nodes events =
 (*       let () = match neo#cypher "match (n) detach delete n;" with
    | OK () -> ()
    | Error () -> fprintf stderr "Can't connect to database"; exit 1
  in*)
  let f = fun {event_desc; when_} ->
    (* Add year *)
    let y = Calendar.year when_ in
    let m = Calendar.month when_ |> Date.int_of_month in
    let d = Calendar.day_of_month when_ in

    let p1 = ["y", `Int y] in
    let p2 = ("m",`Int m) :: p1 in
    let p3 = ("d",`Int d) :: p2 in
    let p4 = ("desc",`String (sprintf "%s\\n%s" event_desc (Printer.CalendarPrinter.to_string when_) )) :: p3 in

    let _ = neo#cypher "MERGE (y:YEAR{year: {y} })
                             CREATE (y)-[:HAS_MONTH]->(m:MONTH{month:{m}    })
                             CREATE (m)-[:HAS_DAY]->  (d:DAY  {day:  {d}    })
                             CREATE (d)-[:HAS_EVENT]->(e:EVENT{desc: {desc} })
                             WITH y AS y
                             MATCH (n) RETURN n
                             " p4 in
    let _ = next_year y
      >>= function
        | Some next -> printf "There is next year %d\n%!" next; connect_years y next
        | None -> OK ()
    in
    let _ = prev_year y
            >>= function
              | Some prev -> printf "There is prev year %d\n%!" prev; connect_years prev y
              | None -> OK ()
    in

    ()
  in
  List.iter events ~f;;



let events =
  let open Calendar in
  [ { event_desc="event1"; when_ = make 2009 08 23 15 32 43 }
  ; { event_desc="event2"; when_ = make 2012 10 21 12 13 14 }
  ; { event_desc="event3"; when_ = make 2009 09 22 15 32 43 }
  ; { event_desc="event3"; when_ = make 2009 09 23 15 32 43 }
  ; { event_desc="event3"; when_ = make 2009 09 23 16 11 04 }
  ;
  ]

let () =
  make_nodes events
*)
























