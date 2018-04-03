open StdLabels
open DocOckHtml

let get_package root = Root.Package.to_string (Root.package root)

let path identifier =
  match Url.from_identifier ~get_package ~stop_before:false identifier with
  | Error _ -> failwith "TODO"
  | Ok url ->
    let name = DocOckPaths.Identifier.name identifier in
    let components = List.tl (List.rev url.page) in
    let components = if String.equal (String.lowercase_ascii name) name then
        components @ [name]
      else
        components
    in
    String.concat ~sep:"." components
;;

let url identifier =
  match Url.from_identifier ~get_package ~stop_before:false identifier with
  | Error _ -> failwith "TODO"
  | Ok url -> Url.to_string url
;;

let html_string (doc : 'a DocOckTypes.Documentation.t) =
  let html = (Documentation.to_html ~get_package doc) in
  let lst = List.map html ~f:(fun h ->
    Format.asprintf "%a" (Tyxml.Html.pp_elt ()) h
  )
  in
  String.concat lst ~sep:"\n"
;;

let index_entry id doc =
  String.concat ~sep:"\n" [ url id; path id; html_string doc ];
;;

(* CR jsomers for lwhite: This is the workhorse function that parses
the odoc tree recursively printing stuff out. *)
(* [nearest_id] is for free-floating doc comments *)
let rec print_index_entries signature nearest_id =
  let open DocOckTypes.Signature in
  List.iter signature ~f:(fun i ->
    print_endline "\n...\n";
    match i with
    | Type t -> print_endline (index_entry t.id t.doc)
    | Comment c -> begin
        match c with
        | Documentation doc -> begin
            print_endline (index_entry nearest_id doc);
          end
        | Stop -> print_endline ""
      end
    | Value v -> print_endline (index_entry v.id v.doc)
    | Module m -> begin
        print_endline (index_entry m.id m.doc);
        let expansion = match m.expansion with
          | Some AlreadyASig ->
            begin match m.type_ with
            | ModuleType (DocOckTypes.ModuleType.Signature sg) -> DocOckTypes.Module.Signature sg
            | _ -> failwith "TODO"
            end
          | Some e -> e
          | None -> AlreadyASig
        in
        match expansion with
        | Signature s -> print_index_entries s m.id
        | AlreadyASig -> print_endline ""  (* CR jsomers for lwhite: Cases I didn't understand, I ignored... *)
        | Functor _ -> print_endline "" (* TODO *)
      end
    | _ -> print_endline "" (* CR jsomers for lwhite: What are the other cases that matter? Must we cover EVERY case? *)
  );
;;

let from_odoc ~env input =
  let root = Root.read input in
  match Root.file root with
  | Page _ -> failwith "TODO"  (* CR jsomers for lwhite: I suppose this is where we'd handle mld files right? *)
  | Unit {hidden; _} ->
    let unit = Unit.load input in
    let unit = DocOckLookup.lookup unit in
    let odoctree =
      let resolve_env = Env.build env (`Unit unit) in
      let resolved = DocOck.resolve (Env.resolver resolve_env) unit in
      let expand_env = Env.build env (`Unit resolved) in
      DocOck.expand (Env.expander expand_env) resolved
      |> DocOckLookup.lookup
      |> DocOck.resolve (Env.resolver expand_env)
    in
    print_endline (index_entry odoctree.id odoctree.doc);
    let signature = match odoctree.content with
      | Pack _ -> failwith "TODO"  (* CR jsomers for lwhite: Not sure whether we need to handle this case. *)
      | Module s -> s
    in
    print_index_entries signature odoctree.id;
;;
