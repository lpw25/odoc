open StdLabels
open DocOckPaths
open DocOckHtml

let get_package root = Root.Package.to_string (Root.package root)

let rec package : type a. (Root.t, a) Identifier.t -> string =
  let open Identifier in function
  | Root(root, _) -> Root.Package.to_string (Root.package root)
  | Page(root, _) -> Root.Package.to_string (Root.package root)
  | Module(parent, _) -> package parent
  | Argument(parent, _, _) -> package parent
  | ModuleType(parent, _) -> package parent
  | Type(parent, _) -> package parent
  | CoreType _ -> failwith "core types have no package"
  | Constructor(parent, _) -> package parent
  | Field(parent, _) -> package parent
  | Extension(parent, _) -> package parent
  | Exception(parent, _) -> package parent
  | CoreException _ -> failwith "core exceptions have no package"
  | Value(parent, _) -> package parent
  | Class(parent, _) -> package parent
  | ClassType(parent, _) -> package parent
  | Method(parent, _) -> package parent
  | InstanceVariable(parent, _) -> package parent
  | Label(parent, _) -> package parent

let rec name : type a. (Root.t, a) Identifier.t -> string =
  let open Identifier in function
  | Root(_, s) -> s
  | Page(_, s) -> s
  | Module(parent, s) -> name parent ^ "." ^ s
  | Argument(parent, _, s) -> name parent ^ "." ^ s
  | ModuleType(parent, s) -> name parent ^ "." ^ s
  | Type(parent, s) -> name parent ^ "." ^ s
  | CoreType s -> s
  | Constructor(parent, s) -> name parent ^ "." ^ s
  | Field(parent, s) -> name parent ^ "." ^ s
  | Extension(parent, s) -> name parent ^ "." ^ s
  | Exception(parent, s) -> name parent ^ "." ^ s
  | CoreException s -> s
  | Value(parent, s) -> name parent ^ "." ^ s
  | Class(parent, s) -> name parent ^ "." ^ s
  | ClassType(parent, s) -> name parent ^ "." ^ s
  | Method(parent, s) -> name parent ^ "." ^ s
  | InstanceVariable(parent, s) -> name parent ^ "." ^ s
  | Label(parent, s) -> name parent ^ "." ^ s

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
  String.concat ~sep:"\n" [ url id; package id; name id; html_string doc ];
;;

let rec print_class_signature nearest_id (class_signature : 'a DocOckTypes.ClassSignature.t) fmt =
  List.iter class_signature.items ~f:(fun i ->
    match i with
    | DocOckTypes.ClassSignature.Method m -> Format.fprintf fmt "%s" (index_entry m.id m.doc)
    | InstanceVariable iv -> Format.fprintf fmt "%s" (index_entry iv.id iv.doc)
    | Inherit cte -> begin
        match cte with
        | Constr _ -> ignore ()
        | Signature cs -> print_class_signature nearest_id cs fmt
      end
    | Comment c -> begin
        match c with
        | Documentation doc -> Format.fprintf fmt "%s" (index_entry nearest_id doc)
        | Stop -> ignore ()
      end
    | _ -> ignore ()
  )
;;

(* [nearest_id] is used to handle free-floating doc comments. *)
let rec print_index_entries signature nearest_id fmt =
  (* TODO: file path is basically probably the top-level signature and id here. *)
  let open DocOckTypes.Signature in
  List.iter signature ~f:(fun i ->
    let dots = "\n...\n" in
    Format.fprintf fmt "%s" dots;
    match i with
    | Module m -> begin
        Format.fprintf fmt "%s" (index_entry m.id m.doc);
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
        | Signature s -> print_index_entries s m.id fmt
        | AlreadyASig -> ignore ()
        | Functor (_, si) -> print_index_entries si m.id fmt
      end
    | ModuleType mt -> Format.fprintf fmt "%s" (index_entry mt.id mt.doc);
    | Type t -> Format.fprintf fmt "%s" (index_entry t.id t.doc);
    | TypExt te -> begin
        (* TODO: ignoring the toplevel doc bc it's not clear what to do with it bc there
           is no corresponding [id]. *)
        List.iter te.constructors ~f:(fun (c : 'a DocOckTypes.Extension.Constructor.t) ->
          Format.fprintf fmt "%s" (index_entry c.id c.doc);
        )
      end
    (* TODO: There seem to be expansions even of an Exception's [args] record field which
       lead to more docs and ids. *)
    | Exception e -> Format.fprintf fmt "%s" (index_entry e.id e.doc);
    | Value v -> Format.fprintf fmt "%s" (index_entry v.id v.doc);
    | External e -> Format.fprintf fmt "%s" (index_entry e.id e.doc);
    | Class c -> begin
        Format.fprintf fmt "%s" (index_entry c.id c.doc);
        match c.expansion with
          | Some cs -> print_class_signature c.id cs fmt
          | None -> ignore ()
      end
    | ClassType ct -> begin
        Format.fprintf fmt "%s" (index_entry ct.id ct.doc);
        match ct.expansion with
        | Some cs -> print_class_signature ct.id cs fmt
        | None -> ignore ();

        match ct.expr with
        | Constr _ -> ignore ()
        | Signature cs -> print_class_signature ct.id cs fmt
      end
    | Include inc -> begin
        Format.fprintf fmt "%s" (index_entry inc.parent inc.doc);
        (* TODO: Not sure this [nearest_id] is right. Possibly it should be [inc.parent]
           but that has the wrong type. *)
        print_index_entries inc.expansion.content nearest_id fmt;
      end
    | Comment c -> begin
        match c with
        | Documentation doc -> Format.fprintf fmt "%s" (index_entry nearest_id doc)
        | Stop -> ignore ()
      end
  );
;;

let from_odoc ~env ~output ~(odoctree:'a DocOck.Types.Unit.t) input =
  let root = Root.read input in
  match Root.file root with
  | Page _ -> failwith "TODO"
  | Unit {hidden; _} ->
    if (not hidden) then
      begin
        let oc = open_out (Fs.File.to_string output) in
        let fmt = Format.formatter_of_out_channel oc in
        Format.fprintf fmt "%s" (index_entry odoctree.id odoctree.doc);
        let signature = match odoctree.content with
          (* CR jsomers for lwhite: Not sure whether we need to handle this case. *)
          | Pack _ -> failwith "TODO"
          | Module s -> s
        in
        print_index_entries signature odoctree.id fmt;
        close_out oc;
      end
    else
      ()
;;

let from_mld ~env ~output ~(content:'a DocOck.Types.Documentation.t) ~(page:'a DocOck.Types.Page.t) =
  let oc = open_out (Fs.File.to_string output) in
  let fmt = Format.formatter_of_out_channel oc in
  let id = page.name in  (* FIXME: Fix the name/URL that we print at the top of this. *)
  Format.fprintf fmt "%s" (index_entry id content);
  close_out oc;
;;
