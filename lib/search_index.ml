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

let rec full_name : type a. (Root.t, a) Identifier.t -> string =
  let open Identifier in function
  | Root(root, s) -> (Root.Package.to_string (Root.package root)) ^ "." ^ s
  | Page(root, s) -> (Root.Package.to_string (Root.package root)) ^ "." ^ s
  | Module(parent, s) -> full_name parent ^ "." ^ s
  | Argument(parent, _, s) -> full_name parent ^ "." ^ s
  | ModuleType(parent, s) -> full_name parent ^ "." ^ s
  | Type(parent, s) -> full_name parent ^ "." ^ s
  | CoreType s -> s
  | Constructor(parent, s) -> full_name parent ^ "." ^ s
  | Field(parent, s) -> full_name parent ^ "." ^ s
  | Extension(parent, s) -> full_name parent ^ "." ^ s
  | Exception(parent, s) -> full_name parent ^ "." ^ s
  | CoreException s -> s
  | Value(parent, s) -> full_name parent ^ "." ^ s
  | Class(parent, s) -> full_name parent ^ "." ^ s
  | ClassType(parent, s) -> full_name parent ^ "." ^ s
  | Method(parent, s) -> full_name parent ^ "." ^ s
  | InstanceVariable(parent, s) -> full_name parent ^ "." ^ s
  | Label(parent, s) -> full_name parent ^ "." ^ s

let url identifier =
  match Url.from_identifier ~get_package ~stop_before:false identifier with
  | Error _ -> failwith "TODO"
  | Ok url -> Url.to_string url

let html_string (doc : 'a DocOckTypes.Documentation.t) =
  let html = (Documentation.to_html ~get_package doc) in
  let lst = List.map html ~f:(fun h ->
    Format.asprintf "%a" (Tyxml.Html.pp_elt ()) h
  )
  in
  String.concat lst ~sep:"\n"

let index_entry id doc =
  String.concat ~sep:"\n" [ url id; package id; full_name id; html_string doc ]

let mld_index_entry id doc =
  let package = package id in
  let url = package ^ "/index.html" in
  String.concat ~sep:"\n" [ url; package; full_name id; html_string doc ]

let rec print_class_signature_entries nearest_id
          (class_signature : 'a DocOckTypes.ClassSignature.t) fmt =
  List.iter class_signature.items ~f:(fun i ->
    match i with
    | DocOckTypes.ClassSignature.Method m ->
        Format.fprintf fmt "%s" (index_entry m.id m.doc)
    | InstanceVariable iv ->
        Format.fprintf fmt "%s" (index_entry iv.id iv.doc)
    | Constraint _ -> ()
    | Inherit cte -> begin
        match cte with
        | Constr _ -> ()
        | Signature cs -> print_class_signature_entries nearest_id cs fmt
      end
    | Comment c -> begin
        match c with
        | Documentation doc ->
            Format.fprintf fmt "%s" (index_entry nearest_id doc)
        | Stop -> ()
      end
  )

let rec print_signature_entries : 'a. _ -> (_, 'a) Identifier.t -> _ -> _ =
  fun signature nearest_id fmt ->
    let open DocOckTypes.Signature in
    List.iter signature ~f:(fun i ->
      let dots = "\n...\n" in
      Format.fprintf fmt "%s" dots;
      match i with
      | Module m ->
          Format.fprintf fmt "%s" (index_entry m.id m.doc);
          let signature =
            match m.expansion with
            | Some AlreadyASig -> begin
                match m.type_ with
                | ModuleType (Signature sg) -> sg
                | _ -> assert false
              end
            | Some (Signature sg) -> sg
            | Some (Functor(_, sg)) -> sg
            | None -> []
          in
          print_signature_entries signature m.id fmt
      | ModuleType mt ->
          Format.fprintf fmt "%s" (index_entry mt.id mt.doc);
          let signature =
            match mt.expansion with
            | Some AlreadyASig -> begin
                match mt.expr with
                | Some (Signature sg) -> sg
                | _ -> assert false
              end
            | Some (Signature sg) -> sg
            | Some (Functor(_, sg)) -> sg
            | None -> []
          in
          print_signature_entries signature mt.id fmt
      | Type t -> Format.fprintf fmt "%s" (index_entry t.id t.doc);
      | TypExt te -> begin
          List.iter te.constructors
            ~f:(fun (c : 'a DocOckTypes.Extension.Constructor.t) ->
              Format.fprintf fmt "%s" (index_entry c.id c.doc);
            )
        end
      | Exception e ->
        begin
          Format.fprintf fmt "%s" (index_entry e.id e.doc);
          match e.args with
          | Tuple _ -> ()
          | Record field_list -> begin
              List.iter field_list
                ~f:(fun (fld : 'a DocOckTypes.TypeDecl.Field.t) ->
                  Format.fprintf fmt "%s" (index_entry fld.id fld.doc)
                )
            end
        end
      | Value v -> Format.fprintf fmt "%s" (index_entry v.id v.doc);
      | External e -> Format.fprintf fmt "%s" (index_entry e.id e.doc);
      | Class c -> begin
          Format.fprintf fmt "%s" (index_entry c.id c.doc);
          match c.expansion with
          | Some cs -> print_class_signature_entries c.id cs fmt
          | None -> ()
        end
      | ClassType ct -> begin
          Format.fprintf fmt "%s" (index_entry ct.id ct.doc);
          match ct.expansion with
          | Some cs -> print_class_signature_entries ct.id cs fmt
          | None -> ()
        end
      | Include inc -> begin
          Format.fprintf fmt "%s" (index_entry inc.parent inc.doc);
          print_signature_entries inc.expansion.content nearest_id fmt;
        end
      | Comment c -> begin
          match c with
          | Documentation doc ->
              Format.fprintf fmt "%s" (index_entry nearest_id doc)
          | Stop -> ()
        end
    )

let from_unit ~output ~(unit:'a DocOck.Types.Unit.t) =
  let oc = open_out (Fs.File.to_string output) in
  let fmt = Format.formatter_of_out_channel oc in
  let entry = index_entry unit.id unit.doc in
  Format.fprintf fmt "%s" entry;
  let signature =
    match unit.content with
    | Pack _ -> failwith "TODO"
    | Module s -> s
  in
  print_signature_entries signature unit.id fmt;
  close_out oc

let from_page ~output ~(page:'a DocOck.Types.Page.t) =
  let oc = open_out (Fs.File.to_string output) in
  let fmt = Format.formatter_of_out_channel oc in
  let entry = index_entry page.name page.content in
  Format.fprintf fmt "%s" entry;
  close_out oc

let from_mld ~output ~(page:'a DocOck.Types.Page.t) =
  let oc = open_out (Fs.File.to_string output) in
  let fmt = Format.formatter_of_out_channel oc in
  let entry = mld_index_entry page.name page.content in
  Format.fprintf fmt "%s" entry;
  close_out oc;

