(** @canonical Odoc_html.Tree *)
module Tree = Tree

(** @canonical Odoc_html.Comment *)
module Comment = Comment

(** @canonical Odoc_html.Targets *)
module Targets = Targets

module Generator =
struct
  (** @canonical Odoc_html.Generator.ML *)
  module ML = ML

  (** @canonical Odoc_html.Generator.Reason *)
  module Reason = Reason
end

(* Exposed as an unstable public API for third-party packages to "hack" on, see

    https://github.com/ocaml/odoc/pull/252
    https://github.com/ocaml/odoc/issues/236. *)
(** @canonical Odoc_html.Url *)
module Url = Url
