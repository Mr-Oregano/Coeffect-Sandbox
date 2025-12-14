open Types.ET
open Types.Interp

module Context : sig
  type t

  val empty : unit -> t
  val add_var : t -> id * typ -> unit
  val add_vars : t -> (id * typ) list -> unit
  val add_imp : t -> id * typ -> unit
  val add_imps : t -> (id * typ) list -> unit
  val remove_var : t -> id -> unit
  val remove_vars : t -> id list -> unit
  val remove_imp : t -> id -> unit
  val remove_imps : t -> id list -> unit
  val get_var : t -> id -> typ option
  val get_vars : t -> (id * typ) Seq.t
  val get_imp : t -> id -> typ option
  val get_imps : t -> (id * typ) Seq.t
end

module Env : sig
  type t

  val empty : unit -> t
  val add_var : t -> binding -> unit
  val add_vars : t -> binding list -> unit
  val add_var_shadow : t -> binding -> unit
  val add_vars_shadow : t -> binding list -> unit
  val add_imp : t -> binding -> unit
  val add_imps : t -> binding list -> unit
  val add_imp_shadow : t -> binding -> unit
  val add_imps_shadow : t -> binding list -> unit
  val get_var : t -> id -> value option
  val get_vars : t -> binding list
  val get_imp : t -> id -> value option
  val get_imps : t -> binding list
  val remove_var : t -> id -> unit
  val remove_vars : t -> id list -> unit
  val remove_imp : t -> id -> unit
  val remove_imps : t -> id list -> unit
  val get_imps_excluded : t -> id Seq.t -> binding list
end
