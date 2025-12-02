open Types.ET

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
