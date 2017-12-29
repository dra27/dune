open! Import

module File_set : Set.S with type elt = string
module File_map : Map.S with type key = string

module Dir : sig
  type t

  val path     : t -> Path.t
  val files    : t -> File_set.t
  val sub_dirs : t -> t File_map.t

  (** Whether this directory is ignored by a [jbuild-ignore] file in
      one of its ancestor directories. *)
  val ignored : t -> bool

  val fold
    :  t
    -> traverse_ignored_dirs:bool
    -> init:'a
    -> f:(t -> 'a -> 'a)
    -> 'a
end

type t

val load : ?extra_ignored_subtrees:Path.Set.t -> Path.t -> t

val fold
  :  t
  -> traverse_ignored_dirs:bool
  -> init:'a
  -> f:(Dir.t -> 'a -> 'a)
  -> 'a

val root : t -> Dir.t

val find_dir : t -> Path.t -> Dir.t option

val exists : t -> Path.t -> bool
val file_exists : t -> Path.t -> string -> bool

val files_recursively_in : t -> ?prefix_with:Path.t -> Path.t -> Path.Set.t
