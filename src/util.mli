type ident = string

module type Env = sig
  type key
  type value
  type t = (key * value) list

  val empty  : t
  val lookup : t key -> value
  val insert : t key value -> t
end

module type Value = sig
  type t
end

module type Variable = sig
  type t
  val equal   : t * t -> bool
  val compare : t * t -> int
end

module type Exp = sig
  type t
  type ty
  type value

  val type_check : t -> bool
  val type_inf   : t -> ty
  val eval       : t -> value
end
