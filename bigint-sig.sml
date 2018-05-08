signature BIGINT =
sig
type bigint
val bigzero: bigint
val normalize : bigint -> bigint
val fromString : string -> bigint
val toString : bigint -> string
val bigint : int -> bigint
val int : bigint -> int
val ~~ : bigint -> bigint
val abs : bigint -> bigint
val ++ : bigint * bigint -> bigint
val succ : bigint -> bigint
val min : bigint * bigint -> bigint
val max : bigint * bigint -> bigint
val sign : bigint -> int
val sameSign : bigint * bigint -> bool
val ** : bigint * bigint -> bigint
val compare : bigint * bigint -> order
val << : bigint * bigint -> bool
val <<= : bigint * bigint -> bool
val >> : bigint * bigint -> bool
val >>= : bigint * bigint -> bool
val == : bigint * bigint -> bool
val len : bigint -> int
val lenCompare : bigint * bigint -> order
val lenLt : bigint * bigint -> bool
val lenLeq : bigint * bigint -> bool
val lenGt : bigint * bigint -> bool
val lenGeq : bigint * bigint -> bool
val lenEq : bigint * bigint -> bool
val -- : bigint * bigint -> bigint
val pred : bigint -> bigint
exception division_by_zero
val %% : bigint * bigint -> bigint * bigint
val div : bigint * bigint -> bigint
val mod : bigint * bigint -> bigint
val quo : bigint * bigint -> bigint
val rem : bigint * bigint -> bigint
end