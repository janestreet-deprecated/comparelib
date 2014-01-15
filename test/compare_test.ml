let ignore = `Should_refer_to_pervasives_explicitely
let ( = ) = `Should_refer_to_pervasives_explicitely
let ( <> ) = `Should_refer_to_pervasives_explicitely
let ( == ) = `Should_refer_to_pervasives_explicitely
let ( != ) = `Should_refer_to_pervasives_explicitely
let ( > ) = `Should_refer_to_pervasives_explicitely
let ( < ) = `Should_refer_to_pervasives_explicitely
let ( >= ) = `Should_refer_to_pervasives_explicitely
let ( <= ) = `Should_refer_to_pervasives_explicitely
let ( max ) = `Should_refer_to_pervasives_explicitely
let ( min ) = `Should_refer_to_pervasives_explicitely
let ( equal ) = `Should_refer_to_pervasives_explicitely
let ( compare ) = `Should_refer_to_pervasives_explicitely

module M1 = struct type t = unit with compare end

module M2 = struct type t = int with compare end

module M3 = struct type t = bool with compare end

module M4 = struct type t = int32 with compare end

module M5 = struct type t = nativeint with compare end

module M6 = struct type t = int64 with compare end

module M7 = struct type t = float with compare end

module M8 = struct type t = bool * float with compare end

module M9 = struct type t = bool * float * int with compare end

module M10 = struct type t = bool * float * int * string with compare  end

module M11 = struct type t = int ref with compare end

module M12 = struct type t = (float * float) option with compare end

module M13 = struct type t = float array with compare end

module M14 = struct type t = (int * int) array with compare end

module M15 = struct type t = float array array with compare end

module M16 = struct type t = int list with compare end

module M17 = struct type t = {
  s : string;
  b : float array list;
  mutable c : (int * int64 option);
} with compare
end

module M18 = struct type t = {
  a : float;
  b : float;
  c : float;
} with compare
end

module M19 = struct type t = Foo with compare end

module M20 = struct type t = Foo of int with compare end

module M21 = struct type t = Foo of int * float with compare    end

module M22 = struct type t = Foo | Bar of int | Baz of string option with compare end

module M23 = struct type t = [`Foo | `Bar of string * string] with compare end

module M24 = struct type t = int * string * [`Foo | `Bar ] with compare end

module M25 = struct type t = String.t with compare end

module M26 = struct type 'a t = 'a array with compare end

module MyList = struct type 'a t = Nil | Node of 'a * 'a t with compare end

module M27 = struct
  type t = int with compare
  module Inner = struct
    type nonrec t = t list with compare
    let _ = ((compare : int list -> int list -> int) : t -> t -> int)
  end
end

module M28 = struct
  (* making sure that nobody is reversing the type parameters *)
  type ('a, 'b) t = ('a * 'b) list with compare
  let _ = <:compare< (int,float) t >> [(1,nan)]
end

module Polyrec = struct
  type ('a, 'b) t = T of ('a option, 'b) t with compare

  type ('a, 'b) t1 = T of ('a option, 'b) t2
  and ('a, 'b) t2 = T1 of ('a list, 'b) t1 | T2 of ('a, 'b list) t2
  with compare
end

module type Variance_sig = sig
  type +'a t with compare
end

module Variance = struct
  type -'a t with compare
  type (-'a, +'b) u = 'a t * 'b with compare
end

module Test = struct
  let (=) : int -> int -> bool = Pervasives.(=)
  (* checking that for the types mentioned in the readme, we compare structurally  *)
  TEST = <:compare< unit option >> None (Some ()) = Pervasives.compare None (Some ())
  TEST = <:compare< unit list >> [] [()] = Pervasives.compare [] [()]
  TEST = <:compare< int array >> [|0; 1|] [|1|] = Pervasives.compare [|0; 1|] [|1|]
  TEST =
    Pervasives.(=)
      (List.sort <:compare< int option >> [Some 3; None; Some 2; Some 1])
      [None; Some 1; Some 2; Some 3]
end

module Variant_inclusion = struct
  type 'a type1 = [ `T1 of 'a ] with compare
  type 'a type2 = [ 'a type1 | `T2 ] with compare
  type 'a type3 = [ `T3 | 'a type1 ] with compare
  type 'a type4 = [ 'a type2 | `T4 | 'a type3 ] with compare
end
