(*
** dn3_27142027.sml
** Author: Tadej Borovšak <tadeboro@gmail.com>
*)

(* Doloci polozaj elementa v seznamu. Prvi element je na polozaju 0. *)
fun polozaj (a : int, xs : int list) =
  let
    fun pos (_, _, []) = NONE
      | pos (a, n, x :: xs) =
          if a = x
          then SOME n
          else pos (a, n + 1, xs)
  in
    pos (a, 0, xs)
  end

(* Izracunaj koren stevila do zeljene natancnosti *)
fun koren (a : real, delta : real) =
  if a < 0.0
  then NONE
  else
    let
      fun iter (x : real) =
        if abs (x * x - a) < delta
        then x
        else iter (0.5 * (x + a / x))
    in
      SOME (iter (a))
    end

(* Posplosena stevila - Num typeclass in Haskell *)
datatype number = Int of int
                | Real of real
(* Sestevanje posplosenih stevil *)
fun sestej (a : number, b : number) =
  case (a, b) of
    (Int x,  Int y)  => Int (x + y)
  | (Int x,  Real y) => Real (Real.fromInt x + y)
  | (Real x, Int y)  => Real (x + Real.fromInt y)
  | (Real x, Real y) => Real (x + y)

(* Binarna iskalna drevesa *)
datatype 'a bstree = Nil
                   | Node of {key : 'a, left : bstree, right : bstree}
fun 
(* Vstavljanje elementov v drevo *)
fun vstavi (n : int, Nil : int bstree) = Node {key = n, left = Nil, right = Nil}
  | vstavi (n, tree) =
  case tree of
    Node {key = k, left = l, right = r} =>
      case Int.compare (n, k) of
        EQUAL   => tree
      | LESS    => Node {key = k, left = vstavi (n, l), right = r}
      | GREATER => Node {key = k, left = l, right = vstavi (n, r)}

(* Izpisi elemente drevesa v narascajocem vrstnem redu *)
fun poVrsti (tree : int bstree) =
  case tree of
    Nil => []
  | Node {key = k, left = l, right = r} => poVrsti l @ [k] @ poVrsti r

(* Vrni visino drevesa *)
fun visina (tree : int bstree) =
  case tree of
    Nil => 0
  | Node {key = _, left = l, right = r} => 1 + Int.max (visina l, visina r)

(* Preveri veljavnost binarnega iskalnega drevesa *)
fun jeBst Nil = true
  | jeBst Node {key = k, left = l, right = r} =


(* Tests *)
val t01 = polozaj (4, [1, 2, 3, 4, 5, 6]) = SOME 3
val t02 = koren (~1.0, 0.6)
val t03 = koren (3.0, 0.01)
val t04 = sestej (Int 1, Real 2.0)
val t05 = vstavi (Nil, 3)
