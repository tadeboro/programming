(*
** dn3_27142027.sml
** Author: Tadej Borovšak <tadeboro@gmail.com>
*)

(* Doloci polozaj elementa v seznamu. Prvi element je na polozaju 0. *)
fun polozaj (a, xs) =
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
fun koren (a, delta) =
  if a < 0.0
  then NONE
  else
    let
      fun iter x =
        if abs (x * x - a) < delta
        then x
        else iter (0.5 * (x + a / x))
    in
      SOME (iter a)
    end

(* Posplosena stevila - Num typeclass in Haskell *)
datatype number = Int of int
                | Real of real
(* Sestevanje posplosenih stevil *)
fun sestej (a, b) =
  case (a, b) of
    (Int x,  Int y)  => Int (x + y)
  | (Int x,  Real y) => Real (Real.fromInt x + y)
  | (Real x, Int y)  => Real (x + Real.fromInt y)
  | (Real x, Real y) => Real (x + y)

(* Binarna iskalna drevesa *)
datatype 'a bstree = Nil
                   | Node of {key : 'a, left : 'a bstree, right : 'a bstree}
(* Vstavljanje elementov v drevo *)
fun vstavi (n, Nil) = Node {key = n, left = Nil, right = Nil}
  | vstavi (n, Node {key = k, left = l, right = r}) =
      case Int.compare (n, k) of
        EQUAL   => Node {key = k, left = l, right = r}
      | LESS    => Node {key = k, left = vstavi (n, l), right = r}
      | GREATER => Node {key = k, left = l, right = vstavi (n, r)}

(* Izpisi elemente drevesa v narascajocem vrstnem redu *)
fun poVrsti Nil = []
  | poVrsti (Node {key = k, left = l, right = r}) =
      poVrsti l @ [k] @ poVrsti r

(* Vrni visino drevesa *)
fun visina Nil = 0
  | visina (Node {key = _, left = l, right = r}) =
      1 + Int.max (visina l, visina r)

(* Preveri veljavnost binarnega iskalnega drevesa *)
fun jeBst tree =
  let
    fun isSorted [] = true
      | isSorted [_] = true
      | isSorted (x :: y :: xs) = x < y andalso isSorted (y :: xs)
  in
    isSorted (poVrsti tree)
  end

(* Skusaj sestaviti seznam elementov, ki se sestejeo v zelen rezultat *)
fun sestavi (_, 0) = SOME []
  | sestavi (xs, n) =
      let
        fun vstavi _ [] = []
          | vstavi a (x :: xs) = (a :: x) :: vstavi a xs
        fun potencna [] = [[]]
          | potencna (x :: xs) =  potencna xs @ vstavi x (potencna xs)
        fun sum [] s = s
          | sum (x :: xs) s = sum xs (s + x)
        fun properSum xs = n = sum xs 0
        val res = List.filter properSum (potencna xs)
      in
        if null res then NONE else SOME (hd res)
      end


(* Preveri, ce je drevo prazno *)
fun praznoDrevo Nil = true
  | praznoDrevo _ = false

(* Vrne stevilo elementov v drevesu *)
fun stElementov Nil = 0
  | stElementov (Node {key = _, left = l, right = r}) =
      1 + stElementov l + stElementov r

(* Vrnse stevilo listov v drevesu *)
fun stListov tree =
      case tree of
        Nil => 0
      | Node {key = _, left = Nil, right = Nil} => 1
      | Node {key = _, left = l, right = r} =>
          stListov l + stListov r

(* Izracunaj casovni interval med dvema casovnima zigoma *)
fun casovniRazpon (zacetek, konec) =
  let
    fun toOffset (h, m, s) = 3600 * h + 60 * m + s
    fun fromOffset off =
      let
        val h = off div 3600
        val m = (off - h * 3600) div 60
        val s = off - h * 3600 - m * 60
      in
        (h, m, s)
      end
    val s = toOffset zacetek
    val e = toOffset konec
    val diff = (e - s) mod 86400
  in
    fromOffset diff
  end

(* Tests *)
val t01 = polozaj (4, [1, 2, 3, 4, 5, 6]) = SOME 3
val t02 = koren (~1.0, 0.6)
val t03 = koren (3.0, 0.01)
val t04 = sestej (Int 1, Real 2.0)
val t05 = vstavi (3, Nil)
val t06 = vstavi (7, t05)
val t07 = vstavi (5, t06)
val t08 = poVrsti (t07)
val t09 = visina Nil
val t10 = visina t07
val t11 = jeBst t07
val t12 = jeBst (Node {key = 1,
                       left = Node {key = 2, left = Nil, right = Nil},
                       right = Nil})
val t13 = sestavi ([1, 2, 3], 0)
val t14 = sestavi ([1, 2, 3], 4)
val t15 = sestavi ([1, 2, 3], 6)
val t16 = sestavi ([1, 2, 3], 7)
val t17 = sestavi ([1, 2, ~3], ~1)

val t18 = praznoDrevo Nil
val t19 = praznoDrevo (Node {key = 1, left = Nil, right = Nil})

val t20 = stElementov Nil
val t21 = stElementov t07

val t22 = stListov Nil
val t23 = stListov t07

val t24 = casovniRazpon ((1, 23, 59), (1, 24, 0))
val t25 = casovniRazpon ((23, 56, 27), (0, 24, 28))
