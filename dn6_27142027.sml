(*
** dn6_27142027.sml
** Author: Tadej Borovšak <tadeboro@gmail.com>
*)

fun foldl _ e [] = e
  | foldl f e (x::xs) = foldl f (f(x, e)) xs

fun foldr _ e [] = e
  | foldr f e (x::xs) = f(x, foldr f e xs)

fun map _ [] = []
  | map f (x::xs) = (f x) :: map f xs

fun filter _ [] = []
  | filter p (x::xs) = if p(x) then x::(filter p xs) else filter p xs

(* Numerično izračunaj določeni integral funkcije f na intervalu [a, b]
** s korakom st. *)
fun integral f (a, b, n) =
  let
    val d = (b - a) / n
    fun sestaviSez i =
      if i <= 0.0
      then []
      else 2.0 * f (a + i * d) :: sestaviSez (i - 1.0)
  in
    d / 2.0 * foldl (fn (x, e) => x + e) (f (a) + f (b)) (sestaviSez (n - 1.0))
  end
val t01 = integral (fn x => x) (0.0, 1.0, 10.0)

(* Uredi elemente seznama na podlagi danega kriterija *)
fun uredi f sez =
  let
    fun vstavi (el, []) = [el]
      | vstavi (el, x :: xs) =
          if f (el, x)
          then x :: vstavi (el, xs)
          else el :: x :: xs
  in
    foldr vstavi [] sez
  end
val t02 = uredi (fn (x, y) => x < y) [3, 4, 2, 5]

(* Preštejemo zaporedne pare presledka in nepresledka *)
fun prestejBesede niz =
  let
    val presledki = map Char.isSpace (explode niz)
    val pairs = ListPair.zip (presledki, (tl presledki))
    val first = if hd presledki then 0 else 1
  in
    first + List.foldl (fn ((a, b), s) => if a andalso (not b) then s + 1 else s) 0 pairs
  end
val t03 = prestejBesede " tsdf da fd f"

(* Produkt matrik *)
fun produktMatrik a b =
  let
    fun transponiraj ([] :: _) = []
      | transponiraj vrstice = map hd vrstice :: transponiraj (map tl vrstice)
    fun skalarni [] [] = 0
      | skalarni (x :: xs) (y :: ys) =
          x * y + (skalarni xs ys)
  in
    map (fn (x, y) => skalarni x y) List.zip (a (transponiraj b))
  end
val t04 = produktMatrik [[0, 7], [8, 9]] [[1, 2, 3], [4, 5, 6]]
