(*
** dn2_27142027.sml
** Author: Tadej Borovšak <tadeboro@gmail.com>
*)

(* Vrni mesec in letnico studenta *)
fun mesecInLetnica (_, _, (_, m, l), _) = (m, l)

(* Vrsni seznam predmetov, ki jih obiskuje student *)
fun seznamPredmetov (_, _, _, s) =
  let
    fun firstElList [] = []
      | firstElList ((pred, _) :: xs) = pred :: firstElList xs
  in
    firstElList s
  end

(* Sestej dva ulomka. Rezultat je okrajšani ulomek *)
fun sestej ((a, b), (c, d)) =
  let
    fun gcd (a, b) = if b = 0 then a else gcd (b, a mod b)

    val e = a * d + b * c
    val f = b * d
    val g = gcd (e, f)
  in
    (e div g, f div g)
  end

(* Preveri, ce seznam vsebuje podano vrednost *)
fun vsebuje (_, []) = false
  | vsebuje (a : int, x :: xs) = (a = x) orelse vsebuje (a, xs)

(* Vrni zadnji element seznama - ne deluje na praznih seznamih *)
fun zadnji [] = raise Empty
  | zadnji [x] = x
  | zadnji (_ :: xs) = zadnji (xs)

(* Vrni predzadnji element seznama - deluje le na seznamih dolzine 2
** ali vec *)
exception LessThan2
fun predzadnji (x :: _ :: []) = x
  | predzadnji (_ :: xs) = predzadnji xs
  | predzadnji _ = raise LessThan2

(* Vrni dolzino seznama *)
fun dolzina (xs) =
  let
    fun dolzina' ([], l) = l
      | dolzina' (x :: xs, l) = dolzina' (xs, l + 1)
  in
    dolzina' (xs, 0)
  end

(* Vrni n-ti element seznama - seznam je ostevilcen po C-jevsko (0, 1, ...) *)
fun vrni_ntega ([], _) = raise Empty
  | vrni_ntega (x :: _, 0) = x
  | vrni_ntega (_ :: xs, n) = vrni_ntega (xs, n - 1)

(* Obrni vrstni red elementov v seznamu. *)
fun obrni xs =
  let
    fun obrni' [] acc = acc
      | obrni' (x :: xs) acc = obrni' xs (x :: acc)
  in
    obrni' xs []
  end

(* Izbrisi vse pojavitve elementa iz seznama *)
fun brisi (_, []) = []
  | brisi (a, x :: xs) = if a <> x then x :: brisi (a, xs) else brisi (a, xs)

(* Sestej polinoma, podana s seznamoma koeficientov. Prva specifikacija
** tipa je dodana le zaradi neumnosti sml-ja, ki ne pozna koncepta
** splosnega numericnega tipa, ampak se privzeto odloci, da bodo elementi
** sestevanja v tem primeru cela stevila. *)
fun vsota ([], ps) = ps : real list
  | vsota (ps, []) = ps
  | vsota (p :: ps, q :: qs) = p + q :: vsota (ps, qs)

(* Sestej polinoma, podana kot seznama parov (koeficient, stopnja).
** Predpostavljamo, da sta seznama urejena narascajoce  po stopnjah
** koeficientov. *)
fun polyVsota [] ps = ps : (real * int) list
  | polyVsota ps [] = ps
  | polyVsota ((cp, dp) :: ps) ((cq, dq) :: qs) =
      if dp = dq
      then (cp + cq, dp) :: polyVsota ps qs
      else
        if dp > dq
        then (cq, dq) :: polyVsota ((cp, dp) :: ps) qs
        else (cp, dp) :: polyVsota ps ((cq, dq) :: qs)


(* Ugotovi, če je seznam palindrom *)
fun jePalindrom (xs) =
  xs = obrni (xs)

(* Zlij dva seznama *)
fun zdruzi (xs, []) = xs
  | zdruzi ([], ys) = ys
  | zdruzi (x :: xs, y :: ys) =
      if x > y
      then y :: x :: zdruzi (xs, ys)
      else x :: y :: zdruzi (xs, ys)


(* Hitro preverjanje *)
val s = ("Ime", 23456, (21, 11, 2011), [("x", 5), ("y", 7)])
val h1 = mesecInLetnica s = (11, 2011)
val h2 = seznamPredmetov s = ["x", "y"]

val t01 = (sestej ((3, 4), (10, 8))) = (2, 1)
val t02 = (vsebuje (4, [1, 2, 3])) = false
val t03 = (vsebuje (2, [1, 2, 3])) = true
val t04 = (zadnji ([1, 2, 3, 4])) = 4
val t05 = (dolzina ([1, 2, 3, 4, 5])) = 5
val t06 = (vrni_ntega (["jaz", "sem", "spisal", "tole", "rec"], 2)) = "spisal"
val t07 = (obrni ([1, 2, 3])) = [3, 2, 1]
val t08 = (brisi (2, [1, 2, 3, 2, 3])) = [1, 3, 3]
val t09 = vsota ([1.0, 2.0, 4.8], [3.1, 1.4])
val t10 = jePalindrom ([1, 2, 3, 2, 1]) = true

val t11 = zdruzi ([1, 4, 6], [2, 3, 7]) = [1, 2, 3, 4, 6, 7]

val t12 = polyVsota [(1.0, 1), (2.0, 3)] [(3.0, 0), (4.0, 1)]
