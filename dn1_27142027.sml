(*
** dn1_27142027.sml
** Author: Tadej Borovšak <tadeboro@gmail.com>
*)

(* Naslednjik *)
fun naslednje (n : int) : int = n + 1

(* Vrni večje od podanih števil *)
fun max (a : int, b : int) : int = if a > b then a else b

(* Izračunaj n! *)
fun fakulteta (n : int) : int =
  if n < 2 then 1 else n * fakulteta (n - 1)

(* Vrni n-to Fibonaccijevo stevilo *)
fun fib (n : int) : int =
  if n < 3 then 1 else fib (n - 1) + fib (n - 2)

(* Izračunaj x^n *)
fun potenca (x : int, n : int) : int =
  if n = 0 then 1 else x * potenca (x, n - 1)

(* Preveri, če je n praštevilo *)
fun jePrastevilo' (n : int, b : int) : bool =
  if n mod b = 0
  then false
  else
    if b * b > n
    then true
    else jePrastevilo' (n, b + 1)

fun jePrastevilo (n : int) : bool =
  if n = 1
  then false
  else
    if n = 2
    then true
    else jePrastevilo' (n, 2)

(* Vrni niz, ki predstavlja binarni zapis števila *)
fun binarno (n : int) : string =
  if n = 0
  then "0"
  else
    if n = 1
    then "1"
    else binarno (n div 2) ^ binarno (n mod 2)

(* Izracunaj najvecji skupni delitelj stevil a in b *)
fun gcd (a : int, b : int) : int =
  if a < b
  then gcd (b, a)
  else
    if a mod b = 0
    then b
    else gcd (b, a mod b)


(* Testi *)
val t01 = (naslednje 3) = 4
val t02 = (max (2, 3)) = 3
val t03 = (fakulteta 5) = 120
val t04 = (fib 7) = 13
val t05 = (potenca (3, 5)) = 243
val t06 = (jePrastevilo 2) = true
val t07 = (jePrastevilo 9) = false
val t08 = (binarno 6) = "110"
val t09 = (gcd (2, 3)) = 1
val t10 = (gcd (8, 12)) = 4
