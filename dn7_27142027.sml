(*
** dn7_27142027.sml
** Author: Tadej Borovšak <tadeboro@gmail.com>
*)

(* Vrni seznam [a, a + 1, ... b - 1, b] *)
fun obseg a b =
  let
    fun obseg_acc a b acc =
      if a > b
      then acc
      else obseg_acc a (b - 1) (b :: acc)
  in
    obseg_acc a b []
  end
val t01 = obseg 1 1 = [1]
val t02 = obseg 1 0 = []
val t03 = obseg 3 6 = [3, 4, 5, 6]

(* Funkcija, ki vrne seznam [1, 2, ..., n] *)
val stejNavzgor = obseg 1
val t04 = stejNavzgor 0 = []
val t05 = stejNavzgor 1 = [1]
val t06 = stejNavzgor 4 = [1, 2, 3, 4]

(* Funkcija, ki zamenja vrstni red znakov v nizu *)
fun obrniNiz niz = implode (List.foldl op:: [] (explode niz))
val t07 = obrniNiz "abc" = "cba"
val t08 = obrniNiz "" = ""

(* Določi, če je seznam oblike [1, 2, 1, 2, 1, ...] *)
fun enDva seznam =
  let
    fun start1 [] = true
      | start1 (x :: xs) = if x = 1 then start2 xs else false
    and start2 [] = true
      | start2 (x :: xs) = if x = 2 then start1 xs else false
  in
    start1 seznam
  end
val t09 = enDva [] = true
val t10 = enDva [1] = true
val t11 = enDva [1, 2] = true
val t12 = enDva [2] = false
val t13 = enDva [1, 2, 2] = false

(* Konstruktor števca *)
(*
fun stevec = {naslednji: 1}
  *)

(* Implementacija sklada *)
signature M_STACK = 
sig
  type 'a mstack
  val new : 'a -> 'a mstack
  val push : 'a mstack * 'a -> unit
  val pop : 'a mstack -> 'a option
end

structure Sklad :> M_STACK =
struct
  type 'a mstack = 'a list ref
  fun new el = ref [el]
  fun push (s, el) = s := el :: (!s)
  fun pop s =
    case !s of
      []      => NONE
    | x :: xs => (s := xs; SOME x)
end
val s = Sklad.new 3
val _ = Sklad.push (s, 4)
val _ = Sklad.push (s, 1)
val _ = Sklad.push (s, 2)
val t14 = Sklad.pop s = SOME 2
val t15 = Sklad.pop s = SOME 1
val t16 = Sklad.pop s = SOME 4
val t17 = Sklad.pop s = SOME 3
val t18 = Sklad.pop s = NONE

(* Podatkovni tip pcl *)
datatype 'a pcl = Pcl of 'a pcell ref
and 'a pcell = Nil | Cons of 'a * 'a pcl

(* Funkcije za delo s tipom pcl *)
exception Empty
val nill = Pcl (ref Nil) : int pcl
fun cons arg = Pcl (ref (Cons arg))
fun car arg =
  case !arg of
    Nil => raise Empty
  | Cons (x, _) => x
fun cdr arg =
  case !arg of
    Nil => raise Empty
  | Cons (_, xs) => xs

(* Testi za pcl *)
val t19 = cons (7, cons (6, cons (5, cons (4, cons (3, nill)))))
val t20 = car t19
