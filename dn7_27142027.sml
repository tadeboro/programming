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
val s = ref 0
fun stevec () = {
  naslednji = fn () => (s := !s + 1; !s),
  ponastavi = fn () => s := 0
}
val stev = stevec ()
val s1 = #naslednji stev () = 1
val s2 = #naslednji stev () = 2
val s3 = #naslednji stev () = 3
val _  = #ponastavi stev ()
val s4 = #naslednji stev () = 1

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
  fun pop (ref []) = NONE
    | pop (s as ref (x :: xs)) = (s := xs; SOME x)
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
fun nill () = Pcl (ref Nil)
fun cons arg = Pcl (ref (Cons arg))
fun car (Pcl arg) =
  case !arg of
    Nil => raise Empty
  | Cons (x, _) => x
fun cdr (Pcl arg) =
  case !arg of
    Nil => raise Empty
  | Cons (_, xs) => xs

(* Testi za pcl *)
val t19 = cons (7, cons (6, cons (5, cons (4, cons (3, nill ())))))
val t20 = car t19
val t21 = cdr t19

(* Funkcija stl *)
fun stl (Pcl (r as ref (Cons (h, t))), u) = (r := Cons (h, u))

(* Seznam samih enic *)
val t22 = cons (1, nill ())
val _ = stl (t22, t22)

(* Seznam 1, 2, 1, 2 ... - rezultat je v spremenljivki t24 *)
val t23 = cons (2, nill ())
val t24 = cons (1, t23)
val _ = stl (t23, t24)

(* Pridobimo seznam prvih n elementov *)
fun take n pclist =
  let
    fun take_acc 0 _ acc = rev acc
      | take_acc _ (Pcl (ref (Nil))) acc = rev acc
      | take_acc n (Pcl (ref (Cons (x, xs)))) acc =
          take_acc (n - 1) xs (x :: acc)
  in
    take_acc n pclist []
  end
val t25 = take 10 t22
val t26 = take 10 t24

(*Funkcija toList' vzame zraven še naravno število, s katerim povemo koliko elementov želimo. Ta lepo vrača rezultat.*)
fun toList' ((Pcl (ref Nil)), _) = []
	|toList' (_, 0) = []
	|toList' (x, n) = (car x)::(toList'(cdr x, n-1)) 
val t27 = toList' (t24, 10)
