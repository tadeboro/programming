(*
** dn5_27142027.sml
** Author: Tadej Borovšak <tadeboro@gmail.com>
*)

fun obstaja (_, []) = false
  | obstaja (f, (x :: xs)) = f x orelse obstaja (f, xs)

fun zaVse (_, []) = true
  | zaVse (f, (x :: xs)) = f x andalso zaVse (f, xs)

fun filter (p,[]) = []
  | filter (p,x::xs) = if p(x) then x::filter(p, xs) else filter(p, xs)

fun map (f, []) = []
  | map (f, x::xs) = f(x) :: map(f, xs)

fun clan (e, sez) = obstaja (fn x => e = x, sez)
val t01 = clan (1, [1, 2, 3])
val t02 = clan (1, [3, 4, 5])


fun vstavi (e, sez) = if clan (e, sez) then sez else e :: sez
val t03 = vstavi (1, [1, 2, 3])
val t04 = vstavi (1, [2, 3, 4])

fun jePodmnozica (a, b) =
  zaVse (fn x => obstaja (fn y => x = y, b), a)
val t05 = jePodmnozica ([1, 2], [1, 2, 3])
val t06 = jePodmnozica ([1, 4], [1, 2, 3])

fun staLoceni (a, b) =
  zaVse (fn x => zaVse (fn y => x <> y, b), a)
val t07 = staLoceni ([1, 2], [3, 4])
val t08 = staLoceni ([1, 3], [3, 4])

fun presek (a, b) =
  filter (fn x => clan (x, a), b)
val t09 = presek ([1, 2, 3], [2, 3, 4])
val t10 = presek ([1, 2, 3], [5, 7, 4])

fun razlika (a, b) =
  filter (fn x => not (clan (x, b)), a)
val t11 = razlika ([1, 2, 3], [2])
val t12 = razlika ([1, 2, 3], [5, 7, 4])

fun zamenjaj (a, b, sez) =
  map (fn x => if x = a then b else x, sez)
val t13 = zamenjaj (1, 2, [1, 1, 2])
val t14 = zamenjaj (3, 2, [1, 1, 2])

fun kartproc (a, b) =
  map (fn x => map (fn y => (x, y), b), a)
val t15 = kartproc ([1, 2, 3], [4, 5, 6])

fun unija (a, b) = a @ razlika (b, a)
val t16 = unija ([1, 2, 3], [3, 4, 5])

fun odvod sez =
  filter (fn (a, _) => a <> 0, map (fn (a, n) => (a * n, n - 1), sez))
val t17 = odvod [(3,2),(~4,1),(5,0)]
