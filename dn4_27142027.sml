(*
** dn4_27142027.sml
** Author: Tadej Borovšak <tadeboro@gmail.com>
*)

(* Izracunaj sinus podanega kota *)
fun sin (a, eps) =
  let
    fun sin_t an st eps res =
      if abs an < eps
      then res
      else
        let
          val denom = Real.fromInt ((st + 1) * (st + 2))
          val fact = ~an * a * a / denom
        in
          sin_t fact (st + 2) eps (res + an)
        end
  in
    sin_t a 1 eps 0.0
  end
val t01 = sin (3.14 / 2.0, 0.0000001)

(* Izracunaj n-to Fibbonacijevo stevilo. Deluje le na pozitivnih stevilih. *)
fun fib 0 = 0
  | fib 1 = 1
  | fib n =
      let
        fun fib_t 0 a b = a + b
          | fib_t n a b = fib_t (n - 1) b (a + b)
      in
        fib_t (n - 2) 0 1
      end
val t02 = fib (0)
val t03 = fib (1)
val t04 = fib (7)

(* Sestavi seznam 3-terk *)
exception napacnaDolzina
fun sestavi3 (terka) =
  let
    fun sestavi_t ([], [], []) res = res
      | sestavi_t (x :: xs, y :: ys, z :: zs) res =
          sestavi_t (xs, ys, zs) ((x, y, z) :: res)
      | sestavi_t _ _ = raise napacnaDolzina
  in
    sestavi_t terka []
  end
val t05 = sestavi3 ([1, 2, 3], [true, false, true], ["ma", "ba", "ha"])
val t06 = sestavi3 ([1], [1, 2,3], [1, 2])
  handle napacnaDolzina => []

(* Razstavi seznam 3-terk v 3-terko seznamov *)
fun razstavi3 seznam =
  let
    fun razstavi_t [] (xs, ys, zs) = (rev xs, rev ys, rev zs)
      | razstavi_t ((x, y, z) :: seznam) (xs, ys, zs) =
          razstavi_t seznam (x :: xs, y :: ys, z :: zs)
  in
    razstavi_t seznam ([], [], [])
  end
val t07 = razstavi3 [(1, true, 0.0), (2, true, 5.0), (4, false, 7.0)]

(* Uporabi funkcijo na lihih položajih seznama *)
fun naLihih (f, seznam) =
  let
    fun apply _ [] res = rev res
      | apply f [x] res = apply f [] ((f x) :: res)
      | apply f (x :: _ :: xs) res = apply f xs ((f x) :: res)
  in
    apply f seznam []
  end
val t08 = naLihih ((fn x => x * 2), [1, 2, 3, 4, 5])

(* Preveri, če predikat drži na vseh elementih seznama *)
fun veljaNaVseh (_, []) = true
  | veljaNaVseh (f, (x :: xs)) =
      (f x) andalso veljaNaVseh (f, xs)
val t09 = veljaNaVseh (fn x => x > 3, [4, 5, 6])
val t10 = veljaNaVseh (fn x => x > 3, [1, 5, 6])

(* Preveri dolocene pogoje na vseh elementih seznama *)
fun vsiPozitivni sez = veljaNaVseh (fn x => x > 0, sez)
fun vsiLihi sez = veljaNaVseh (fn x => x mod 2 = 1, sez)

(* Standardne funkcije *)
fun map (_, []) = []
  | map (f, x :: xs) = (f x) :: map (f, xs)

fun filter (f, []) = []
  | filter (f, x :: xs) = if f x then x :: filter (f, xs) else filter (f, xs)

fun fold (_, acc, []) = acc
  | fold (f, acc, x :: xs) = fold (f, f (acc, x), xs)

fun sum sez = fold (fn (a, x) => a + x, 0, sez)

fun preslikaj sez = map (sum, sez)
