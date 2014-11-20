(*
** dn6_27142027.sml
** Author: Tadej Borovšak <tadeboro@gmail.com>
*)

(* Podane funkcije *)
fun foldl _ e [] = e
  | foldl f e (x::xs) = foldl f (f(x, e)) xs

fun foldr _ e [] = e
  | foldr f e (x::xs) = f(x, foldr f e xs)

fun map _ [] = []
  | map f (x::xs) = (f x) :: map f xs

fun filter _ [] = []
  | filter p (x::xs) = if p(x) then x::(filter p xs) else filter p xs


(* Izračun integrala funkcije s trapezno metodo an intervalu [a, b] in
** z delitvijo na n intervalov. *)
fun integral f (a, b, n) =
  let
    (* Ploščina trapeza v delitvi *)
    fun pl_trap (y1, y2) = (y1 + y2) / 2.0 * (b - a) / n
    (* Izračun delitvene točke x_i na x osi *)
    fun izr_x i = Real.fromInt i / n * (b - a) + a
    (* Izračun vrednosti funkcije v točkah x_0, ..., x_n *)
    val ys = map f (List.tabulate (trunc n + 1, izr_x))
    (* Pari točk (y_i, y_{i+1}) *)
    val pari = ListPair.zip (ys, tl ys)
  in
    foldl (fn (p, v) => (pl_trap p) + v) 0.0 pari 
  end
val t01 = integral (fn x => x) (0.0, 1.0, 10.0)
val t02 = integral (fn x => x * x) (0.0, 1.0, 10.0)

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
val t11 = uredi op< [3, 4, 2, 5]
val t12 = uredi op> [3, 4, 2, 5]

(* Prešteje besede v nizu. *)
fun prestejBesede niz =
  let
    (* Začetek besede označuje par (true, false) *)
    fun je_zacetek (a, b) = a andalso (not b)
    (* Presledki so v seznamu označeni s true *)
    val presledki = true :: (map Char.isSpace (explode niz))
    (* Pari (bool, bool) dveh sosednjih znakov *)
    val pari = ListPair.zip (presledki, (tl presledki))
  in
    List.foldl (fn (p, s) => if je_zacetek p then s + 1 else s) 0 pari
  end
val t21 = prestejBesede " tsdf da fd f" = 4
val t22 = prestejBesede "tsdf da fd " = 3

(* Produkt matrik *)
fun produktMatrik a b =
  let
    fun transponiraj ([] :: _) = []
      | transponiraj vrstice = map hd vrstice :: transponiraj (map tl vrstice)
    fun skalarni [] [] = 0
      | skalarni (x :: xs) (y :: ys) =
          x * y + (skalarni xs ys)
  in
    map (fn y => map (fn x => skalarni x y) a) (transponiraj b)
  end
val t31 = produktMatrik [[0, 7], [8, 9]] [[1, 2, 3], [4, 5, 6]]

(* Vrni potenčno množico množice. *)
fun potencna [] = [[]]
  | potencna (x :: xs) =
      let
        fun pripni y = x :: y
      in
        (potencna xs) @ map pripni (potencna xs)
      end
val t41 = potencna [1, 2, 3]

(* Implementiraj map s pomočjo foldr *)
fun map1 f xs = foldr (fn (x, l) => (f x) :: l) [] xs
val t51 = map1 (fn x => x + 1) [1, 2, 3]

(* Implementiraj filter s pomočjo foldr *)
fun filter1 f xs = foldr (fn (x, l) => if f x then x :: l else l) [] xs
val t61 = filter1 (fn x => x < 3) [1, 2, 3, 4]

(* Implementiraj treemap *)
datatype btree = Nil | Node of {left: btree, value: int ,right: btree}
fun treemap _ Nil = Nil
  | treemap f (Node {left = l, value = v, right = r}) =
      Node {left = treemap f l, value = f v, right = treemap f r}
val t71 = Node {left = Nil, value = 4, right = Node {left = Nil, value = 1, right = Nil}}
val t72 = treemap (fn x => x + 1) t71
