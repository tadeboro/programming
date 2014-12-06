(*
** dn8_27142027.sml
** Author: Tadej Borovšak <tadeboro@gmail.com>
*)

signature SET =
sig
  type ''a set
  val insert : (''a * ''a set) -> ''a set
  val member : (''a * ''a set) -> bool
  val empty : ''a set
  val toList : ''a set -> ''a list
end

signature ORD =
sig
  type t
  val cmp : t * t -> order
end

signature ORDSET =
sig
  structure Order : ORD

  type elem = Order.t
  type set

  val insert : (elem * set) -> set
  val member : (elem * set) -> bool
  val empty : set
  val toList : set-> elem list
end

signature ORDMAP =
sig
  structure Key : ORD
  type 'a map

  val empty : 'a map
  val find : Key.t * 'a map -> 'a option
  val insert : Key.t * 'a * 'a map -> 'a map
end


signature GRAPH =
sig
  type node
  type edge
  type graph

  val addNode : node * graph -> graph
  val addEdge : node * edge * graph -> graph
  val getEdges : node * graph -> edge list
  val empty : graph
end

(* Implementacija množice *)
structure ListSet :> SET =
struct
  (* Osnovna podatkovna struktura je seznam. Učinkoviteje bi bilo uporabljati
  ** binarno iskalno drevo, ampak za začetek bo seznam OK. *)
  type ''a set = ''a list

  (* Preveri, če je element a v množici *)
  fun member (_, []) = false
    | member (a, x :: xs) = a = x orelse member (a, xs)

  (* Vstavljanje elementov v množico. *)
  fun insert (el, s) = if member (el, s) then s else el :: s

  (* Vrni prazno množico. *)
  val empty = []

  (* Vrni množico v obliki seznama. *)
  fun toList s = s
end

(* Uporaba *)
structure LS = ListSet
val t01 = LS.insert (3, LS.insert (2, LS.insert (1, LS.empty)))
val t02 = LS.insert (2, t01)
val t03 = LS.toList (t02)
val t04 = LS.member (1, t02)

(* Implementacija urejenosti *)
structure IntOrd : ORD =
struct
  (* Interni tip *)
  type t = int

  (* Primerjanje dveh celih števil *)
  val cmp = Int.compare
end

functor OrdListSetFn (ordParam: ORD) :> ORDSET
  where type Order.t = ordParam.t =
struct
  (* Definicija tipov *)
  structure Order = ordParam
  type elem = Order.t
  type set = elem list

  (* Vstavljanje elementov *)
  fun insert (a, []) = [a]
    | insert (a, x :: xs) =
        case Order.cmp (a, x) of
          LESS    => a :: x :: xs
        | EQUAL   => x :: xs
        | GREATER => x :: insert (a, xs)

  (* Preverjanje prisotnosti v množici *)
  fun member (_, []) = false
    | member (a, x :: xs) =
        case Order.cmp (a, x) of
          LESS    => false
        | EQUAL   => true
        | GREATER => member (a, xs)

  (* Prazna množica *)
  val empty = []

  (* pretvorba v seznam *)
  fun toList s = s
end

(* Testi *)
structure SS = OrdListSetFn (IntOrd)
val t10 = SS.insert (3, SS.insert (2, SS.insert (1, SS.empty)))
val t11 = SS.insert (2, t10)
val t12 = SS.member (2, t11)
val t13 = SS.toList t11

functor TreeMapFn (ordParam: ORD) :> ORDMAP
  where type Key.t = ordParam.t =
struct
  structure Key = ordParam
  datatype 'a tree = Empty
                   | Node of Key.t * 'a * 'a tree * 'a tree
  type 'a map = 'a tree

  val empty = Empty
  fun find (_, Empty) = NONE
    | find (a, Node (k, e, l, r)) =
        case Key.cmp (a, k) of
          LESS    => find (a, l)
        | EQUAL   => SOME e
        | GREATER => find (a, r)
  fun insert (k, e, Empty) = Node (k, e, Empty, Empty)
    | insert (k, e, Node (kt, et, l, r)) =
        case Key.cmp (k, kt) of
          LESS    => Node (kt, et, insert (k, e, l), r)
        | EQUAL   => Node (kt, e, l, r)
        | GREATER => Node (kt, et, l, insert (k, e, r))
end

structure TS = TreeMapFn (IntOrd)
val t20 = TS.insert (3, "c", TS.insert (1, "a", TS.empty))
val t21 = TS.find (2, t20)
val t22 = TS.find (3, t20)

(* Graf *)
(* Funktor GraphFn kot parametra prejme dva tipa elementov (eno
** za vozlišča in eno za povezave), ki se jih da medsebojno
** primerjati. *)
functor GraphFn (structure N : ORD
                 structure E : ORD) : GRAPH
  where type node = N.t and type edge = E.t =
struct
  type node = N.t
  type edge = E.t

  structure T =
  struct
    type t = node * edge
    fun cmp ((n1, e1), (n2, e2)) =
      case N.cmp (n1, n2) of
        LESS    => LESS
      | EQUAL   => E.cmp (e1, e2)
      | GREATER => GREATER
  end
  structure NS = OrdListSetFn (N)
  structure ES = OrdListSetFn (T)

  type graph = NS.set * ES.set

  (* Pomožne funkcije *)
  fun eqNode a b = N.cmp (a, b) = EQUAL
  fun snd (_, x) = x

  (* Vmesnik *)
  fun addNode (n, g as (ns, es)) =
    if NS.member (n, ns) then g else (NS.insert (n, ns), es)

  fun addEdge (n, e, g as (ns, es)) =
    if ES.member ((n, e), es) then g else (ns, ES.insert ((n, e), es))

  fun getEdges (n, g as (ns, es)) =
    List.map snd (List.filter (fn (y, _) => eqNode n y) (ES.toList es))

  val empty = (NS.empty, ES.empty)
end

(* Povezave so urejene najprej po končnem vozlišču, nato pa
** po vrednosti na povezavi. *)
functor EdgeOrdFn (structure N : ORD
                   structure V : ORD) : ORD
  where type t = N.t * V.t =
struct
  type t = N.t * V.t
  fun cmp ((n1, e1), (n2, e2)) =
    case N.cmp (n1, n2) of
      LESS    => LESS
    | EQUAL   => V.cmp (e1, e2)
    | GREATER => GREATER
end

(* Vzorčni primer: vozlišča so označena s števili, na povezavah pa
** se nahajajo znaki (uporabno za predstaviti DFA). *)
structure NodeOrd : ORD =
struct
  type t = int
  val cmp = Int.compare
end
structure CharOrd : ORD =
struct
  type t = char
  val cmp = Char.compare
end
structure EdgeOrd = EdgeOrdFn (structure N = NodeOrd
                               structure V = CharOrd)
structure G = GraphFn (structure N = NodeOrd
                       structure E = EdgeOrd)
(* Funkcija, ki preverja pripadnost besede jeziku avtomata g.
**
** Funkcjia sprejme neko mešanico med DFA in NFA. Dovoljujemo,
** da delta funkcija avtomata ni totalna, ne dovoljujemo pa
** nedeterminističnih odločitev v stanjih. *)
fun dfa (w, g, s, f) =
  let
    fun dfa' ([], _, s, f) = List.exists (fn x => x = s) f
      | dfa' (c :: cs, g, s, f) =
          let
            val next = G.getEdges (s, g)
            val s1 = List.find (fn (_, v) => c = v) next
          in
            case s1 of
              NONE        => false
            | SOME (x, _) => dfa' (cs, g, x, f)
          end
  in
    dfa' (explode w, g, s, f)
  end

(* Avtomat, ki sprejema regularni izraz a+|b+|c+ *)
val t30 = G.addNode (1,
          G.addNode (2,
          G.addNode (3,
          G.addNode (4,
          G.empty))))
val t31 = G.addEdge (1, (2, #"a"),
          G.addEdge (1, (3, #"b"),
          G.addEdge (1, (4, #"c"),
          G.addEdge (2, (2, #"a"),
          G.addEdge (3, (3, #"b"),
          G.addEdge (4, (4, #"c"),
          t30))))))
val t32 = dfa ("aaaa", t31, 1, [2, 3, 4]) = true
val t33 = dfa ("bbb", t31, 1, [2, 3, 4]) = true
val t34 = dfa ("cc", t31, 1, [2, 3, 4]) = true
val t35 = dfa ("", t31, 1, [2, 3, 4]) = false
val t36 = dfa ("abc", t31, 1, [2, 3, 4]) = false

(* Še en primer grafa, ki ima na v vozliščih nize znakov,
** na povezavah pa številske uteži. *)
structure N : ORD =
struct
  type t = string
  val cmp = String.compare
end
structure V : ORD =
struct
  type t = real
  val cmp = Real.compare
end
structure E = EdgeOrdFn (structure N = N
                         structure V = V)
structure GR = GraphFn (structure N = N
                        structure E = E)
(* Testi *)
val t40 = GR.addNode ("a",
          GR.addNode ("b",
          GR.addNode ("c",
          GR.addNode ("d",
          GR.empty))))
val t41 = GR.addEdge ("a", ("b", 1.2),
          GR.addEdge ("a", ("c", 3.4),
          GR.addEdge ("a", ("d", 5.6),
          GR.addEdge ("b", ("b", 7.8),
          GR.addEdge ("c", ("c", 1.4),
          GR.addEdge ("d", ("d", 5.9),
          t40))))))
val t42 = GR.getEdges ("a", t41)
val t43 = GR.getEdges ("c", t41)
