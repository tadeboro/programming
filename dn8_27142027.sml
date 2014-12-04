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
structure IntOrd :> ORD =
struct
  (* Interni tip *)
  type t = int

  (* Primerjanje dveh celih števil *)
  val cmp = Int.compare
end

functor OrdListSetFn (ordParam: ORD) :> ORDSET
  where type elem = ordParam.t =
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
val t12 = SS.member (2, t12)
val t13 = SS.toList t12

functor TreeMapFn (ordParam: ORD) :> ORDMAP
  where Key.t = ordParam.t =
struct
  (* Structures and types *)
  structure Key = ordParam
  datatype 'a tree = Empty
                   | Node of Key.t * 'a * 'a tree * 'a tree
  type 'a map = 'a tree

  (* Implementation *)
  val empty = Empty
  fun find (_, Empty) = NONE
    | find (a, Node (k, e, l, r)) =
        case Key.cmp (a, k) of
          LESS    => find (a, l)
        | EQUAL   => SOME e
        | GREATER => find (a, r)
  fun insert (k, e, Empty) = Node (k, e, Empty, Empty)
    | insert (k, e, t as Node (kt, et, l, r)) =
        case Key.cmp (k, kt) of
          LESS    => insert (k, e, l)
        | EQUAL   => t
        | GREATER => insert (k, e, r)
end
