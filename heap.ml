open Array
open Printf

type heap = { a: int array; mutable size: int }

let swap a i j = let t = a.(i) in a.(i) <- a.(j); a.(j) <- t
let print_intarray a = Array.iter (fun x->printf "%d " x) a

(* arrays start from 0 *)
(* a heap array, i index *)

let parent i = (i-1) / 2
let left i = 2*i + 1
let right i = 2*i + 2

let rec heapify h i =
  let l = ref (left i)
  and r = ref (right i)
  and largest = ref 0
  and a = h.a in
    (*printf "i=%d, l=%d, r=%d, hs=%d\n" i !l !r h.size;*)
    if !l < h.size && a.(!l) > a.(i)
    then largest := !l
    else largest := i;
    if !r < h.size && a.(!r) > a.(!largest)
    then largest := !r else ();
    if !largest <> i then
      begin
	swap a i !largest;
	heapify h !largest
      end
    else ()

let build_heap a =
  let h = { a = a; size = length a} in
    for i = (length a)/2-1 downto 0 do heapify h i done;
    h

let sort a =
  let h = build_heap a in
    for i = length a-1 downto 1 do
      swap a 0 i;
      h.size <- h.size-1;
      heapify h 0
    done

exception Underflow

let extract_max h =
  if h.size < 1 then raise Underflow else
    let a = h.a in
    let max = a.(0) in
      a.(0) <- a.(h.size-1);
      h.size <- h.size - 1;
      heapify h 0;
      max
      
let insert h key =
  h.size <- h.size + 1;
  let i = ref (h.size - 1) in (* last node *)
  let a = h.a in
    while !i > 0 && a.(parent !i) < key do
      a.(!i) <- a.(parent !i);
      i := parent !i
    done;
    a.(!i) <- key

let test () =
  printf "heap test\n";
  begin
    let a = [| 5; 13; 2; 25; 7; 16; 20; 8; 4 |] in
      sort a;
      print_intarray a; printf "\n"
  end;
  begin
    let h = {a = [| 15;13;9;5;12;8;7;4;0;6;2;1;0;0;0 |]; size=12} in
      insert h 3
  end;
  begin
    let h = {a = [| 15;13;9;5;12;8;7;4;0;6;2;1 |]; size=12} in
    let t = extract_max h in
      printf "top is %d\n" t
  end

