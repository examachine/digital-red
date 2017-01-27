(*
**
** ocaml module LinearSort
**
** Description: Three linear time sorting algorithms!
**
** Author: Eray Ozkural (exa) <erayo@cs.bilkent.edu.tr>, (C) 2003
**
** Copyright: See COPYING file that comes with this distribution
**
*)

open Printf

let exchange a i j = let t = a.(i) in a.(i) <- a.(j); a.(j) <- t
let print_intarray a = Array.iter (fun x->printf "%d " x) a

let minimum a =
  let min = ref a.(0) in
    for i = 1 to Array.length a - 1 do
      if !min > a.(i) then min := a.(i) else ()
    done;
    !min

exception Done

let partition a p r =
  let x = a.(p) (* select pivot *)
  and i = ref p
  and j = ref (r: int) in
    try 
      while true do
	(* downward scan *)
	while a.(!j) > x do j := !j - 1; done;
 	(* upward scan *)
	while a.(!i) < x do i := !i + 1; done;
	if !i < !j then
	  exchange a !i !j
	else
	  raise Done
      done; !j (* never reached! *)
    with Done -> !j

(* make sure you initialize your random number generator prior to call! *)
let randomized_partition a p r =
  let i = p + (Random.int (r-p)) in
    (*printf "p=%d r=%d i=%d a[i]=%d \n" p r i a.(i);*)
    exchange a p i;
    partition a p r

let rec randomized_select (a: int array) p r i =
  if p = r then a.(p)
  else
    begin
      Random.self_init ();
      let q = randomized_partition a p r in
      let k = q - p + 1 in
	(*printf "partitioned array is = "; print_intarray a; printf "\n";*)
	if i <= k then
	  randomized_select a p q i
	else
	  randomized_select a (q+1) r (i-k)
    end

let median select a =
  select a 0 (Array.length a - 1) (Array.length a / 2)

let test () =
  printf "order statistics test \n";
  begin
    let a = [| 5; 13; 2; 25; 7; 16; 20; 8; 4 |] in
      printf "a="; print_intarray a; printf "\n";
      printf "minimum is %d \n" (minimum a);
      printf "using randomized select \n";
      printf "median is %d \n" (median randomized_select a);
      printf "3rd smallest elt is %d \n" (randomized_select a 0 8 3);
      printf "6th smallest elt is %d \n" (randomized_select a 0 8 6)
  end;
  begin
    let a = [| 5; 13; 2; 25; 7; 16; 20; 8; 4 |] in
      printf "a="; print_intarray a; printf "\n";
      printf "using median-of-medians select \n";
      printf "median is %d \n" (median randomized_select a);
      printf "3rd smallest elt is %d \n" (randomized_select a 0 8 3);
      printf "6th smallest elt is %d \n" (randomized_select a 0 8 6)
  end
