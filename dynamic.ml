(*
**
** ocaml module Dynamic
**
** Description: Dynamic programming examples
**
** Author: Eray Ozkural (exa) <erayo@cs.bilkent.edu.tr>, (C) 2003
**
** Copyright: See COPYING file that comes with this distribution
**
*)

open Array
open Printf
let print_intarray a = Array.iter (fun x->printf "%d " x) a
let print_intmatrix a = Array.iter (fun x->print_intarray x;printf"\n") a

(*let print_introw a i = for i=0 
let print_intmatrix a = Array.iter (fun x->print_intarray x;printf"\n") a*)

(* compute optimal parenthesization of a matrix product sequence
 * in O(n^3) time. The quintessential dynamic programming example. 
 * p is an array of matrix sizes where A_i has p[i].p[i+1] size
 *)

let matrix_chain_order p =
  let n = length p - 1 in (* n matrices in product sequence *)
  let m = make_matrix n n 0 (* m[i,j] = min. cost of computing  Ai..Aj *)
  and s = make_matrix n n 0 (* s[i,j] = optimal parenthesization of Ai..Aj *)
  in (* m has already minimum cost of chains of length 1 = 0 *)
    for l = 2 to n do (* length runs from 2 to n *)
      for i = 0 to n - l do (* start end for minimum chains of length l *)
	let j = i + l - 1 in
	  (*printf "i=%d j=%d \n" i j;*)
	  m.(i).(j) <- max_int; (* initialize cost to +infinite *)
	  for k = i to j-1 do (* k in possible paren. split points *)
	    let q = m.(i).(k) + m.(k+1).(j) + p.(i)*p.(k+1)*p.(j+1) in
	      if q < m.(i).(j) then (* is cost lower than current? *)
		begin
		  m.(i).(j) <- q; (* update cost *)
		  s.(i).(j) <- k  (* determine optimal parenthesization *)
		end
	      else ()
	  done
      done
    done;
    (m,s)

let rec print_optimal_parens s i j =
  if j > i then
    begin
      printf "(";
      print_optimal_parens s i s.(i).(j);
      print_optimal_parens s (s.(i).(j)+1) j;
      printf ")"
    end
  else
    printf "A_%d" i

(* longest common subsequence *)
(* compute the longest common subsequences of input strings x and y *)
(* c[i,j]=length of LCS in prefix X_i Y_j *)
(* b[i,j]=traversal info to construct subsequences *)

type subseq_arrow = NoArrow | UpLeft | Up | Left

let lcs_length x y =
  let m = length x
  and n = length y in
  let c = make_matrix (m+1) (n+1) 0 
  and b = make_matrix (m+1) (n+1) NoArrow 
  in (* c[0..m-1,0] and c[0,0..n-1] already 0 *)
    for i = 1 to m do
      for j = 1 to n do
	if x.(i-1) = y.(j-1) then (*i starts from 1, array ix starts from 0!*)
	  begin
	    c.(i).(j) <- c.(i-1).(j-1) + 1;
	    b.(i).(j) <- UpLeft
	  end
	else
	  if c.(i-1).(j) > c.(i).(j-1) then
	    begin
	      c.(i).(j) <- c.(i-1).(j);
	      b.(i).(j) <- Up
	    end
	  else
	    begin
	      c.(i).(j) <- c.(i).(j-1);
	      b.(i).(j) <- Left
	    end
      done
    done;
    (c, b)

let rec print_lcs b x i j =
  if i = 0 || j = 0 then () else
    match b.(i).(j) with 
	UpLeft -> print_lcs b x (i-1) (j-1); printf "%d " x.(i-1) 
      | Up -> print_lcs b x (i-1) j
      | Left -> print_lcs b x i (j-1)
      | NoArrow -> ()
	  

let print_saarray a =
  Array.iter
    (fun x-> match x with
	 NoArrow -> printf "NoArrow "
       | UpLeft  -> printf "UpLeft  "
       | Left    -> printf "Left    "
       | Up      -> printf "Up      ") a
let print_samatrix a = Array.iter (fun x->print_saarray x;printf"\n") a

let test () =
  printf "dynamic programming test\n";
  begin
    let p = [| 5; 10; 3; 12; 5; 50; 6 |] in
    let (m,s)=matrix_chain_order p in
      printf "p="; print_intarray p; printf "\n";
      printf "m=\n"; print_intmatrix m; printf "\n";
      printf "s=\n"; print_intmatrix s; printf "\n";
      printf "parenthesization="; print_optimal_parens s 0 (length p-2);
      printf "\n"
  end;
  begin
    let x = [| 1;0;0;1;0;1;0;1 |]
    and y = [| 0;1;0;1;1;0;1;1;0 |] in
    let (c,b)= lcs_length x y in
      printf "x="; print_intarray x; printf "\n";
      printf "y="; print_intarray y; printf "\n";
      printf "c=\n"; print_intmatrix c; printf "\n";
      printf "b=\n"; print_samatrix b; printf "\n";
      printf "LCS(x,y)="; print_lcs b x (length x) (length y);
      printf "\n"
  end
