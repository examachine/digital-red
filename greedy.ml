
open Printf
let print_intarray a = Array.iter (fun x->printf "%d " x) a
let print_intlist a = List.iter (fun x->printf "%d " x) a
let print_pairarray a = Array.iter (fun (x,y)->printf "(%d,%d) " x y ) a

let compare_activity a b = compare (snd a) (snd b)

let activity_selector a = 
  Array.sort compare_activity a;
  let n = Array.length a in
  let schedule = ref [0] in
  let j = ref 0 in
    for i = 1 to n-1 do
      if fst a.(i) > snd a.(!j) then
	begin
	  schedule := !schedule @ [i];
	  j := i
	end
      else ()
    done;
    schedule
  

let test () =
  printf "greedy programming test\n";
  begin
    let a = [| (3,8);(0,6);(1,4);(2,13);(5,9);(8,11);
	       (3,5);(8,12);(12,14);(5,7);(6,10) |] in
      printf "a="; print_pairarray a; printf "\n";
      let s=activity_selector a in
	printf "s="; print_intlist !s; printf "\n"
  end

