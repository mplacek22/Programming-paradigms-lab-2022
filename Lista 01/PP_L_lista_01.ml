(*zad 1*)
let mirror4 (v1, v2, v3, v4) = (v2, v1, v4, v3);;

mirror4((1, 2, 3, 4)) = (2, 1, 4, 3);;
mirror4 ((true, 1, "dupa", 4.20)) = (1, true, 4.20, "dupa");;

(*zad 2*)
let rec remove (l, n) = 
	match (l, n) with
	(_::tail,0) -> tail
	|([],_) -> l
	|(head::tail,_) -> head :: remove(tail,n-1);;

remove([0; 1; 2; 3; 4; 5], 0) = [1; 2; 3; 4; 5];;
remove([0; 1; 2; 3; 4; 5], 3) = [0; 1; 2; 4; 5];;
remove([0; 1; 2; 3; 4; 5], 10) = [0; 1; 2; 3; 4; 5];;
remove([0; 1; 2; 3; 4; 5], -1) = [0; 1; 2; 3; 4; 5];;

(*zad 3*) 
let rec rotations l = 
match l with
	[] -> 0.
	|_::[] -> 1.
	|head::tail -> -1. *. head/. (List.hd tail) *. rotations(tail);;
	
rotations([]) = 0.;;
rotations([10.]) = 1.;;
rotations([10.; 5.; 1.]) = 10.;;
rotations([10.;20.]) = -0.5;;
rotations([10.; 20.; 5.]) = 2.;;
rotations([10.; 20.; 5.; 10.]) = -1.;;

