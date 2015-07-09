let rec f l = 
 match l with
 [] -> 1
 |h::q -> f q
