(* 1.1 *)
10;

5 + 3 + 4;

9 - 1;

6 div 2;

2*4 + 4-6;

val a = 3;

val b = a+1;

a = b;

if b > a andalso b < a*b
then b
else a;

case (a, b) of
    (4, _) => 6
 |  (_, 4) => 6+7+a
 |  _ => 25;

2 + (if b > a then b else a);

(a + 1) * (if a > b
	   then a
	   else if a < b
	   then b
	   else ~1); (* Note the -1 *)
	       
(* 1.2 - Note the div instead of real *)
val numer = (5 + 4 + (2 - (3 - (6 + 4 div 5))));
val denom = (3 * (6-2) * (2-7));
numer div denom;

(* 1.3 - Note that all cases must be covered, unlike the scheme version *)
fun sum_larger_two(a: int, b: int, c: int) = 
  if a <= b andalso a <= c
  then b+c
  else if b <= a andalso b <= c
  then a+c 
  else a+b 

val t1 = sum_larger_two(1, 0, 0) = 1;
       
				      (* 1.4 - Note the op *)
fun a_plus_abs_b(a, b) =
  (if b > 0 then op+ else op-)(a, b)
val t2 = (a_plus_abs_b(0, 1) = 1);

(* 1.5 *)
fun p() = p()
fun test(x, y) = if x = 0 then 0 else y
