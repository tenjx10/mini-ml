(* Recursion and integer arithmetic. *)

let rec fact n = if n <= 1 then 1 else n * fact (n - 1)

let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let _ = assert (fact 10 = 3628800)
let _ = assert (fib 15 = 610)
let _ = assert (gcd 84 36 = 12)
