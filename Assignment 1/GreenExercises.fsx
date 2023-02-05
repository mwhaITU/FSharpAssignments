// 1.1
let sqr x = x * x
sqr 5

// 1.2
let pow x n = System.Math.Pow(x, n)
pow 2 5

// 1.3
let rec sum =
    function
    | 0 -> 0
    | n -> n + sum(n - 1);;
sum 10

// 1.4
let rec fib = 
    function
    | 0 -> 0   
    | 1 -> 1
    | n -> fib(n-1) + fib(n-2)
fib 20

// 1.5
let dup (s : string) = s + s
dup "Hi "

// 1.6
let rec dupn (s : string) (n : int) = 
    match n with
    | 1 -> s
    | _ -> s + dupn s (n-1)
dupn "Hi " 3

// 1.7
let rec bin n k = 
    match n, k with
    | (n, k) when k = 0 -> 1
    | (n, k) when n = k -> 1
    | _ -> (bin (n-1) (k-1)) + (bin (n-1) k)
bin 4 2