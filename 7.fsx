
let isPrime x = 
    { 2L .. int64(sqrt(float x)) } 
    |> Seq.forall (fun i -> x % i <> 0L) 

let primes = 
    Seq.initInfinite int64 
    |> Seq.skip 2
    |> Seq.filter isPrime
    |> Seq.cache

/// By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
/// What is the 10 001st prime number?
let pe7() = // result: 104743
   primes |> Seq.skip 10000 |> Seq.head

