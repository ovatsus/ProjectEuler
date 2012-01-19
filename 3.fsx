
let isPrime x = 
    { 2L .. int64(sqrt(float x)) } 
    |> Seq.forall (fun i -> x % i <> 0L) 

let primes = 
    Seq.initInfinite int64 
    |> Seq.skip 2
    |> Seq.filter isPrime
    |> Seq.cache

let factors x =
    let rec factors x primes list = 
        if x = 1L then
            list
        else
            let factor = Seq.head primes
            if x % factor = 0L then
                factors (x / factor) (Seq.skip 1 primes) (factor::list)
            else
                factors x (Seq.skip 1 primes) list
    factors x primes []

/// The prime factors of 13195 are 5, 7, 13 and 29.
/// What is the largest prime factor of the number 600851475143 ?
let pe3() = // result: 6857
    factors 600851475143L |> List.head
