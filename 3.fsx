
/// The prime factors of 13195 are 5, 7, 13 and 29.
/// What is the largest prime factor of the number 600851475143 ?
let pe3() = // result: 6857
    let isPrime x = 
        { 2L .. int64(sqrt(float x)) } 
        |> Seq.forall (fun i -> x % i <> 0L) 
    let allPrimes = 
        Seq.initInfinite int64 
        |> Seq.skip 2
        |> Seq.filter isPrime
        |> Seq.cache
    let rec factors x primes = seq {
        if x <> 1L then
            let factor = Seq.head primes
            if x % factor = 0L then
                yield factor
                yield! factors (x / factor) (Seq.skip 1 primes)
            else
                yield! factors x (Seq.skip 1 primes)
    }
    factors 600851475143L allPrimes
    |> System.Linq.Enumerable.Last
