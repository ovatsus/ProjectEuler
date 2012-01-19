
/// greatest common divisor
let rec gcd n m =
    if m = 0L then 
        n 
    else 
        gcd m (n % m)

/// least common multiple
let lcm n m = 
    n * m / (gcd n m)

/// 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
/// What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
let pe5() = // result: 232792560
    { 1L .. 20L }
    |> Seq.reduce lcm
