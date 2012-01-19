
let isPalindrome n =
    let rec isPalindrome (str:string) i j =
        i >= j || str.[i] = str.[j]  && isPalindrome str (i+1) (j-1)
    let str = string n
    isPalindrome str 0 (str.Length - 1)

/// A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 99.
/// Find the largest palindrome made from the product of two 3-digit numbers.
let pe4() = // result: 906609    
    seq { for i in 100..999 do for j in 100..999 -> i*j }
    |> Seq.sortBy (fun i -> -i)
    |> Seq.filter isPalindrome
    |> Seq.head
