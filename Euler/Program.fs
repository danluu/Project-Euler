//Using this to learn F#. Solutions are probably terrible :-)

#if INTERACTIVE
    #r "FSharp.PowerPack.dll";;
    #r "C:\Users\d\Documents\My Dropbox\Visual Studio 2010\xunit-1.6.1\Xunit.dll";;
#endif

open Xunit

let MustEqual expected actual = Assert.Equal(expected,actual)

let benchmark f x = 
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let res = f x
    timer.Stop()
    printfn "Duration (ms): %i" timer.ElapsedMilliseconds
    res
 
module Problem1 =
    

    let ans = seq {for i in 1 .. 999 do if i % 3 = 0 || i % 5 = 0 then yield i} |> Seq.sum

    //curiously, this 2nd method is faster if we increase the range enough that it actually takes a significant of time to run {1I..999999I}
    //instead of {1..999}: 
    //Real: 00:00:02.981, CPU: 00:00:02.979, GC gen0: 68, gen1: 1, gen2: 0
    //Real: 00:00:02.314, CPU: 00:00:02.324, GC gen0: 61, gen1: 1, gen2: 0
    let altAns = {1..999} |> Seq.filter (fun n -> n % 3 = 0 || n % 5 = 0) |> Seq.sum

    let altAns2 = {1..999} |> Seq.sumBy (fun n -> if n % 3 = 0 || n % 5 = 0 then n else 0)

(*    let benchAns = {1I..9999999I} |> Seq.filter (fun n -> n % 3I = 0I || n % 5I = 0I) |> Seq.sum
    let benchAns2 = [1I..9999999I] |> List.filter (fun n -> n % 3I = 0I || n % 5I = 0I) |> List.sum
    let benchAns3 = {1I..9999999I} |> Seq.sumBy (fun n -> if n % 3I = 0I || n % 5I = 0I then n else 0I)*)

module Problem2 = 
    let ans = Seq.unfold (fun (n,m) -> Some(m,(m, n+m))) (0,1) |> Seq.filter(fun n -> n % 2 = 0) |> Seq.takeWhile((>) 4000000)  |> Seq.sum
    //note: every third number is even, so we could save a constant factor here.

//    let benchSum = Seq.unfold (fun (n,m) -> Some(m,(m, n+m))) (0I,1I) |> Seq.filter(fun n -> n % 2I = 0I) |> Seq.takeWhile((>) (2I**100000))  |> Seq.sum

module AlternateProblem2 =
    let rec fib n m = seq{yield m; yield! fib m (n+m)} 
    let ans = fib 0 1 |> Seq.filter(fun n -> n % 2 = 0) |> Seq.takeWhile((>) 4000000)  |> Seq.sum


//    let benchSum = fib 0I 1I |> Seq.filter(fun n -> n % 2I = 0I) |> Seq.takeWhile((>) (2I**100000))  |> Seq.sum

module Problem3 = 
    //Lets see if this stupid brute force method is fast enough
    let isPrime n = {2L .. n-1L} |> Seq.forall(fun m -> n%m <> 0L)    
    let primes = seq {for i in 1L .. 600851475142L do if isPrime i then yield i} 
    let multiples = primes |> Seq.filter(fun n -> 600851475143L % n = 0L)
//    let biggest = Seq.max multiples way too slow!

    //if this is too slow, we can stop at sqrt. Could also inc by 2, but I suspect that's actually slower

    let rec factors n m ms =
        if n % m = 0L then
            factors (n/m) m (m::ms)
        elif n > m then
            factors n (m+1L) ms
        else
            ms

    let ans = factors 600851475143L 2L [] |> List.head

module Problem4 =
    //we have to write (fun n -> System.String n) instead of just System.String because class constructors can't be used without arguments :-(
    let isPalindrome n = n.ToString() |> Array.ofSeq |> Array.rev |> (fun n -> System.String n) |> System.Convert.ToInt32 = n
    let min = 100
    let max = 999

    //ridiculously inefficient way of generating all pairs
    let brutePairs = seq{for i in min .. max do 
                            for j in min .. max do
                                yield i * j}
    let ans = brutePairs |> Seq.filter isPalindrome |> Seq.max   
    
//better palindrome testing from http://diditwith.net/2008/05/06/YAPESProblemFourAlternateSolution.aspx
module BetterProblem4 =
    let min = 100
    let max = 999

    let reverse n =
        let rec loop x acc =
            if x = 0 then acc
            else loop (x/10) (acc*10 + (x%10))
        loop n 0

    let isPalindrome n =
        n = reverse n

    //inefficient way of generating all pairs
    let brutePairs = seq{for i in min .. max do 
                            for j in min .. max do
                                yield i * j}

    let ans = brutePairs |> Seq.filter isPalindrome |> Seq.max   

module Problem5 =
    //lcm = product / gcd; could get rid of the % op by using Euclid's, but that would probably be slower
    //could speed up by checking to see which is bigger and possibly reversing args
    let rec gcd n m =
        if m = 0 then n
        else gcd m (n%m)

    let lcm n m = n / (gcd n m) * m //we get an overflow if we don't divide first (or use a long)
    let ans = {1..2000} |> Seq.reduce lcm

module Problem6 =
(*  
    let min = 1
    let max = 100
    let brutePairs = seq{for i in min .. max do 
                            for j in min .. max do
                                if i <> j then
                                    yield 2 * i * j}
    
    let ans = brutePairs |> Seq.distinct |> Seq.reduce(+)
    erm, this isn't quite right, so lets use some more math to simplify even further...
    *)

    //n(n+1)(2n+1)/6 - (n(n+1)/2)^2
    let ans n = (n*(n-1)*(n+1)*(3*n+2))/12

    //since I'm trying to learn F# (as opposed to using random high school math) lets try the brute force method
    let square n = n * n
    let ans2 = ({1 .. 100} |> Seq.sum |> square) - ({1..100} |> Seq.map square |> Seq.sum)

    //using an idea from http://diditwith.net/blog/CategoryView,category,F%23.aspx#a030a32d3-0854-4b74-a55d-a0773fee3c27
    let diff f1 f2 seq = f1 seq - f2 seq
    let ans3 = {1 .. 100} |> diff (square << Seq.sum) (Seq.sum << (Seq.map square)) 

module Problem7 =
    //Lets see if this stupid brute force method is fast enough
    let isPrime n = {2L .. n |> float |> sqrt |> int64} |> Seq.forall(fun m -> n%m <> 0L)    
    let primes = seq {1L .. System.Int64.MaxValue} |> Seq.filter(isPrime)

    //this works, but it takes an entire half second!
    let ans = Seq.nth 10001 primes  //104743L

//as usual, this guy does it better: http://diditwith.net/blog/CategoryView,category,F%23.aspx#a030a32d3-0854-4b74-a55d-a0773fee3c27
module BetterProblem7 = 
    module Dict = 
        open System.Collections.Generic 

        let empty() = new Dictionary<_,_>() 

        let add k v (d : #IDictionary<'a,'b>) = 
            d.[k] <- v; d 

        let remove (k : 'a) (d : #IDictionary<'a,'b>) = 
            d.Remove(k) |> ignore; d 

        let tryfind k (d : #IDictionary<'a,'b>) = 
            match d.TryGetValue(k) with 
            | true, v  -> Some(v) 
            | false, _ -> None

    //could use a priority queue to be efficient while staying immutable..?
    let reinsert x table prime = 
        let comp = x+prime 
        match Dict.tryfind comp table with 
        | None        -> table |> Dict.add comp [prime] 
        | Some(facts) -> table |> Dict.add comp (prime::facts) 

    let rec sieve x table = 
        seq { match Dict.tryfind x table with 
                | None -> 
                    yield x 
                    yield! sieve (x+1L) (table |> Dict.add (x*x) [x]) 
                | Some(factors) -> 
                    yield! sieve (x+1L) (factors |> List.fold (reinsert x) (table |> Dict.remove x)) } 

    let primes = sieve 2L (Dict.empty())
    let ans = Seq.nth 10001 primes

module AlternateProblem7 = 
    open System.Collections.Generic

    let reinsert (n : 'a) (table : #IDictionary<'a,'a list>) prime = 
        let comp = n+prime
        table.Remove(n) |> ignore
        match table.TryGetValue comp with
        | false,_     -> table.[comp] <- [prime]
        | true,factors  -> table.[comp] <- prime::factors
        table

    let rec sieve n (table : #IDictionary<'a,'a list>) = 
        seq { match table.TryGetValue(n) with 
                | false,_ ->
                    table.[n*n] <- [n]                                 
                    yield n 
                    yield! sieve (n+1L) table
                | true,factors -> 
                    factors |> List.fold (reinsert n) table |> ignore
                    yield! sieve (n+1L) table } 

    let primes = sieve 2L (new Dictionary<_,_>())
    let ans = Seq.nth 10001 primes



module Problem8 =
    let input = 
        "73167176531330624919225119674426574742355349194934
        96983520312774506326239578318016984801869478851843
        85861560789112949495459501737958331952853208805511
        12540698747158523863050715693290963295227443043557
        66896648950445244523161731856403098711121722383113
        62229893423380308135336276614282806444486645238749
        30358907296290491560440772390713810515859307960866
        70172427121883998797908792274921901699720888093776
        65727333001053367881220235421809751254540594752243
        52584907711670556013604839586446706324415722155397
        53697817977846174064955149290862569321978468622482
        83972241375657056057490261407972968652414535100474
        82166370484403199890008895243450658541227588666881
        16427171479924442928230863465674813919123162824586
        17866458359124566529476545682848912883142607690042
        24219022671055626321111109370544217506941658960408
        07198403850962455444362981230987879927244284909188
        84580156166097919133875499200524063689912560717606
        05886116467109405077541002256983155200055935729725
        71636269561882670428252483600823257530420752963450"

    let ans = seq {for c in input -> int(c) - int '0'} |> Seq.windowed 5 |> Seq.map (Array.reduce (*)) |> Seq.max

// generating pytheagorean triples, instead of numbers summing to 1000 would be a lot more efficient, but I can type this up in 30 seconds :-).
module Problem9 =
    let max = 1000
    let isPythagoreanTriple (a,b,c) = a*a + b*b = c*c
    let product (a,b,c) = a*b*c
    let triples = seq{for c in 1 .. max do
                        for a in 1 .. (max - c) do
                            yield (a ,(max - c - a), c)}

    let ans = Seq.find isPythagoreanTriple triples |> product

module Problem10 =
    let primes = AlternateProblem7.primes
    let ans = primes |> Seq.takeWhile ((>=) 2000000L) |> Seq.reduce(+)

module Problem12 =   
    let triangleNumbers = Seq.initInfinite (fun n -> (n+2) * (n+3) / 2) //skip the first triangle number (1), to avoid having to deal with an edge case in numFactors

    //could probably speed this up a lot by pre-calculating prime factors (maybe using a LazyList)
    let factors n = 
        let rec loop n m ms =
            if n % m = 0 then
                loop (n/m) m (m::ms)
            elif n > m then
                loop n (m+1) ms
            else
                ms
        loop n 2 []
        
    let numFactors n = factors n |> Seq.countBy(fun x->x) |> Seq.map (fun (a,b) -> b+1) |> Seq.reduce(*)  

    let ans = Seq.find (fun x -> numFactors x > 500) triangleNumbers;;

//boring solution using bigint library. We could do this by just adding the first 11 digits of each of the numbers
module Problem13 =
    let input = "37107287533902102798797998220837590246510135740250
                46376937677490009712648124896970078050417018260538
                74324986199524741059474233309513058123726617309629
                91942213363574161572522430563301811072406154908250
                23067588207539346171171980310421047513778063246676
                89261670696623633820136378418383684178734361726757
                28112879812849979408065481931592621691275889832738
                44274228917432520321923589422876796487670272189318
                47451445736001306439091167216856844588711603153276
                70386486105843025439939619828917593665686757934951
                62176457141856560629502157223196586755079324193331
                64906352462741904929101432445813822663347944758178
                92575867718337217661963751590579239728245598838407
                58203565325359399008402633568948830189458628227828
                80181199384826282014278194139940567587151170094390
                35398664372827112653829987240784473053190104293586
                86515506006295864861532075273371959191420517255829
                71693888707715466499115593487603532921714970056938
                54370070576826684624621495650076471787294438377604
                53282654108756828443191190634694037855217779295145
                36123272525000296071075082563815656710885258350721
                45876576172410976447339110607218265236877223636045
                17423706905851860660448207621209813287860733969412
                81142660418086830619328460811191061556940512689692
                51934325451728388641918047049293215058642563049483
                62467221648435076201727918039944693004732956340691
                15732444386908125794514089057706229429197107928209
                55037687525678773091862540744969844508330393682126
                18336384825330154686196124348767681297534375946515
                80386287592878490201521685554828717201219257766954
                78182833757993103614740356856449095527097864797581
                16726320100436897842553539920931837441497806860984
                48403098129077791799088218795327364475675590848030
                87086987551392711854517078544161852424320693150332
                59959406895756536782107074926966537676326235447210
                69793950679652694742597709739166693763042633987085
                41052684708299085211399427365734116182760315001271
                65378607361501080857009149939512557028198746004375
                35829035317434717326932123578154982629742552737307
                94953759765105305946966067683156574377167401875275
                88902802571733229619176668713819931811048770190271
                25267680276078003013678680992525463401061632866526
                36270218540497705585629946580636237993140746255962
                24074486908231174977792365466257246923322810917141
                91430288197103288597806669760892938638285025333403
                34413065578016127815921815005561868836468420090470
                23053081172816430487623791969842487255036638784583
                11487696932154902810424020138335124462181441773470
                63783299490636259666498587618221225225512486764533
                67720186971698544312419572409913959008952310058822
                95548255300263520781532296796249481641953868218774
                76085327132285723110424803456124867697064507995236
                37774242535411291684276865538926205024910326572967
                23701913275725675285653248258265463092207058596522
                29798860272258331913126375147341994889534765745501
                18495701454879288984856827726077713721403798879715
                38298203783031473527721580348144513491373226651381
                34829543829199918180278916522431027392251122869539
                40957953066405232632538044100059654939159879593635
                29746152185502371307642255121183693803580388584903
                41698116222072977186158236678424689157993532961922
                62467957194401269043877107275048102390895523597457
                23189706772547915061505504953922979530901129967519
                86188088225875314529584099251203829009407770775672
                11306739708304724483816533873502340845647058077308
                82959174767140363198008187129011875491310547126581
                97623331044818386269515456334926366572897563400500
                42846280183517070527831839425882145521227251250327
                55121603546981200581762165212827652751691296897789
                32238195734329339946437501907836945765883352399886
                75506164965184775180738168837861091527357929701337
                62177842752192623401942399639168044983993173312731
                32924185707147349566916674687634660915035914677504
                99518671430235219628894890102423325116913619626622
                73267460800591547471830798392868535206946944540724
                76841822524674417161514036427982273348055556214818
                97142617910342598647204516893989422179826088076852
                87783646182799346313767754307809363333018982642090
                10848802521674670883215120185883543223812876952786
                71329612474782464538636993009049310363619763878039
                62184073572399794223406235393808339651327408011116
                66627891981488087797941876876144230030984490851411
                60661826293682836764744779239180335110989069790714
                85786944089552990653640447425576083659976645795096
                66024396409905389607120198219976047599490197230297
                64913982680032973156037120041377903785566085089252
                16730939319872750275468906903707539413042652315011
                94809377245048795150954100921645863754710598436791
                78639167021187492431995700641917969777599028300699
                15368713711936614952811305876380278410754449733078
                40789923115535562561142322423255033685442488917353
                44889911501440648020369068063960672322193204149535
                41503128880339536053299340368006977710650566631954
                81234880673210146739058568557934581403627822703280
                82616570773948327592232845941706525094512325230608
                22918802058777319719839450180888072429661980811197
                77158542502016545090413245809786882778948721859617
                72107838435069186155435662884062257473692284509516
                20849603980134001723930671666823555245252804609722
                53503534226472524250874054075591789781264330331690"

    let sum = input.Split '\n' |> Array.map bigint.Parse |> Array.sum
    let numDigits = bigint.Log10 sum |> System.Math.Ceiling |> int
    let ans = sum / 10I**(numDigits - 10)

//this is can actually be done quickly enough without memoization, and it would be trivial to do this using automatic memoization, but lets try doing this using explicit memoizaton to get some practice with C# dictionaries
module Problem14 =

    //pointless use of an active pattern just to learn how to use active patterns
    let (|Even|Odd|) n =
        if n % 2L = 0L then Even
        else Odd

    let nextCollatz n =
        match n with
        | 1L -> 1L
        | Even -> n/2L
        | Odd -> (3L*n) + 1L

    let collatz =
        let cache = new System.Collections.Generic.Dictionary<_,_>()
        let rec cachedCollatz n =
            match cache.TryGetValue(n),n with
            | (true,v),_ -> v 
            | (_),1L ->  1
            | _ -> let v = 1 + cachedCollatz (nextCollatz n)
                   cache.Add(n,v)
                   v
        fun n -> cachedCollatz n

    let ans = {1L..1000000L} |> Seq.map (fun n -> (n,collatz n)) |> Seq.maxBy snd |> fst

module Problem15 =
    //40 choose 20
    let ans = ({21I .. 40I} |> Seq.reduce(*)) / ({1I .. 20I} |> Seq.reduce(*))

//boring solution using bigint library. Todo: Try this without using a bigint library.
module Problem16 =
    //note: if we try a really huge number e.g., 2*10000I, this blows up with a stack overflow if we don't use an accumulator to force this to be tail recursive
    let originalSumOfDigits n =
        let rec loop m acc =
            if m = 0I then acc
            else loop (m/10I) (acc + (m%10I))
        loop n 0I

    
    //using DivRem results in a ~2x speedup
    let rec sumOfDigits n =
        let rec loop m acc =
            if m = 0I then acc
            else
                let div,rem = bigint.DivRem (m,10I)
                loop div (acc + rem)
        loop n 0I

    let ans = 2I**1000 |> sumOfDigits

    //via string conversion
    //let ans2 = (2I**1000).ToString() |> Seq.map System.Char.GetNumericValue |> Seq.sum 

module Problem17 =
    let ones = [|""; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "ten"; "eleven"; "twelve"; "thirteen"; "fourteen"; "fifteen"; "sixteen"; "seventeen"; "eighteen";  "nineteen"; "twenty"|] |> Array.map String.length 
    let tens = [|""; "";"twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety"|] |> Array.map String.length
    let hundred = [|"hundred"|] |> Array.map String.length 

    let rec numLength n =
        match n with 
        | n when n <= 20                    -> ones.[n]
        | n when n < 100                    -> tens.[(n/10)] + (numLength (n%10))
        | n when n < 1000 && n % 100 = 0    -> ones.[n/100] + hundred.[0]
        | n when n < 1000                   -> ones.[n/100] + hundred.[0] + String.length "and" + (numLength (n%100))
        | n when n = 1000                   -> String.length "oneThousand"
        | _ -> failwith "Out of range"

    let ans = {1..1000} |> Seq.map numLength |> Seq.sum

//using a 'C' style dynamic program instead of an idiomatic solution, just for fun. Or, erm. something.
module Problem18 =
    open System
    let fileName = @"C:\Users\d\Documents\Visual Studio 2010\Projects\Euler\Euler\euler 18.txt"

    let aData = System.IO.File.ReadAllLines(fileName) |> Array.rev |>  Array.map (fun l ->  l.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.map System.Int32.Parse)
    let size = aData.Length
    let maxTable = Array2D.init size size (fun i j -> if j < size - i then aData.[i].[j] else 0) //no reason to do this; just wanted to learn to use Array2D.init

    for i in 1 .. size - 1 do
        for j in 0 .. size - i - 1 do
            maxTable.[i,j] <- maxTable.[i,j] + (max maxTable.[i-1,j] maxTable.[i-1,j+1])
    let ans = maxTable.[size-1, 0]

module Problem19 =
    open System

    let start = new DateTime(1901, 1, 1)
    let finish = new DateTime(2000, 12, 31)

    let ans = start |> Seq.unfold (fun (t : DateTime) -> Some(t, t.AddMonths(1)))
                    |> Seq.filter (fun (t : DateTime) -> t.DayOfWeek = DayOfWeek.Sunday)
                    |> Seq.takeWhile (fun (t : DateTime) -> t <= finish)
                    |> Seq.length

//boring solution using bigint library. Todo: Try this without using a bigint library.
module Problem20 =
    let ans = {1I..100I} |> Seq.reduce(*) |> Problem16.sumOfDigits

module Problem21 =
    //note: this sumOfDivisors function is actually incorrect for perfect squares, but those aren't amicable numbers, so it's ok.
    let sumOfDivisors n = seq {for m in 2..int(sqrt(float n)) do if n % m = 0 then yield! [m;n/m]} |> Seq.sum |> (+) 1
    let ans = {1..10000} |> Seq.filter (fun n -> sumOfDivisors (sumOfDivisors n) = n && sumOfDivisors n <> n) |> Seq.sum

//this alternate method actually takes the same amount of time!
module AlternateProblem21 =
    let sumOfDivisors n = seq {for m in 2..int(sqrt(float n)) do if n % m = 0 then yield! [m;n/m]} |> Seq.sum |> (+) 1
    let amicable n = 
        let m = sumOfDivisors n
        n = sumOfDivisors m && m <> n
    let ans = {1..10000} |> Seq.filter amicable |> Seq.sum    

    

   

//here's another problem which could be done by hand...
module Problem25 =
    let threshold = bigint.Pow(10I,999)
    let ans = Seq.unfold (fun (n,m) -> Some(m,(m, n+m))) (0I,1I) |> Seq.takeWhile(fun n -> n < threshold) |> Seq.length |> (+) 1

//Oughta do this by only keeping the least significant digits, but the bigint library is just too convenient
module Problem48 =
    let ans = {1..1000} |> Seq.map (fun n -> (bigint n)**n) |> Seq.sum |> (fun n -> n % 10I**10)

module Problem108 =
    let factors n = 
        let rec loop n m ms =
            if n % m = 0L then
                loop (n/m) m (m::ms)
            elif n > m then
                loop n (m+1L) ms
            else
                ms
        loop n 2L []

    let numFactors n = factors n |> Seq.countBy(fun x->x) |> Seq.map (fun (a,b) -> b+1) |> Seq.reduce(*)
    let numSolutionsTimesTwo n = numFactors (n*n)

    let ans = (Seq.initInfinite (fun n -> n + 2) |> Seq.map int64 |> Seq.map numSolutionsTimesTwo |> Seq.takeWhile ((>) 2000) |> Seq.length) + 2

(*    
module Problem110 =
    //this input sequence is from Sloane's A018894. This is just a test to see if a trial an error solution that doesn't already "know" the possibly answers is feasible.
    let candidates = [2;4;6;12;24;30;60;120;180;210;360;420;840;1260;1680;2520;4620;9240;13860;18480;27720;55440;110880;120120;180180;240240;360360;720720;1441440;2162160;3603600;4084080;4324320;6126120;12252240;24504480]
//    candidates |> List.map int64 |> List.find (fun n -> (Problem108.numSolutionsTimesTwo n) > 4000000)
    candidates |> List.map int64 |> List.map Problem108.numSolutionsTimesTwo
    *)

module Problem110 =
    open System
    //blargh! Generating primorial numbers using my dumb method is way too slow
    let fileName = @"C:\Users\d\Documents\Visual Studio 2010\Projects\Euler\Euler\primorial.txt"

    let primorial = System.IO.File.ReadAllLines(fileName) |> Array.map (fun l -> l.Split(' ').[1] |> bigint.Parse) |> Array.toSeq
 
    let factors n = 
        let rec loop n m ms =
            if n % m = 0I then
                loop (n/m) m (m::ms)
            elif n > m then
                loop n (m+1I) ms
            else
                ms
        loop n 2I []

    let numFactors n = factors n |> Seq.countBy(fun x->x) |> Seq.map (fun (a,b) -> b+1) |> Seq.reduce(*)
    let numSolutionsTimesTwo n = numFactors (n*n)

    let ans = primorial |> Seq.find (fun n -> (numSolutionsTimesTwo n) > 8000000)

module Problem219 =
    let cost (c,(n0,n1,n2,n3,n4)) = c*n0 + (c+1L)*(n1) + (c+2L)*(n2) + (c+3L)*(n3) + (c+4L)*(n4)

    let counts = (1L,(1L,0L,0L,1L,0L)) |> Seq.unfold(fun (c,(n0,n1,n2,n3,n4)) ->
        if n0 > 0L then Some((c,(n0,n1,n2,n3,n4)), (c,(n0-1L,n1+1L,n2,n3,n4+1L)))
        else Some((c,(n0,n1,n2,n3,n4)), (c+1L,(n1-1L,n2+1L,n3,n4,1L))))

    let minCost n = cost (Seq.nth (n-2) counts)

    let ans = minCost 1000000000

module Problem219Tests= 
    open Problem219
    [<Fact>] 
    let Test1 = Seq.nth 3 counts|> MustEqual (3L, (0L, 2L, 1L, 1L, 1L))
    [<Fact>] 
    let Test2 = Seq.nth 5 counts|> MustEqual (4L, (0L, 3L, 1L, 1L, 2L))
    [<Fact>] 
    let Test3 = minCost 6 |> MustEqual 35L
    [<Fact>] 
    let Test4 = minCost 12 |> MustEqual 96L


module Orphan=
    //it makes more sense to re-use our old factors function, but I wanted to try using sets in F#
    let distinctFactors n = 
        let rec loop n m (ms : Set<int>) =
            if n % m = 0 then
                loop (n/m) m (ms.Add m)
            elif n > m then
                loop n (m+1) ms
            else
                ms
        loop n 2 (Set.ofList [1])

    module Seq =
        let unzip seq =
            let seq = Seq.cache seq
            Seq.map fst seq, Seq.map snd seq
    


