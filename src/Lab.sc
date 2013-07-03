object Lab {

  // Problem 1
  val p01 = (1 until 1000) filter (x => x % 3 == 0 || x % 5 == 0) sum
                                                  //> p01  : Int = 233168
  ; assert(p01 == 233168)

  // Problem 2
  def fib(upperInclusive: Int): Seq[Int] = {
    def fib(a: Int, b: Int): List[Int] =
      if (a > upperInclusive) Nil
      else a :: fib(b, a + b)
    fib(1, 2)
  }                                               //> fib: (upperInclusive: Int)Seq[Int]

  val p02a = fib(4000000) filter (_ % 2 == 0) sum //> p02a  : Int = 4613732
  ; assert(p02a == 4613732)

  def fibs: Seq[Int] = {
    def fib(a: Int, b: Int): Stream[Int] =
      a #:: fib(b, a + b)
    fib(1, 2)
  }                                               //> fibs: => Seq[Int]

  val p02b = fibs takeWhile (_ <= 4000000) filter (_ % 2 == 0) sum
                                                  //> p02b  : Int = 4613732
  ; assert(p02b == 4613732)

  // Problem 3

  //???

  // Problem 4

  def isPalindrome(num: Int): Boolean = {
    val s = num.toString
    val l = s.toList.take(s.size / 2)
    val r = s.toList.reverse.take(s.size / 2)
    l == r
  }                                               //> isPalindrome: (num: Int)Boolean

  val p04 = (for (a <- 100 to 999; b <- 100 to 999) yield a * b) filter isPalindrome max
                                                  //> p04  : Int = 906609
  ; assert(p04 == 906609)

  // Problem 5

  val positiveNumbers = {
    def loop(x: BigInt): Stream[BigInt] =
      x #:: loop(x + 1)
    loop(1)
  }                                               //> positiveNumbers  : Stream[BigInt] = Stream(1, ?)
  
  positiveNumbers filter (_ == 5)                 //> res0: scala.collection.immutable.Stream[BigInt] = Stream(5, ?)
  
  (positiveNumbers filter (a => (1 to 10) forall (b => a % b == 0)))
                                                  //> res1: scala.collection.immutable.Stream[BigInt] = Stream(2520, ?)
    
  //positiveNumbers filter (x => x % 2 == 0 && x % 3 == 0 && x % 5 == 0 && x % 7 == 0 && x % 11 == 0 && x % 13 == 0 && x % 17 == 0 && x % 19 == 0 && x % 20 == 0)

  //(positiveNumbers filter (a => (BigInt(1) to BigInt(20)) forall (b => a % b == 0)))
  
  (BigInt(1) to BigInt(20)) reduce (_*_)          //> res2: BigInt = 2432902008176640000

  // Problem 6
  val p06 = {
    val min = 1
    val max = 100

    val x = (min to max) map (x => x * x) sum
    val y = (min to max).sum * (min to max).sum

    y - x
  }                                               //> p06  : Int = 25164150
  ;assert(p06 == 25164150)
  
  // Problem 7

  // ???

  // Problem 8

{
  val s = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
 }

}