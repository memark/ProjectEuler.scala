object Lab {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(99); 

  // Problem 1
  val p01 = (1 until 1000) filter (x => x % 3 == 0 || x % 5 == 0) sum;System.out.println("""p01  : Int = """ + $show(p01 ));$skip(26); 
  ; assert(p01 == 233168);$skip(185); 

  // Problem 2
  def fib(upperInclusive: Int): Seq[Int] = {
    def fib(a: Int, b: Int): List[Int] =
      if (a > upperInclusive) Nil
      else a :: fib(b, a + b)
    fib(1, 2)
  };System.out.println("""fib: (upperInclusive: Int)Seq[Int]""");$skip(52); 

  val p02a = fib(4000000) filter (_ % 2 == 0) sum;System.out.println("""p02a  : Int = """ + $show(p02a ));$skip(28); 
  ; assert(p02a == 4613732);$skip(113); 

  def fibs: Seq[Int] = {
    def fib(a: Int, b: Int): Stream[Int] =
      a #:: fib(b, a + b)
    fib(1, 2)
  };System.out.println("""fibs: => Seq[Int]""");$skip(68); 

  val p02b = fibs takeWhile (_ <= 4000000) filter (_ % 2 == 0) sum;System.out.println("""p02b  : Int = """ + $show(p02b ));$skip(28); 
  ; assert(p02b == 4613732);$skip(212); 

  // Problem 3

  //???

  // Problem 4

  def isPalindrome(num: Int): Boolean = {
    val s = num.toString
    val l = s.toList.take(s.size / 2)
    val r = s.toList.reverse.take(s.size / 2)
    l == r
  };System.out.println("""isPalindrome: (num: Int)Boolean""");$skip(91); 

  val p04 = (for (a <- 100 to 999; b <- 100 to 999) yield a * b) filter isPalindrome max;System.out.println("""p04  : Int = """ + $show(p04 ));$skip(26); 
  ; assert(p04 == 906609);$skip(127); 

  // Problem 5

  val positiveNumbers = {
    def loop(x: BigInt): Stream[BigInt] =
      x #:: loop(x + 1)
    loop(1)
  };System.out.println("""positiveNumbers  : Stream[BigInt] = """ + $show(positiveNumbers ));$skip(37); val res$0 = 
  
  positiveNumbers filter (_ == 5);System.out.println("""res0: scala.collection.immutable.Stream[BigInt] = """ + $show(res$0));$skip(72); val res$1 = 
  
  (positiveNumbers filter (a => (1 to 10) forall (b => a % b == 0)));System.out.println("""res1: scala.collection.immutable.Stream[BigInt] = """ + $show(res$1));$skip(300); val res$2 = 
    
  //positiveNumbers filter (x => x % 2 == 0 && x % 3 == 0 && x % 5 == 0 && x % 7 == 0 && x % 11 == 0 && x % 13 == 0 && x % 17 == 0 && x % 19 == 0 && x % 20 == 0)

  //(positiveNumbers filter (a => (BigInt(1) to BigInt(20)) forall (b => a % b == 0)))
  
  (BigInt(1) to BigInt(20)) reduce (_*_);System.out.println("""res2: BigInt = """ + $show(res$2));$skip(177); 

  // Problem 6
  val p06 = {
    val min = 1
    val max = 100

    val x = (min to max) map (x => x * x) sum
    val y = (min to max).sum * (min to max).sum

    y - x
  };System.out.println("""p06  : Int = """ + $show(p06 ));$skip(27); 
  ;assert(p06 == 25164150);$skip(1065); 
  
  // Problem 7

  // ???

  // Problem 8

{
  val s = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
 }}

}
