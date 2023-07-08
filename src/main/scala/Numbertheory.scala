package projecteuler
package numbertheory

/** slightly more general function than LazyList.from(n)
  *
  * @param n
  *   initial value
  * @param jump
  *   the distance (can be ≤0) to the next value in the list
  * @return
  */
def from(n: Int, jump: Int): LazyList[Int] = n #:: from(n + jump, jump)

/** Helper function to speed up the primes lazylist a little.
  * @param n
  *   a number of the form 5 + 6 k (we do not verify this)
  * @return
  *   for the default parameter n=5, the lazy list 5,7,11,13,17,19,... which are
  *   the n ≥ 5 not divisible by 2 or 3 or equivalently n = 1 or 5 mod 6.
  *   Otherwise, a truncated version of this list starting from n.
  */
def no2no3(n: Int = 5): LazyList[Int] = n #:: (n + 2) #:: no2no3(n + 6)

def arithmeticSum(numTerms: Int, from: Int, by: Int): Int =
  (numTerms * (2 * from + (numTerms - 1) * by)) / 2
def sumTo(n: Int): Int = arithmeticSum(n, 1, 1)
def sumSquares(n: Int): Long = n * (n + 1) * (2 * n + 1) / 6
def sumCubes(n: Int): BigInt = pow(sumTo(n), 2)
def sumFourthPowers(n: Int): BigInt =
  n * (n + 1) * (2 * n + 1) * (3 * n * n + 3 * n - 1) / 30
def sumFifthPowers(n: Int): BigInt = {
  val a = sumTo(n); a * a * (4 * a - 1) / 3
}

extension (p: Long)
  /** check if the Long is a triangular number, i.e. a number of the form
    * n(n+1)/2.
    *
    * @returns
    *   true if triangular, false otherwise.
    */
  def isTri: Boolean =
    // if p = n(n+1)/2 then 2p = n**2 + n, so
    // n = -1/2 ± sqrt(1 + 8p)/2. Take the positive number
    val s = math.sqrt(8 * p + 1.toDouble)
    s.isWhole && (s.toLong - 1) % 2 == 0

  /** check if the Long is a pentagonal number, i.e. a number of the form
    * n(3n-1)/2.
    *
    * @returns
    *   true if pentagonal, false otherwise.
    */
  def isPent: Boolean =
    // A pentagonal number is a number of the form n(3n-1)/2.
    val s = math.sqrt(24 * p + 1.toDouble)
    s.isWhole && (s.toLong + 1) % 6 == 0

/** A slightly faster than normal lazylist of primes computed by trial division.
  */
val primes: LazyList[Int] = 2 #:: 3 #:: primesFrom5

/** Auxilliary list of primes starting from 5.
  */
val primesFrom5: LazyList[Int] = 5 #:: 7 #:: no2no3(11)
  .filter(k => primesFrom5.takeWhile(_ <= math.sqrt(k)).forall(k % _ != 0))

// TODO: implement a sieve of eratosthenes properly (see:
// https://raganwald.com/2013/02/23/sieve.html
// and
// http://allaboutscala.com/tutorials/chapter-7-beginner-tutorial-using-scala-mutable-collection/scala-tutorial-learn-use-mutable-priorityqueue/)
// and
// https://www.scala-lang.org/api/2.12.6/scala/collection/mutable/PriorityQueue.html
// and??
// https://dcsobral.blogspot.com/2010/12/sieve-of-eratosthenes-real-one-scala.html
// and
// https://codereview.stackexchange.com/questions/3850/initialize-a-sieve-of-eratosthenes-in-scala
// and
// https://stackoverflow.com/questions/4595941/scala-performance-sieve
// and
// https://stackoverflow.com/questions/11768958/prime-sieve-in-haskell

extension (n: Int)
  def **(power: Int) = pow(n, power)

  // below implementation of isPrime is slow because primes is a lazy list.

  def isPrime = n > 1 && primes.takeWhile(_ <= math.sqrt(n)).forall(n % _ != 0)
  // below is slower
  // def isPrime = primes.dropWhile(_ < n).head == n

  def numDigits: Int = { if n < 0 then -n else n }.toString.length

extension (n: BigInt) def **(power: Int) = pow(n, power)

/** not so naive version of the integer power function, pow(n,power) := n^power.
  * Repeated squaring means that the number of multiplications is logarithmic in
  * the power.
  *
  * @param n
  * @param power
  * @return
  */
def pow(n: BigInt, power: Int): BigInt =
  require(power >= 0)
  @annotation.tailrec
  def pow1(n: BigInt, power: Int, acc: BigInt): BigInt =
    require(power >= 0)
    if power == 0 then acc
    else if power % 2 == 0 then pow1(n * n, power / 2, acc)
    else pow1(n, power - 1, acc * n)
  pow1(n, power, 1)

/** Generic function that implements factorials as well as generalised
  * factorials and falling powers. Also helpful in computing the binomial
  * coefficients
  *
  * @param n
  *   the starting number
  * @param k
  *   number of products
  * @param drop
  *   the amount to decrease n
  * @return
  */
def fallingPow(n: Int, k: Int, drop: Int): BigInt =
  require(drop > 0)
  @annotation.tailrec
  def fPow1(n: Int, k: Int, acc: BigInt): BigInt =
    if k <= 0 then acc
    else fPow1(n - drop, k - 1, acc * n)
  fPow1(n, k, 1)

def factorial(n: Int): BigInt = fallingPow(n, n, 1)

/** Theoretically, binom(n,k) is the number of ways to choose k things from n
  * things, and can be written binom(n,k) = n!/(k!(n-k)!). We use fallingPow
  * which pre-cancels some factors for speed/accuracy
  * @param n
  * @param k
  * @return
  */
def binom(n: Int, k: Int): BigInt =

  val small = math.min(k, n - k)
  fallingPow(n, small, 1) / factorial(small)

def doublefactorial(n: Int) =
  fallingPow(n, n / 2, 2)

/** Returns a non-increasing list of the prime divisors of n. Can probably be
  * sped up.... worst case is if n is a prime. then the algo takes O(n) steps.
  *
  * @param n
  * @return
  */
def divisors(n: Long): List[Long] =
  @annotation.tailrec
  def divisors1(n: Long, k: Int, acc: List[Long]): List[Long] =
    // TODO: there is an optimisation in the following:
    // if primes(k) > sqrt(n) then it is already impossible, as we pull out factors from the smallest up.
    if n < primes(k) then acc
    else if n % primes(k) == 0 then
      divisors1(n / primes(k), primes(k), primes(k) :: acc)
    else divisors1(n, k + 1, acc)
  divisors1(n, 0, Nil)

/** A map whose keys are the prime factors of n, and the values are the
  * multiplicities
  *
  * @param n
  * @return
  */
def factorization(n: BigInt): Map[Int, Int] =
  @annotation.tailrec
  def factor1(n: BigInt, factors: Map[Int, Int]): Map[Int, Int] =
    if n == 1 then factors.withDefaultValue(0)
    else
      val minPrime = primes.filter(n % _ == 0).head
      val newFactors =
        factors.updated(minPrime, factors.getOrElse(minPrime, 0) + 1)
      factor1(n / minPrime, newFactors)
  factor1(n, Map())

// def factorization(n: Int): Map[Int, Int] =
//   @annotation.tailrec
//   def factor1(n: Int, factors: Map[Int, Int]): Map[Int, Int] =
//     if n == 1 then factors.withDefaultValue(0)
//     else
//       val minPrime = primes.filter(n % _ == 0).head
//       val newFactors =
//         factors.updated(minPrime, factors.getOrElse(minPrime, 0) + 1)
//       factor1(n / minPrime, newFactors)
//   factor1(n, Map())

def factorizationToNumber(factors: Map[Int, Int]) =
  factors.map((prime, pows) => pow(prime, pows)).foldLeft(BigInt(1))(_ * _)

def factors(n: Int): List[Int] =
  val fact = factorization(n)
  @annotation.tailrec
  def factors1(curFact: Map[Int, Int], acc: List[Int]): List[Int] =
    if curFact.isEmpty then acc
    else
      val (prime, k) = curFact.head
      factors1(
        curFact.tail,
        (0 to k).toList.flatMap(m => acc.map(x => (x / pow(prime, m)).toInt))
      )
  // WARN: only works if b is small
  factors1(fact.map((a, b) => (a, b.toInt)), List(n))

extension (n: Int) def isAbundant: Boolean = factors(n).tail.sum > n

/** the divisor function of n, denoted sigma(n), is the sum of the factors of n.
  * It is multiplicative but not completely multiplicative..
  * https://en.wikipedia.org/wiki/Divisor_function
  * @param n
  * @return
  */
def sigma(n: Int) = factors(n).sum

/** A convenience function that only sums the [p]roper divisors of n.
  *
  * @param n
  * @return
  */
def psigma(n: Int) = factors(n).sum - n

def amicable(n: Int) =
  if n < 2 then false else (psigma(psigma(n)) == n)

/** Euler's totient function, sometimes also phi(n). It counts the number of
  * naturals in [0..n] that are coprime to n.
  *
  * It is a multiplicative function which is easy to describe on the prime
  * powers (hence the implementation.)
  * @param n
  * @return
  */
def totient(n: BigInt): BigInt =
  val factors = factorization(n)
  (for (prime, power) <- factors
  yield pow(prime, power - 1) * (prime - 1)).foldLeft(BigInt(1))(_ * _)

  /** The [g]reatest [c]ommon [d]ivisor. We compute this with the Euclidean
    * algorithm. Also known as the The [h]ighest [c]ommon [f]actor.
    *
    * @param a
    * @param b
    * @return
    */
def gcd(a: BigInt, b: BigInt): BigInt =
  val (q, r) = a /% b
  if r == 0 then b else gcd(b, r)

// TODO: https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm

/** The [l]owest [c]ommon [m]ultiple. We compute this with the identity a*b =
  * gcd(a,b)*lcm(a,b)
  *
  * @param a
  * @param b
  * @return
  */
def lcm(a: BigInt, b: BigInt): BigInt = a * b / gcd(a, b)

def digits(n: Int): Seq[Int] = // n.abs.toString.map(_.toString.toInt)
  def dig1(n: Int, acc: Seq[Int]): Seq[Int] =
    if n < 10 then n +: acc
    else dig1(n / 10, n % 10 +: acc)
  dig1(n, Seq())
def digits(n: BigInt): Seq[Int] = n.abs.toString.map(_.toString.toInt)

extension (xs: Seq[Int])
  def toInt: Int =
    @annotation.tailrec
    def toInt1(ys: Seq[Int], acc: Int): Int =
      if ys.isEmpty then acc
      else toInt1(ys.tail, ys.head + 10 * acc)
    toInt1(xs, 0)
  def cycle: Seq[Int] = xs.tail :+ xs.head
  def cycles: Seq[Seq[Int]] =
    (1 until xs.length).scanLeft(xs)((xs, _) => xs.cycle)

def unitTests(): Unit =
  println(primes.take(7).toList == List(2, 3, 5, 7, 11, 13, 17))
  println((0 to 10).map(pow(2, _)))
  println(totient(700000000))
  println(gcd(5, 7))
  println(gcd(18, 4))
  println(gcd(400, 950))
