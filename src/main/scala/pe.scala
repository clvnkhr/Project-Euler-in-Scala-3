package projecteuler

import projecteuler.util._
import projecteuler.numbertheory._
import scala.util.boundary, boundary.break

def pe(number: Int) = number match
  case 1 =>
    (1 until 1000).filter(i => i % 3 == 0 || i % 5 == 0).sum

  case 2 =>
    @annotation.tailrec
    def pe002(f1: Int = 1, f2: Int = 1, total: Int = 0): Int =
      if f2 > 4000000 then total
      else
        val f3 = f1 + f2
        val newTotal = if f3 % 2 == 0 then total + f3 else total
        pe002(f2, f3, newTotal)
    pe002()

  case 3 =>
    divisors(600851475143L).head

  case 4 =>
    val n = 999
    extension (n: Int)
      def isPalindrome: Boolean =
        val str = n.toString
        str == str.reverse

    (
      for
        i <- (1 to n)
        j <- (i to n)
        if (i * j).isPalindrome
      yield i * j
    ).max(using (a, b) => if a > b then 1 else -1)

  case 5 =>
    (1 to 20).map(BigInt(_)).foldLeft(BigInt(1))(lcm)

  case 6 =>
    sumFourthPowers(100) - sumCubes(100)

  case 7 =>
    primes(10001)

  case 8 =>
    (
      for i <- 0 until pe008data.length - 13
      yield pe008data
        .substring(i, i + 13)
        .toList
        .map(_.toInt - 48) // toInt on char gives the ASCII point...
        .foldLeft(BigInt(1))((a, b) => a * BigInt(b))
    ).max

  case 9 =>
    val maxlen: Int = 1000
    (for
      a <- 1 to maxlen
      b <- a to maxlen
      if a * a + b * b == (maxlen - b - a) * (maxlen - b - a)
    yield a * b * (maxlen - b - a)).head

  case 10 =>
    // TODO: this would be faster if we implemented a sieve
    primes.takeWhile(_ < 2_000_000).foldLeft(0L)(_ + _.toLong)

  case 11 => {
    val numList = pe011data.split("\\s+").map(_.toLong)
    val gridMap = (
      for
        i <- 0 until 20
        j <- 0 until 20
      yield (i, j) -> numList(i + 20 * j)
    ).toMap.withDefaultValue(0L)
    def window(pt: (Int, Int), dir: (Int, Int), w: Int) =
      for i <- 0 until w yield (pt(0) + i * dir(0), pt(1) + i * dir(1))
    (
      for
        i <- 0 to 20
        j <- 0 to 20
        direction <- List((1, 0), (0, 1), (1, 1), (1, -1))
      yield window((i, j), direction, 4).map(gridMap).product
    ).max
  }

  case 12 =>
    def numFactors(n: Int): Long =
      factorization(n).map((prime, pow) => pow + 1).product
    val triangles = LazyList.from(1).map(sumTo)
    triangles.filter(numFactors(_) > 500).head

  case 13 =>
    pe013data
      .split("\\s+")
      .map(BigInt(_))
      .foldLeft(BigInt(0))(_ + _)
      .toString
      .take(10) // .toLong

  case 14 => {
    val m: Int = 1000000
    /* returns the length of the collatz sequence starting at n
     * */
    @annotation.tailrec
    def collatzlen(n: Long, len: Int = 0): Int =
      if n == 1 then len + 1
      else if n % 2 == 0 then collatzlen(n / 2, len + 1)
      else collatzlen((3 * n + 1) / 2, len + 2)

    (1 to m)
      .map(n => (n, collatzlen(n, 0)))
      .reduceLeft((p, q) => if p(1) > q(1) then p else q)(0)
  }

  case 15 => binom(40, 20)
  // Explanation: take h=20 steps down and w=20 steps right, in any order.
  // Use balls and bins model
  //
  case 16 => (2 ** 1000).toString.toList.map(_.toString.toInt).sum

  case 17 => {
    def basicToEnglish: Map[Int, String] = Map(
      1 -> "one",
      2 -> "two",
      3 -> "three",
      4 -> "four",
      5 -> "five",
      6 -> "six",
      7 -> "seven",
      8 -> "eight",
      9 -> "nine",
      10 -> "ten",
      11 -> "eleven",
      12 -> "twelve",
      13 -> "thirteen",
      14 -> "fourteen",
      15 -> "fifteen",
      16 -> "sixteen",
      17 -> "seventeen",
      18 -> "eighteen",
      19 -> "nineteen",
      20 -> "twenty",
      30 -> "thirty",
      40 -> "forty",
      50 -> "fifty",
      60 -> "sixty",
      70 -> "seventy",
      80 -> "eighty",
      90 -> "ninety"
    )
    def toEnglish(n: Int): String =
      if basicToEnglish contains n then basicToEnglish(n)
      else if (21 to 99) contains n then
        toEnglish((n / 10) * 10) + toEnglish(n % 10)
      else if (100 to 900 by 100) contains n then toEnglish(n / 100) + "hundred"
      else if (101 to 999) contains n then
        toEnglish((n / 100) * 100) + "and" + toEnglish(n % 100)
      else if n == 1000 then "one" + "thousand"
      else "huh?"

    (1 to 1000).map(toEnglish(_).length).sum
  }

  case 18 => {
    val triRows =
      pe018data.split("\n").toList.map(_.trim.split(" ").toList.map(_.toInt))
    def pathsum(directions: String): Long =
      @annotation.tailrec
      def psAcc(dir: String, acc: Long, point: (Int, Int)): Long =
        if dir.isEmpty then acc
        else
          val a = point(0) + 1
          val b = if dir.head == '0' then point(1) else point(1) + 1
          psAcc(dir.tail, acc + triRows(a)(b), (a, b))

      if directions.length <= 14 then psAcc(directions, triRows(0)(0), (0, 0))
      else 0

    (for i <- (0 to (math.pow(2, 15).toInt - 1)).map(_.toBinaryString)
    yield
      val l = i.length
      val rem = if l > 8 then 0 else 8 - l
      pathsum("0" * rem + i)
    ).max
  }

  case 19 => {
    val firstYear = 1901
    val lastYear = 2000
    def leapYear(year: Int): Boolean =
      year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
    def numDaysInYear(year: Int): Int = if leapYear(year) then 366 else 365
    def lengthOfMonth(month: Int, year: Int): Int =
      require(month >= 1 && month <= 12)
      if month == 2 then if leapYear(year) then 29 else 28
      else if List(9, 4, 6, 11) contains month then 30
      else 31
    def firstsOfMonths(year: Int): Set[Int] =
      (1 to 12).map(lengthOfMonth(_, year)).scanLeft(0)(_ + _).toSet.map(_ + 1)
      //                                  one-indexing------------------^^^^^^^
    def setOfDay(day: Int, year: Int): Set[Int] =
      val offset =
        if year == 1900 then 0
        else
          val sgn = (year - 1900).sign
          ((1900 until year by sgn).map(numDaysInYear(_)).sum)
      (1 to numDaysInYear(year))
        .filter(d => (d + offset) % 7 == day % 7)
        .toSet

    (firstYear to lastYear)
      .map(year => (firstsOfMonths(year) & setOfDay(0, year)).size)
      .sum
  }

  case 20 =>
    factorial(100).toString.map(_.toString.toInt).foldLeft(0)(_ + _)

  case 21 =>
    def amicUpto(n: Int): List[Int] =
      for
        i <- (2 to n).toList
        i1 = psigma(i)
        if i == psigma(i1) && i != i1
      yield i
    amicUpto(10000).takeWhile(_ < 10000).sum

  case 22 =>
    val home = java.io.File(".").getAbsolutePath().dropRight(1)
    val bufferedSource =
      io.Source.fromFile(s"${home}src/main/scala/pe022names.txt")
    val sortedNames = bufferedSource.getLines.mkString
      .filter(_ != '"')
      .split(',')
      .sorted
    bufferedSource.close
    def score(index: Int, name: String): Int =
      def charScore(c: Char) = c.toInt - ('A'.toInt - 1)
      index * name.foldLeft(0)(_ + charScore(_))

    sortedNames
      .map(n => score(sortedNames.indexOf(n) + 1, n))
      .sum

  case 23 =>
    // WARNING: quite slow
    val upperBd: Int = 28123
    val abundants = LazyList.from(12).filter(_.isAbundant)
    val summand = abundants.takeWhile(_ <= upperBd - 12)

    upperBd * (upperBd + 1) / 2 - (for
      i <- summand
      j <- summand
      if i + j <= upperBd
    yield (i + j)).distinct.sum
  // solution with distinct is faster than toSet (!)

  case 24 =>
    // TODO: redo with pure math
    lazy val canonicalLists = (0 to 10).map(i => lexicoPerm((0 until i).toList))
    def lexicoPerm[A](lst: List[A]): LazyList[List[A]] =
      require(lst.length < 11) // any higher and it will never terminate
      lst match
        case Nil      => LazyList(Nil)
        case x :: Nil => LazyList(List(x))
        case _ =>
          val len = lst.length
          for
            i <- (0 until lst.length).to(LazyList)
            p <- canonicalLists(len - 1)
          yield lst(i) :: p.map(lst.take(i) ::: lst.drop(i + 1))
    end lexicoPerm
    lexicoPerm((0 to 9).toList)(1_000_000 - 1).mkString // .toLong

  case 25 =>
    @annotation.tailrec
    def fibpe25(index: Int, prev1: BigInt, prev2: BigInt): Int =
      if prev1 >= pow(10, 1000 - 1) then index
      else fibpe25(index + 1, prev1 + prev2, prev1)
    fibpe25(2, 1, 1)

  case 26 =>
    // NOTE: answer can be obtained from the OEIS: https://oeis.org/A051626/b051626.txt
    // INFO: we say 1/n has a prefix of length p and reptend of length r if
    // n = 0.a1a2a3...apb1b2...brb1b2...br...
    // for integers ai, bi, and bi are not all 9s.
    // we notate 1/n = [a;b]. For n<1000, all 1/n are larger than 0.001
    // --
    // If 1/n = [a;b] has reptend length r, then (10**r - 1)/n is an integer? Wrong if 2 or 5 divides n.
    // If n = 2^x 5^y then n has a terminating decimal representation (reptend length = 0).
    // equivalently 10**r = 1 mod base.
    //

    extension (n: Int)
      def without2sAnd5s: Int =
        if n % 2 == 0 then (n / 2).without2sAnd5s
        else if n % 5 == 0 then (n / 5).without2sAnd5s
        else n

    def reptendLength(n: Int, prev: Int, base: Int): Int =
      assert(n <= base)
      val next = (prev * 10) % base
      if next == 1 then n
      else reptendLength(n + 1, next, base)

    val best = (
      for
        c <- (3 to 1000).map(_.without2sAnd5s).distinct
        if c > 1
      yield (c, reptendLength(1, 1, c))
    ).maxBy((_, b) => b)

    best(0)
  //
  case 27 =>
    // INFO: for n*n +  a*n + b to be prime at n=0,
    // we need b to be prime (and positive).
    // We also need a to be odd
    // (else we have for n=1, 1 + a + b which is even so cannot be prime)

    def poly(a: Int, b: Int, n: Int) = n * n + a * n + b

    def numPrimesFromZero(a: Int, b: Int): Int =
      (0 until b).map(poly(a, b, _)).takeWhile(_.isPrime).length

    val (aMax, bMax, _) = (
      for
        b <- primes.takeWhile(_ < 1000)
        a <- (-999 to 999 by 2)
      yield (a, b, numPrimesFromZero(a, b))
    ).maxBy((_, _, c) => c)

    aMax * bMax

  case 28 =>
    val sideLength = 1001
    val numSquares = (sideLength + 1) / 2

    def oddSquares(n: Int) = (2 * n - 1) * (2 * n - 1) // up-right diagonal.
    def sumOddSquares(n: Int) = n * (4 * n * n - 1) / 3
    def diag(k: Int)(n: Int): Int =
      if k <= 0 then oddSquares(n)
      else diag(k - 1)(n) - 2 * (n - 1)

    (for
      k <- 0 to 3
      n <- 1 to numSquares
    yield diag(k)(
      n
    )).sum - 3 // INFO: take off 3 for overcounting the initial 1 in the center

  case 29 =>
    def naiveCounter(n: Int) =
      (for
        a <- 2 to n
        b <- 2 to n
      yield a ** b).distinct.length

    naiveCounter(100)

  case 30 =>
    def pe30(n: Int) = digits(n).map(_ ** 5).sum == n
    // INFO: no n>=7 digit numbers are possible. 9^5 * 7 = 413,343 which is only 6 digits.
    (2 to 1000_000).filter(pe30).sum

  case 31 =>
    // (for // INFO: no need for dynamic programming...
    //   gbp2 <- (0 to 1)
    //   gbp1 <- (0 to 2 - 2 * gbp2)
    //   p50 <- (0 to 4 - 4 * gbp2 - 2 * gbp1)
    //   p20 <- (0 to (20 - 20 * gbp2 - 10 * gbp1 - 5 * p50) / 2)
    //   p10 <- (0 to 20 - 10 * gbp1 - 5 * p50 - 2 * p20)
    //   p5 <- (0 to 40 - 40 * gbp2 - 20 * gbp1 - 10 * p50 - 4 * p20 - 2 * p10)
    //   p2 <-
    //     (0 to (200 - 200 * gbp2 - 100 * gbp1 - 50 * p50 - 20 * p20 - 10 * p10 - 5 * p5) / 2)
    // // INFO: the number of one pence coins is fixed by the choice of the other coins
    // yield 1).sum

    // tail-recursive version:
    val coins = 1 :: 2 :: 5 :: 10 :: 20 :: 50 :: 100 :: 200 :: Nil
    def pe31(amount: Int, coins: List[Int]): Int = (amount, coins) match
      case (0, _)   => 1
      case (_, Nil) => 0
      case (amount, _) =>
        if coins.head > amount then 0
        else pe31(amount, coins.tail) + pe31(amount - coins.head, coins)

    pe31(200, coins)

  case 32 =>
    def pandigital(a: Int, b: Int, c: Int): Boolean =
      val combined = (digits(a) ++ digits(b) ++ digits(c))
      (1 to 9).forall(x => combined.contains(x))

    assert(pandigital(39, 186, 7254))
    // INFO:
    // 5 digit numbers are too big
    // - abcde = X * Y, we need X and Y to have 4 digits total, either 2 and 2 or 1 and 3.
    //   but 99 * 99 = 9801 is not 5 digits, similarly 9 * 999 = 8991
    //
    // INFO:
    // 3 digit numbers are too small
    // - abc = X * Y, we need X and Y to have 6 digits total. But X and Y must be smaller than abc. So X,Y have 3 digits.
    //   And then X * Y has more than 3, so cannot equal abc.
    //
    // INFO:
    // numbers with repeats or zeros can be skipped
    (for
      x <- (1234 to 9876)
      if factors(x)
        .filter(_ < math.sqrt(x))
        .exists(factor => pandigital(x, factor, x / factor))
    yield x).sum

  case 33 =>
    // INFO:
    // trivial fractions:
    // a0/b0, aa/bb, or generally F*a/F*b where a,b= 1...9; F*a,F*b<100.
    //
    // INFO:
    // if we cancel the digit c it cannot be on the same side:
    // - ca/cb = a/b => ca * b = cb * a => 10*c*b + a*b = 10*c*a + a*b => a=b => a/b = 1 => out of range
    // - ac/bc = a/b => ac * b = bc * a => 10*a*b + c*b = 10*b*a + c*a => a=b again.
    //
    //
    extension (tup: (Int, Int)) def toInt: Int = 10 * tup(0) + tup(1)
    val type1 = for
      c <- (1 to 9)
      a <- (1 to 9)
      b <- (a + 1 to 9) // a/b < 1 so we must have b>a
      // ac/cb = a/b <=> ac*b = cb*a
      if (a, c).toInt * b == (c, b).toInt * a
    yield (a, b, c)

    // INFO:
    // ca/bc = a/b <=> ca*b = a*bc <=> 10*c*b + a*b = 10*a*b + a*c <=> 10*c*b = 9*a*b + a*c.
    // Not so clear that this is impossible, but true:
    // a<b => a*c < b*c => 9*a*b + a*c = 10*c*b > 10*a*c => a*b > c*b => a>c.
    // 9*b*(c-a) + c*b = a*c => 9*b*(a-c) = c*(b-c)
    // so 9 | RHS. 9 cannot divide c or b-c, so 3|c and 3|b-c.
    // So c=3 or 6 and b=6 or 9. b=9 makes the LHS too large. c=3 makes the RHS too small.
    // So c=6 and b=6. But then c=b, which is impossible. QED
    // Or,
    // use the empirical test below:
    //
    // val type2 = for
    //   c <- (1 to 9)
    //   a <- (1 to 9)
    //   b <- (a + 1 to 9) // a/b < 1 so we must have b>a
    //   //  ca/bc = a/b <=> ca*b = a*bc
    //   if (c, a).toInt * b == (b, c).toInt * a
    // yield (a, b, c)
    // assert(type2.isEmpty)

    val (num, denom, _) = type1.reduce((x, y) => (x(0) * y(0), x(1) * y(1), 0))
    denom / gcd(num, denom)

  case 34 =>
    // INFO:
    // abc...z > 10^(n-1) (exponential) where n is the number of digits.
    // 9!+9!+...+9! is linear in the number of 9s.
    // So eventually abc...z > 9!+9!+...+9!.
    // 10^(n-1) > 9!n when n>7. So we search all x digit numbers, 2<=x<=7.
    // Slightly beter bound, compare directly x vs 9!(lg x + 1) >= 9! ceil(lg x).
    // this gives impossibility for x > 2696488.166 (in particular, 6 digits)
    //
    // INFO: empirical search shows that n>5 is impossible. Proof?

    // INFO: precompute factorials for speed
    val factorial = Array(1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880)
    (for
      i <- (10 to 2_696_489)
      if digits(i).map(factorial).sum == i
    yield i).sum

  case 35 =>
    extension (p: Int)
      def isCircular: Boolean =
        def circ(digs: Seq[Int], step: Int): Boolean =
          if step <= 0 then true
          else digs.toInt.isPrime && circ(digs.cycle, step - 1)
        end circ
        p.isPrime && circ(digits(p).cycle, p.numDigits - 1)
    end extension

    def circularPrimes(n: Int): Seq[Int] =
      var acc = Seq[Int]()
      val bd = (10 ** n).toInt

      for
        p <- primes.dropWhile(_ < bd / 10).takeWhile(_ < bd)
        digs = digits(p)
        if (p < 10 || (!digs.exists(_ % 2 == 0)
          && !digs.contains(5)))
          && !acc.contains(p)
          && p.isCircular
      do acc = acc ++ (digits(p).cycles.map(_.toInt).distinct)

      acc
    end circularPrimes

    (1 to 6).map(circularPrimes(_).length).sum

  case 36 =>
    (for
      n <- 1 to 999999
      digs = digits(n)
      if digs == digs.reverse
      bins = n.toBinaryString
      if bins == bins.reverse
    yield n).sum

  case 37 =>
    // a smarter solution might be to cache and build upwards from 2,3,5,7
    def rightTrunc(n: Int) = n / 10
    def leftTrunc(n: Int): Int =
      val l = n.numDigits
      n - (n / (10 ** (l - 1)).toInt) * (10 ** (l - 1)).toInt

    extension (n: Int)
      def isTruncatable: Boolean =
        val l = n.numDigits
        (1 until l).scanLeft(n)((a, _) => rightTrunc(a)).forall(isPrime)
        && (1 until l).scanLeft(n)((a, _) => leftTrunc(a)).forall(isPrime)

    var acc = List[Int]()
    // testing out boundary-break (Scala3.3 LTS) since we are not given any guarantees that 15 should be the end
    boundary {
      for
        p <- primes
        if digits(p).tail.forall(x => x % 2 != 0 && x != 5) && p.isTruncatable
      do
        acc = (p :: acc)
        if acc.length == 15 then break(acc) // including 2,3,5,7
    }
    acc.sum - 17 // subtracting 2+3+5+7

  case 38 =>
    // INFO: for the concatenated product (CP) of X and (1,...,n), clearly n<10 or there are too many digits
    // for n = 9, only X=1 works and the CP is the infimal value 123456789.
    // for n = 8, there are no solutions; once X>1 then 5*X,...,8*X have 2 digits each, for 3 digits too many
    // for n = 7, similarly we have ≥4 digits 1*X,...4*X, then 5*X, 6*X, 7*X which is ≥6 digits. Too many.
    // Conclusion: search only for n≤6.
    //
    // INFO: we need to make 9 digits. So the sum over i=1...n of floor(lg(i*X))+1 must be 9.
    // lower/upper bounds:
    //    L <= sum over i=1...n of floor(lg(i*X))+1 < U,
    // where
    //    L = sum over i=1...n of [lg(i*X)]   = s + n lgX
    //    U = sum over i=1...n of [lg(i*X)+1] = s + n lgX + n
    //    s = sum over i=1...n of lg(i)
    //    find largest X=X1 where U<=9 and the least X=X2 where L>9,
    //    then check all X in the range X1 to X2
    //    --
    //    U<=9 <=> X1 <= 10**(9-s-n)/n
    //    L>9  <=> X2 > 10**(9-s)/n

    //
    // INFO: for n>1, X cannot be a multiple of 5, else 2*X has a zero.
    // INFO: for n>4, X cannot be even, else 5*X has a zero.
    //
    var currMax = 123456789
    for
      n <- (2 to 6)
      s = (2 to n).map(j => math.log10(j)).sum // log10(1) == 0
      lo = math.pow(10, (9 - s) / n - 1)
      hi = math.pow(10, (9 - s) / n)
      x <- (math.ceil(lo).toInt to math.floor(hi).toInt)
      // if (x % 5 != 0 && (n<5 || x % 2 != 0)) // no noticable speedup
      candidate = (1 to n).flatMap(j => digits(x * j))
      if (1 to 9).forall(candidate.contains) && candidate.length == 9
    do
      // println(s"$candidate from n=$n, x=$x; s=$s" )
      currMax = currMax max candidate.toInt
    currMax

  case 39 =>
    val crudeBd = math.ceil(math.sqrt(1000)).toInt
    // generate all primitive triples which sum to at most 1000, and store their sum
    // c.f. https://en.wikipedia.org/wiki/Pythagorean_triple
    val sums = (for
      n <- 1 to crudeBd
      m <- n + 1 to math
        .ceil((500.toDouble / n).min(1000 - n * n))
        .toInt
        .min(crudeBd)
      a = m * m - n * n
      b = 2 * m * n
      c = m * m + n * n
      if a + b + c <= 1000
      if gcd(
        n,
        m
      ).toInt == 1 && (m % 2 == 0 || n % 2 == 0) && (m % 2 != 0 || n % 2 != 0)
    yield
    // println(s"n=$n, m=$m, $a $b $c")
    a + b + c).sorted

    // count the number of triples with sum p, p=1...1000
    val numSols =
      for p <- sums.head to 1000
      yield
      // println(sums.takeWhile(_ <= p).count(p % _ == 0))
      (p, sums.takeWhile(_ <= p).count(p % _ == 0))
    numSols.maxBy((_, b) => b)._1

  case 40 =>
    // 1 to 9: 9 digits.
    // 10 to 99: 2 digits each, 99 - 10 + 1 numbers = 2*90 = 180 digits.     total:   189
    // 100 to 999: 3 digits each, 999-100+1 numbers = 3*900 = 2700 digits.   total:  2889
    // 1000 to 9999: 4 digits each, 9999-1000+1 nos = 4*9000 = 36000 digits. total: 38889
    // 10000 to 99999:                                                             488889
    // 100000 to 999999:                                                          5888889.
    // 488889 + 6 * n = 1_000_000 => n = 85,185.1666666667
    val cache = (1 to 185186).map(_.toString).mkString
    (0 to 6)
      .map(i => (10 ** i).toInt)
      .map(i => cache(i - 1).toString.toInt)
      .product

  case 41 =>
    // INFO:
    // there cannot be a 9-pandigital prime, as 1 + ... + 9 = 9*10/2 = 45 = 0 mod 3.
    // similarly there cannot be a 8-pandigital prime; 8*9/2 = 4*9 = 36 = 0 mod 3.
    // 7-pandigital: 7*8/2 = 7*4 = 28: possible
    // 6-pandigital: 6*7/2 = 3*7 = 0 mod 3.
    // 5-pandigital: 5*6/2 = 5*3 = 0 mod 3.
    // 4-pandigital: 4*5/2 = 2*5 = 10: possible (indeed 2143 is the given example)
    // so just find all permutations of 1234567 that are prime, and take the max.
    // If there are none then we look at the 4-pandigitals...
    //
    // val possibleFirstDigits = Seq(1,3,7)
    // val otherDigits = Seq(2,4,6,5)
    // (for
    //   d1 <- possibleFirstDigits
    //   ds <- (possibleFirstDigits.filter(_ != d1) ++ otherDigits).permutations
    //   x = (ds :+ d1).toInt
    //   if x.isPrime
    // yield
    //   x).max
    // probably faster if we could run our own permutations that drop from the largest to the smallest.

    // it is faster to compute all 7 digit primes and check digits
    primes
      .dropWhile(_ < 1_000_000)
      .takeWhile(_ < 10_000_000)
      .filter(p => (1 to 7).forall(digits(p).contains(_)))
      .last

  case 42 =>
    val home = java.io.File(".").getAbsolutePath().dropRight(1)
    val bufferedSource =
      io.Source.fromFile(s"${home}src/main/scala/pe042words.txt")
    val potentialTriangles = bufferedSource.getLines.mkString
      .filter(_ != '"')
      .split(',')
      .map(s => s.map(c => c.toInt - 'A'.toInt + 1).sum)
    bufferedSource.close

    val max = potentialTriangles.max

    val triangles = from(1, 1).map(sumTo).takeWhile(_ <= max)

    (for
      pt <- potentialTriangles
      if triangles.contains(pt)
    yield 1).sum

  case 43 =>
    // INFO:
    // d2d3d4  div by 2  : d4 % 2 == 0: d4=0,2,4,6,8
    // d3d4d5  div by 3  : d3 + d4 + d5 % 3 == 0
    // d4d5d6  div by 5  : d6 % 5 == 0
    // d5d6d7  div by 7  : d5d6 = 2*d7 mod 7
    // d6d7d8  div by 11 : d6>0 and (d6+d8<9 and d7=d6+d8 or d6+d8>9 and d6+d8-10=d7) or (d6=0 and d7=d8)
    // d7d8d9  div by 13 : make a table
    // d8d9d10 div by 17 : make a table
    //
    // INFO: d1,d2,d3 are free, probably we require d1>0

    //
    def neq(is: Int*)(j: Int) = is.forall(i => i != j)
    // although messy, the below is faster than only checking at the end
    (for
      d3 <- (0 to 9)
      d4 <- (0 to 9 by 2) // 234 div by 2
      if neq(d3)(d4)
      d6 = 5 // 456 div by 5: see d8 condition below
      if neq(d3, d4)(d6)
      d5 <- (0 to 9) // 345 div by 3
      if neq(d3, d4, d6)(d5) && ((d3 + d4 + d5) % 3 == 0)
      d7 <- (0 to 9) // 567 div by 7
      if neq(d3, d4, d5, d6)(d7)
      if ((10 * d5 + d6 - 2 * d7) % 7 == 0)
      d8 <- (0 to 9) // 678 div by 11; since no repeated digits, d6!=0
      if neq(d3, d4, d5, d6, d7)(d8)
      if d6 + d8 == d7 || d6 + d8 - 11 == d7
      d9 <- (0 to 9) // 789 div by 13
      if neq(d3, d4, d5, d6, d7, d8)(d9)
        && (Seq(d7, d8, d9).toInt % 13 == 0)
      d10 <- (0 to 9) // 89 10 div by 17
      if neq(d3, d4, d5, d6, d7, d8, d9)(d10)
        && (Seq(d8, d9, d10).toInt % 17 == 0)
      d1 <- (1 to 9)
      if neq(d3, d4, d5, d6, d7, d8, d9, d10)(d1)
      d2 <- (0 to 9)
      if neq(d3, d4, d5, d6, d7, d8, d9, d10, d1)(d2)
      // above line implies that all digits are used
      ds = Seq(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10)
    yield BigInt(ds.mkString)).sum

  case 44 =>
    // INFO: Pentagonal numbers: n(3n-1)/2: 1, 5, 12, 22, 35,... (A000326 in the OEIS).
    val pent = from(1, 1).map(n => n * (3 * n - 1) / 2)
    (for
      (p, n) <- pent.zipWithIndex.tail.tail
      q <- pent.tail.take(n)
      if (p - q).isPent && (p + q).isPent
    yield p - q).head

  case 45 =>
    from(1, 1)
      .map(i => { i.toLong * (2 * i - 1) })
      .filter(h => h.isTri && h.isPent)
      .tail
      .tail
      .head

  case 46 =>
    val squares = from(1, 1).map(i => i.toLong * i)
    val odds = from(1, 1).map(i => 2 * i + 1)

    odds
      .filter(!_.isPrime)
      .filter(o =>
        squares.takeWhile(_ <= o / 2).forall(s => !((o - 2 * s).toInt).isPrime)
      )
      .head

  case 47 =>
    (for
      i <- from(647, 1)
      fs = LazyList(0, 1, 2, 3).map(j => factorization(i + j))
      if fs.forall(_.size == 4)
    yield i).head

  case 48 =>
    (1 to 1000).map(i => BigInt(i) ** i).sum.toString.takeRight(10)

  case 49 =>
    val candidatePrimes = primes.dropWhile(_ < 1_000).takeWhile(_ < 10_000)
    (for
      p <- candidatePrimes
      k <-
        (candidatePrimes.tail.head - candidatePrimes.head to (10_000 - p) / 2)
      digs = digits(p).sorted
      if digits(p + k).sorted == digs
        && digits(p + 2 * k).sorted == digs
        && candidatePrimes
          .dropWhile(q => q < p + k)
          .head == p + k
        && candidatePrimes
          .dropWhile(q => q < p + 2 * k)
          .head == p + 2 * k
    yield s"$p${p + k}${p + 2 * k}").tail.head

  case 50 =>
    // INFO:
    // step1 sum from 2 until we hit 1_000_000
    // step2 sum from 3 until we hit 1_000_000, going two primes at a time
    // number of primes we need: primes are increasing.
    // So it is not possible to use two primes larger than 500_000, 3 primes larger than 333_333, or n primes all larger than 1_000_000/n.
    //
    val primesWithIndex = primes.zipWithIndex

    val maxFrom2 = primesWithIndex
      .scanLeft((0, 0))((a, b) => (a(0) + b(0), b(1) + 1))
      // off-by-one for number of primes
      .takeWhile(_(0) < 1_000_000)
      .filter(_(0).isPrime)
      .last

    val maxFroms =
      for
        // INFO: any prime larger than 1_000_000 / maxFrom2._2 cannot beat the number of conseq. sums from 2.
        p <- primesWithIndex.tail.takeWhile(_._1 < 1_000_000 / maxFrom2._2)
        candidates = primesWithIndex
          .drop(p(1))
          // INFO: compute the consecutive sum starting from p. note off-by-one in indexing
          .scanLeft((0, 0))((a, b) => (a(0) + b(0), b(1) + 1 - p(1)))
          // INFO: if it doesn't beat the streak starting from 2, we don't need to check if it's prime. Also, jump twice.
          .filter(t => t._2 > maxFrom2._2 && t._2 % 2 != 0)
          .takeWhile(_(0) < 1_000_000)
          .filter(_(0).isPrime)
        if !candidates.isEmpty
      yield
        val temp = candidates.last
        (p._1, temp._1, temp._2)
    ((2, maxFrom2._1, maxFrom2._2) #:: maxFroms).maxBy(_._3)._2

  case 51 =>
    // INFO:
    // to get 8 primes, we cannot replace the last digit: the last digit cannot be 0,2,4,5,6,8. It must be one of 1,3,7,9.
    // If the last digit is 3, or 9 we cannot replace all the digits with 3,6 or 9. This means that there are 10-3 = 7 digits left; so 8 is impossible.
    // go through primes and change their repeating digits?
    // if we cycle through 8 single digits, 1,2,3,4,5,6,7,8,9,0, one of them will be divisible by 3. See the possible digit sums mod 3:
    //                                      0,1,2,0,1,2,0,1,2,0, 4 numbers div by 3,
    //                                      1,2,0,1,2,0,1,2,0,1, 3                3,
    //                                      2,0,1,2,0,1,2,0,1,2. 3                3.
    // if we cycle through 8 double digits, i.e. 8 of 00,11,22,33,44,55,66,77,88,99, the digit sums are adjusted by
    // .                                               0, 2, 1, 0, 2, 1, 0, 2, 1, 0. Similarly impossible.
    // 8 triple digits: 000,111,222,333,444,555,666,777,888,999
    // adjusts digit sum by 0,...,0 (!)
    // pattern emerges. need to replace triples, sextuples or 9 digits at once.

    (for
      p <- primes.dropWhile(_ <= 56003)
      digs = digits(p)
      if (0 to 9).exists(d =>
        digs.count(_ == d) > 0
          && digs.count(_ == d) % 3 == 0
          && (0 to 9).count(j =>
            digs.map({ case d => { println(digs); j } }).toInt.isPrime
          ) >= 8
      )
    yield p).head

  case 52 =>
    (for
      x <- (100_000 until 1_000_000 / 6).to(LazyList)
      digs = digits(x).sorted
      if digits(6 * x).sorted == digs
        && digits(5 * x).sorted == digs
        && digits(4 * x).sorted == digs
        && digits(3 * x).sorted == digs
        && digits(2 * x).sorted == digs
    yield x).head
  case _ => ???

@main def main(args: Int*): Unit =
  if args.length == 0 then
    try from(1, 1).foreach(problem => pprint(problem, pe(problem)))
    catch case e: NotImplementedError => ()
  else args.foreach(problem => pprint(problem, pe(problem)))
