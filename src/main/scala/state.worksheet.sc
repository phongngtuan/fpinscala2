trait RNG:
  def nextInt: (Int, RNG)

object RNG:
  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
  
  // Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i, r) = rng.nextInt
    (if i < 0 then -(i + 1) else i, r)

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) =
    val (i, r) = nonNegativeInt(rng)
    (i.toDouble / (Int.MaxValue.toDouble + 1)) -> r

  
  // Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)

  // Exercise 6.4
  /**
   *  Simple recursive function
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if count <= 0 then
      (Nil, rng)
    else
      val (i, r) = rng.nextInt
      val (tail, r2) = ints(count-1)(r)
      (i :: tail, r2)

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) =
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if count <= 0 then
        (xs, r)
      else
        val (x, r2) = r.nextInt
        go(count - 1, r2, x :: xs)
    go(count, rng, List())