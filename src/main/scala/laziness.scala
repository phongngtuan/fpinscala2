package laziness

import LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList

  def take(n: Int): LazyList[A] = this match
      case Cons(h, t) if n > 1  => cons(h(), t().take(n-1))
      case Cons(h, t) if n == 1 => cons(h(), Empty)
      case _                    => Empty

  // def takeWhile(p: A => Boolean): LazyList[A] = this match
  //     case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
  //     case _                    => Empty

  // def exists(p: A => Boolean): Boolean = this match
  //   case Cons(h, t) => p(h()) || t().exists(p)
  //   case Empty      => false

  /**
    * f may choose to not look at the accumulation by not evaluating the second parameter
    * but this will blow the stack because of recursion
    */
  def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match
    case Cons(h, t) => f(h(), t().foldRight(acc)(f))
    case Empty      => acc

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)
  
  def takeWhile(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, acc) => if p(a) then cons(a, acc) else empty)

  def map[B](f: A => B): LazyList[B] =
    foldRight(empty)((a, acc) => cons(f(a), acc))

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, acc) => if p(a) then cons(a, acc) else empty)

  def append[A2>:A](that: => LazyList[A2]): LazyList[A2] =
    foldRight(that)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty)((a, acc) => f(a).append(acc))

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption
    

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  def ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = cons(a, continually(a))

  def from(n: Int): LazyList[Int] = cons(n, from(n+1))

  def fibs: LazyList[Int] =
    def go(a: Int, b: Int): LazyList[Int] =
      lazy val c = a + b
      cons(b, go(b, c))
    go(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _            => empty

  def fibsViaUnfold: LazyList[Int] =
    unfold((0, 1)) { case (a, b) =>
      Some(a -> (b, a+b))
    }

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(0)(x => Some(x -> (x+1)))
  
  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(a)(a => Some(a -> a))

  def onesViaUnfold: LazyList[Int] = unfold(())(_ => Some(1, ()))
