package org.merlin.aoc
package lib
package fp


// category theory

trait Semigroup[A]:
  def combine(a0: A, a1: A): A

trait Monoid[A] extends Semigroup[A]:
  def zero: A

object Monoid:
  def instance[A](z: A, f: (A, A) => A): Monoid[A] = new Monoid[A]:
    override def zero: A                  = z
    override def combine(a0: A, a1: A): A = f(a0, a1)

given NumericMonoid: [A: Numeric as N] => Monoid[A] = Monoid.instance(N.zero, N.plus)

given MapMonoid: [A, B: Semigroup as S] => Monoid[Map[A, B]]:
  def zero: Map[A, B] = Map.empty

  def combine(ab0: Map[A, B], ab1: Map[A, B]): Map[A, B] =
    ab1.foldLeft(ab0):
      case (ab, (a1, b1)) =>
        ab.updatedWith(a1):
          case Some(b0) => Some(S.combine(b0, b1))
          case None     => Some(b1)

given ListMonoid: [A] => Monoid[List[A]] = Monoid.instance(List.empty, _ ++ _)

given SetMonoid: [A] => Monoid[Set[A]] = Monoid.instance(Set.empty, _ ++ _)

given Tuple2Monoid: [A: Monoid as MA, B: Monoid as MB] => Monoid[(A, B)]:
  def zero: (A, B)                            = (MA.zero, MB.zero)
  def combine(a0: (A, B), a1: (A, B)): (A, B) = (MA.combine(a0._1, a1._1), MB.combine(a0._2, a1._2))

extension [A: Semigroup as S](self: A) def |+|(a: A): A = S.combine(self, a)

// boolean extensions

extension (self: Boolean) def ??[A](a: => A)(using M: Monoid[A]): A = if self then a else M.zero

// iterable extensions

extension [A](self: Iterable[A])
  def suml(using M: Monoid[A]): A = self.foldLeft(M.zero)(M.combine)

  def foldMap[B](f: A => B)(using M: Monoid[B]) = self.foldLeft(M.zero): (b, a) =>
    M.combine(b, f(a))

// map extensions

extension [A, B](self: Map[A, B]) def suml(using M: Monoid[B]): B = self.values.foldLeft(M.zero)(M.combine)
