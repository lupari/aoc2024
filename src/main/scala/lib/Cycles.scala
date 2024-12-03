package lib

import scala.annotation.tailrec

object Cycles {

  def brent[A](f: A => A, x0: A): (Int, Int) = {
    val lambda = findLambda(f, x0)
    val mu     = findMu(f, x0, lambda)
    (lambda, mu)
  }

  def cycle[A](f: A => A, x0: A): Seq[A] = {
    val (lambda, mu) = brent(f, x0)
    (1 until mu + lambda)
      .foldLeft(Seq(x0))((list, _) => f(list.head) +: list)
      .reverse
      .drop(mu)
  }

  def findLambda[A](f: A => A, x0: A): Int = {
    findLambdaRec(f, tortoise = x0, hare = f(x0), power = 1, lambda = 1)
  }

  def findMu[A](f: A => A, x0: A, lambda: Int): Int = {
    val hare = (0 until lambda).foldLeft(x0)((x, _) => f(x))
    findMuRec(f, tortoise = x0, hare, mu = 0)
  }

  @tailrec
  private def findLambdaRec[A](
      f: A => A,
      tortoise: A,
      hare: A,
      power: Int,
      lambda: Int
  ): Int = {
    if (tortoise == hare) {
      lambda
    } else {
      val (newTortoise, newPower, newLambda) = if (power == lambda) {
        (hare, power * 2, 0)
      } else {
        (tortoise, power, lambda)
      }
      findLambdaRec(f, newTortoise, f(hare), newPower, newLambda + 1)
    }
  }

  @tailrec
  private def findMuRec[A](f: A => A, tortoise: A, hare: A, mu: Int): Int = {
    if (tortoise == hare) {
      mu
    } else {
      findMuRec(f, f(tortoise), f(hare), mu + 1)
    }
  }

  /*  def main(args: Array[String]): Unit = {
    val f            = (x: Int) => (x * x + 1) % 255
    val x0           = 3
    val (lambda, mu) = brent(f, x0)
    val list         = cycle(f, x0)

    println("Cycle length = " + lambda)
    println("Start index  = " + mu)
    println("Cycle        = " + list.mkString(","))
  }*/

}
