package tagless

import scala.quoted._

trait Symantics {

  type repr[_, _]

  def num(x: Double): repr[Double, Double]

  def bool(b: Boolean): repr[Boolean, Boolean]

  //def lam[A: Type, B: Type](f: repr[A, A] => repr[B, B]): repr[repr[A, A] => repr[B, B], A => B]
  def lam[SA: Type, DA: Type, SB: Type, DB: Type](f: repr[SA, DA] => repr[SB, DB]): repr[repr[SA, DA] => repr[SB, DB], DA => DB]

  //def app[A, B](f: repr[repr[A, A] => repr[B, B], A => B], arg: repr[A, A]): repr[B, B]
  def app[SA, DA, SB, DB](f: repr[repr[SA, DA] => repr[SB, DB], DA => DB], arg: repr[SA, DA]): repr[SB, DB]

  //def fix[A: Type, B: Type](f: repr[repr[A, A] => repr[B, B], A => B] => repr[repr[A, A] => repr[B, B], A => B]): repr[repr[A, A] => repr[B, B], A => B]
  def fix[SA: Type, DA: Type, SB: Type, DB: Type](f: repr[repr[SA, DA] => repr[SB, DB], DA => DB] => repr[repr[SA, DA] => repr[SB, DB], DA => DB]): repr[repr[SA, DA] => repr[SB, DB], DA => DB]

  def neg(x: repr[Double, Double]): repr[Double, Double]

  def add(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]

  def mul(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]

  def div(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]

  def leq(x: repr[Double, Double], y: repr[Double, Double]): repr[Boolean, Boolean]

  def if_[A](cond: repr[Boolean, Boolean], e1: => repr[A, A], e2: => repr[A, A]): repr[A, A]

}

object commonTypes {

  type Id[A] = A
  type StatDyn[A, B] = (Option[A], Expr[B])

}