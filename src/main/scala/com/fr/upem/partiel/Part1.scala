package com.fr.upem.partiel

import java.time.Instant
import java.time.temporal.ChronoUnit.YEARS


// Part 1 (10pts)
object Part1 {

  // 1.1 Apply 'mul2' using pattern matching to the given integer (.5pts)
  def mul2(i: Int): Int = i * 2

  def applyMul2WithPatternMatching(i: Option[Int]): Option[Int] = i match {
    case Some(x) => Some(2 * x)
    case None => None
  }

  // 1.2 Apply 'mul2' WITHOUT using pattern matching to the given integer (.5pts)
  def applyMul2WithoutPatternMatching(i: Option[Int]): Option[Int] ={
    if(i.isDefined)Option(i.get*2)
    else None
  }

  // 1.3 Refactor the following code using pattern matching (1pts)
  sealed trait Animal

  case object Cat extends Animal

  case object Bird extends Animal

  case class Dog(age: Int) extends Animal

  def formatAnimal(animal: Animal): String = animal match {
    case Cat =>  "It's a cat"
    case  Bird =>   "It's a bird"
    case x if(x.isInstanceOf[Dog]) =>  s"It's a ${animal.asInstanceOf[Dog].age} year old dog"
    case _ => throw new RuntimeException("This should not happen but I'm a Java developer !")
  }



  // 1.4 Find the index of the given element if any, use recursivity (1pts)
  def indexOf[A](l: List[A], a: A): Option[Int] =
    l match {
      case Nil => None
      case x :: xs => Some(l.foldRight(0)((x, compteur) =>
        if (x == a) compteur
        else compteur + 1
      ))
    }


  // 1.5 Throw away all errors (.5pts)
  case class Error(message: String)

  def parserE[A](l: Either[Error, A]): A = l match {
    case Right(x) => x
    case Left(x) => asInstanceOf

  }

  def keepValid[A](l: List[Either[Error, A]]): List[A] = l match {
    case Nil => Nil
    case x :: xs if x == Left(Error("err")) => keepValid(xs)
    case x :: xs => parserE(x) :: keepValid(xs)
  }

  // 1.6 Aggregate values (.5pts)
  def aggregate[A](l: List[A], combine: (A, A) => A, empty: A): A = l match {
    case Nil => empty
    case x :: xs => xs.foldLeft(x)(combine)

  }


  // 1.7 Aggregate valid values (.5pts)
  def aggregateValid[A](l: List[Either[Error, A]], combine: (A, A) => A, empty: A): A = l match{
    case Nil=>empty
    case x::xs if x == Left(Error("err")) => aggregateValid(xs,combine,empty)
    case x::xs => keepValid(xs).foldLeft(parserE(x))(combine)

  }

  sealed trait Monoid[A] {
    def identity: A
    def combine(a1: A, a2: A): A
  }

  // 1.8 Create the Monoid typeclass and rewrite the above "aggregateValid" (.5pts)
  implicit def aggregateValidM:Monoid[Int]=new Monoid[Int] {
     override def identity: Int =identity
      override def combine(a1: Int, a2: Int): Int =a1+a2

  }




  // 1.9 Implement the Monoid typeclass for Strings and give an example usage with aggregateValidM (.5pts)
    implicit def aggregateValidMSecond:Monoid[String]=new Monoid[String] {
      override def identity:String=identity

      override def combine(a1: String, a2: String): String = a1+a2

    }
  // 1.10 Refactor the following object oriented hierarchy with an ADT (1.5pts)
  abstract class FinancialAsset {
    def computeEarnings: Double
  }
  sealed trait FinancialAsset2 {
    def computeEarnings: Double
  }


  abstract class FlatRateAsset extends FinancialAsset {
    protected val rate: Double
    protected val amount: Double

    override def computeEarnings: Double = amount + (amount * rate)
  }

  sealed trait FlatRateAsset2 extends FinancialAsset {
    protected val rate: Double
    protected val amount: Double

    override def computeEarnings: Double = amount + (amount * rate)
  }

  object LivretA {
    val Rate: Double = 0.75
  }

  object LivretA2 {
    def Rate: Double = 0.75
  }

  class LivretA(override val amount: Double) extends FlatRateAsset {
    override protected val rate: Double = LivretA.Rate
  }

  object Pel {
    val Rate: Double = 1.5
    val GovernmentGrant: Int = 1525
  }
  object Pel2 {
    def Rate: Double = 1.5
    def GovernmentGrant: Int = 1525
  }

  class Pel(override val amount: Double, creation: Instant) extends FlatRateAsset {
    override protected val rate: Double = Pel.Rate
    override def computeEarnings: Double =
      if (Instant.now().minus(4, YEARS).isAfter(creation))
        super.computeEarnings + Pel.GovernmentGrant
      else
        super.computeEarnings
  }

  object CarSale {
    val StateHorsePowerTaxation: Int = 500
  }

  object CarSale2 {
    def StateHorsePowerTaxation: Int = 500
  }
  class CarSale(amount: Int, horsePower: Int) extends FinancialAsset {
    override def computeEarnings: Double = amount - (CarSale.StateHorsePowerTaxation * horsePower)
  }

  // 1.11 Extract the "computeEarnings" logic of the above hierarchy
  // into an "Earnings" typeclass and create the adequate instances (1.5pts)

  // 1.12 Rewrite the following function with your typeclass (.5pts)
  def computeTotalEarnings(assets: List[FinancialAsset]): Double =
    assets.map(_.computeEarnings).sum

  // 1.13 Enrich the "String" type with an "atoi" extension method that parses the
  // given String to an Int IF possible (1pts)
      sealed trait rt
      final case class Intg(a:Integer)extends rt
      final case class Strg(a:String)extends rt
      implicit class strgModifier(val a:String)extends AnyVal {
        def atoi: rt =
          if (a.matches("[0-9]+$")) Intg(Integer.parseInt(a))
          else Strg(a)
      }
}
