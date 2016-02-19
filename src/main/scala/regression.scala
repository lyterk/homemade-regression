package com.lyterk.LR

import scala.math.pow

object LinearRegression extends App {

  val x: List[Double] = List(65.21, 64.75, 65.26, 65.76, 65.96)
  val y: List[Double] = List(67.25, 66.39, 66.12, 65.70, 66.64)

  /* This is a good direction to go for library writing, but for now I'm just going to assume that I'm taking a list of doubles.
  def types(l: List[T]): List[Double] = T match { // Also work on taking data types other than list.
    case i: List[Double] => i
    case j: List[_] => i.map(_.toDouble) 
    }
   */

  def mean(l: List[Double]): Double = l.sum / l.length

  def variance(l: List[Double]): Double = {
    val m = mean(l)
    l.map(i => pow(i - m, 2)).sum / l.length - 1
  }  

  def covariance(x: List[Double], y: List[Double]): Double = {
    val z = x.zip(y)
    val mk = mean(x)
    val ml = mean(y)
    z.map(i => (i._1 - mk) * (i._2 - ml)).sum / (x.length - 1)
  }

  /** Takes two list variables of type Double, and returns values in tuples pertinent to a linear regression. The values returned are: (Intercept, Slope) */
  def linearRegression(k: List[Double], l: List[Double]): (Double, Double) = {
    val m = k.zip(l)
    val mk = mean(k)
    val ml = mean(l)
    val slope = (m.map{case (a,b) => a * b}.sum - k.length * mk * ml) / (k.map(i => i * i).sum - k.length * mk * mk)
    val intercept = ml - slope * mk    
    (intercept, slope)
  }

  println(linearRegression(x,y))
}
