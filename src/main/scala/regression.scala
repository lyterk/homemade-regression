package com.lyterk.LR

import scala.math.pow

val x: List[Double] = List(65.21, 64.75, 65.26, 65.76, 65.96)
val y: List[Double] = List(67.25, 66.39, 66.12, 65.70, 66.64)

/* This is a good direction to go for library writing, but for now I'm just going to assume that I'm taking a list of doubles.
 def types(l: List[T]): List[Double] = T match { // Also work on taking data types other than list.
 case i: List[Double] => i
 case j: List[_] => i.map(_.toDouble) 
 }
 */

object LinearRegression(x: List[Double], y: List[Double) {

  lazy val z = x.zip(y)
  lazy val mux = mean(x)
  lazy val muy = mean(y)
  lazy val varx = variance(x)
  lazy val vary = variance(y)
  lazy val cov = covariance(x, y)
  lazy val linreg = linearRegression(x, y)

  def mean(l: List[Double]): Double = l.sum / l.length

  def variance(l: List[Double]): Double = {
    val m = mean(l)
    l.map(i => pow(i - m, 2)).sum / l.length - 1
  }  

  def covariance(x: List[Double], y: List[Double]): Double = {    
    z.map(i => (i._1 - mk) * (i._2 - ml)).sum / (x.length - 1)
  }

  /** Calculates simple linear regression.
    @see <ahref="https://en.wikipedia.org/wiki/Simple_linear_regression">The wikipedia page on linear regression</a>
    @param k A list of doubles
    @param l Another list of doubles
    @return (Intercept, Slope) 
    */
  def linearRegression(k: List[Double], l: List[Double]): (Double, Double) = {
    val m = k.zip(l)
    val slope = (m.map{case (a,b) => a * b}.sum - k.length * mux * muy) / (k.map(i => i * i).sum - k.length * mux * mux)
    val intercept = muy - slope * mux    
    (intercept, slope)
  }  

  def explainedSumSquares(

  println(linearRegression(x,y))
}
