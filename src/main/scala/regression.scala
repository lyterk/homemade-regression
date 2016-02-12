package com.lyterk.LR

import scala.math.pow

object LR extends App {

  val x: List[Double] = List(65.21, 64.75, 65.26, 65.76, 65.96)
  val y: List[Double] = List(67.25, 66.39, 66.12, 65.70, 66.64)

  /* This is a good direction to go for library writing, but for now I'm just going to assume that I'm taking a list of doubles.
  def types(l: List[T]): List[Double] = T match { // Also work on taking data types other than list.
    case i: List[Double] => i
    case j: List[_] => i.map(_.toDouble) // Bit of a hammer. Will work on more diverse situations later.
    }
   */

  def mean(l: List[Double]): Double = l.sum / l.length

  def variance(l: List[Double]): Double = {
    val m = mean(l)
    l.map(i => pow(i - m, 2)).sum / l.length - 1
  }  

  def covariance(k: List[Double], l: List[Double]): Double = {
    val z = k.zip(l)
    val mk = mean(k)
    val ml = mean(l)
    z.map(i => (i._1 - mk) * (i._2 - ml)).sum / (k.length - 1)
  }

  def regression(k: List[Double], l: List[Double]): (Double, Double) = {
    

}
