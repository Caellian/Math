/*
 * The MIT License (MIT)
 * Copyright (c) 2016 Tin Švagelj <tin.svagelj.email@gmail.com> a.k.a. Caellian
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */

package hr.caellian.math.util

import hr.caellian.math.matrix.MatrixD
import org.apache.commons.math3.linear.{Array2DRowRealMatrix, LUDecomposition}

/**
  *
  */
object InverseTest extends App {
  val startingArrayOur: Array[Array[Double]] = Array(Array(2d, 5d, 3d, 9d), Array(3d, 2d, 6d, 2d), Array(5d, 8d, 1d, 2d), Array(5d, 4d, 2d, 0d))
  val startingArrayApache: Array[Array[Double]] = Array(Array(2d, 5d, 3d, 9d), Array(3d, 2d, 6d, 2d), Array(5d, 8d, 1d, 2d), Array(5d, 4d, 2d, 0d))

  val our = MatrixD(startingArrayOur)
  val ourTimer = System.currentTimeMillis()
  val ourInverse = our.inverse()
  val ourTime = System.currentTimeMillis() - ourTimer

  val apache = new Array2DRowRealMatrix(startingArrayApache)
  val apacheTimer = System.currentTimeMillis()
  val apacheInverse = new LUDecomposition(apache).getSolver.getInverse
  val apacheTime = System.currentTimeMillis() - apacheTimer

  println("Our inverse:")
  println(ourInverse)
  println("Our time: " + ourTime + "ms")

  println("Apache inverse:")
  println(apacheInverse)
  println("Apache time: " + apacheTime + "ms")
}
