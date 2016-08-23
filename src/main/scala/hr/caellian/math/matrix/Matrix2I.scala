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

package hr.caellian.math.matrix


/**
  * Utility object containing initializers for basic 2x2 matrices.
  * These functions should be used instead of any provided by {@link MatrixI}
  * wherever possible as they are supposed to perform faster.
  *
  * @author Caellian
  */
object Matrix2I {
  /**
    * Initializes rotation matrix using degrees.
    *
    * @param degrees Degrees to rotate objects multiplied by this matrix in positive direction.
    * @return rotation matrix
    */
  def initRotation(degrees: Int): MatrixI = {
    if (degrees == 0) {
      return initIdentityMatrix()
    }

    val result = Array.ofDim[Int](2, 2)
    result(0)(0) = Math.cos(Math.toRadians(degrees)).toInt
    result(0)(1) = Math.sin(Math.toRadians(degrees)).toInt
    result(1)(0) = -Math.sin(Math.toRadians(degrees)).toInt
    result(1)(1) = Math.cos(Math.toRadians(degrees)).toInt

    new MatrixI(result)
  }

  /**
    * Initializes a new 2x2 identity matrix.
    *
    * @return identity matrix.
    */
  def initIdentityMatrix(): MatrixI = {
    new MatrixI(Array.tabulate[Int](2, 2)((x, y) => {
      if (x == y) {
        1
      } else {
        0
      }
    }))
  }

  /**
    * Initializes a new 2x2 scaling matrix.
    *
    * @param scale scale.
    * @return scale matrix.
    */
  def initScalingMatrix(scale: Array[Int]): MatrixI = {
    assert(scale.length == 2, "Translation must have 2 values!")
    new MatrixI(Array.tabulate[Int](2, 2)((x, y) => {
      if (x == y) {
        scale(x)
      } else {
        0
      }
    }))
  }
}
