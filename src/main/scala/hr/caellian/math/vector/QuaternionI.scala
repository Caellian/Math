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

package hr.caellian.math.vector

import hr.caellian.math.matrix.MatrixI

/**
  * Object containing utility functions for quaternion initialization.
  *
  * @author Caellian
  */
object QuaternionI {
  /**
    * Creates a new quaternion (4D vector) using {@link VectorI} class from
    * given rotation matrix.
    *
    * @param rotationMatrix rotation matrix to use for quaternion creation.
    * @return resulting quaternion.
    */
  def fromRotationMatrix(rotationMatrix: MatrixI): VectorI = {
    require(rotationMatrix.data.length == 4 && rotationMatrix.data.forall(_.length == 4))
    var x = 0
    var y = 0
    var z = 0
    var w = 0
    val trace: Int = rotationMatrix(0)(0) + rotationMatrix(1)(1) + rotationMatrix(2)(2)

    if (trace > 0) {
      val s: Int = (0.5f / Math.sqrt(trace + 1.0f)).toInt
      x = (0.25f / s).toInt
      y = (rotationMatrix(1)(2) - rotationMatrix(2)(1)) * s
      z = (rotationMatrix(2)(0) - rotationMatrix(0)(2)) * s
      w = (rotationMatrix(0)(1) - rotationMatrix(1)(0)) * s
    }
    else {
      if (rotationMatrix(0)(0) > rotationMatrix(1)(1) && rotationMatrix(0)(0) > rotationMatrix(2)(2)) {
        val s: Int = (2.0f * Math.sqrt(1.0f + rotationMatrix(0)(0) - rotationMatrix(1)(1) - rotationMatrix(2)(2))).toInt
        x = (rotationMatrix(1)(2) - rotationMatrix(2)(1)) / s
        y = (0.25f * s).toInt
        z = (rotationMatrix(1)(0) + rotationMatrix(0)(1)) / s
        w = (rotationMatrix(2)(0) + rotationMatrix(0)(2)) / s
      }
      else if (rotationMatrix(1)(1) > rotationMatrix(2)(2)) {
        val s: Int = (2.0f * Math.sqrt(1.0f + rotationMatrix(1)(1) - rotationMatrix(0)(0) - rotationMatrix(2)(2))).toInt
        x = (rotationMatrix(2)(0) - rotationMatrix(0)(2)) / s
        y = (rotationMatrix(1)(0) + rotationMatrix(0)(1)) / s
        z = (0.25f * s).toInt
        w = (rotationMatrix(2)(1) + rotationMatrix(1)(2)) / s
      }
      else {
        val s: Int = (2.0f * Math.sqrt(1.0f + rotationMatrix(2)(2) - rotationMatrix(0)(0) - rotationMatrix(1)(1))).toInt
        x = (rotationMatrix(0)(1) - rotationMatrix(1)(0)) / s
        y = (rotationMatrix(2)(0) + rotationMatrix(0)(2)) / s
        z = (rotationMatrix(1)(2) + rotationMatrix(2)(1)) / s
        w = (0.25f * s).toInt
      }
    }

    VectorI(x, y, z, w).normalized
  }

  def initRotationQuaternion(axis: VectorI, angle: Float): VectorI = {
    val sinHalfAngle = Math.sin(angle / 2).toInt
    VectorI(axis(X) * sinHalfAngle, axis(Y) * sinHalfAngle, axis(Z) * sinHalfAngle, Math.cos(angle / 2).toInt)
  }
}
