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

import hr.caellian.math.vector.VectorI

/**
  * Utility object containing initializers for basic 4x4 matrices.
  * These functions should be used instead of any provided by {@link MatrixI}
  * wherever possible as they are supposed to perform faster.
  *
  * @author Caellian
  */
object Matrix4I {
  /**
    * Initializes perspective transformation matrix.
    *
    * @param fov         field of view.
    * @param aspectRatio aspect ration.
    * @param clipNear    front clipping position.
    * @param clipFar     back clipping position.
    * @return perspective transformation matrix.
    */
  def initPerspectiveMatrix(fov: Int, aspectRatio: Int, clipNear: Int, clipFar: Int): MatrixI = {
    val result = Array.ofDim[Int](4, 4)
    val fowAngle: Int = Math.tan(fov / 2).toInt
    val clipRange: Int = clipNear - clipFar
    result(0)(0) = (1.0f / (fowAngle * aspectRatio)).toInt
    result(0)(1) = 0
    result(0)(2) = 0
    result(0)(3) = 0
    result(1)(0) = 0
    result(1)(1) = (1.0f / fowAngle).toInt
    result(1)(2) = 0
    result(1)(3) = 0
    result(2)(0) = 0
    result(2)(1) = 0
    result(2)(2) = (-clipNear - clipFar) / clipRange
    result(2)(3) = 2 * clipFar * clipNear / clipRange
    result(3)(0) = 0
    result(3)(1) = 0
    result(3)(2) = 1
    result(3)(3) = 0
    new MatrixI(result)
  }

  /**
    * Initializes orthographic transformation matrix.
    *
    * @param left     left clipping position.
    * @param right    right clipping position.
    * @param bottom   bottom clipping position.
    * @param top      top clipping position.
    * @param clipNear front clipping position.
    * @param clipFar  back clipping position.
    * @return orthographic transformation matrix
    */
  def initOrthographicMatrix(left: Int,
                             right: Int,
                             bottom: Int,
                             top: Int,
                             clipNear: Int,
                             clipFar: Int): MatrixI = {
    val result = Array.ofDim[Int](4, 4)
    val width: Int = right - left
    val height: Int = top - bottom
    val depth: Int = clipFar - clipNear
    result(0)(0) = 2 / width
    result(0)(1) = 0
    result(0)(2) = 0
    result(0)(3) = -(right + left) / width
    result(1)(0) = 0
    result(1)(1) = 2 / height
    result(1)(2) = 0
    result(1)(3) = -(top + bottom) / height
    result(2)(0) = 0
    result(2)(1) = 0
    result(2)(2) = -2 / depth
    result(2)(3) = -(clipFar + clipNear) / depth
    result(3)(0) = 0
    result(3)(1) = 0
    result(3)(2) = 0
    result(3)(3) = 1
    new MatrixI(result)
  }

  /**
    * Initializes rotation matrix using forward and up vector by calculating
    * right vector.
    *
    * @param forward forward 3f vector.
    * @param up      up 3f vector.
    * @return rotation matrix.
    */
  def initRotationMatrix(forward: VectorI, up: VectorI): MatrixI = {
    val f = forward.normalized
    val r = up.normalized.cross(f)
    val u = f.cross(r)
    Matrix4I.initRotationMatrix(f, u, r)
  }

  /**
    * Initializes rotation matrix using forward, up and right vector.
    *
    * @param forward forward 3f vector.
    * @param up      up 3f vector.
    * @param right   right 3f vector.
    * @return rotation matrix.
    */
  def initRotationMatrix(forward: VectorI, up: VectorI, right: VectorI): MatrixI = {
    val result = Array.ofDim[Int](4, 4)
    result(0)(0) = right(X)
    result(0)(1) = right(Y)
    result(0)(2) = right(Z)
    result(0)(3) = 0
    result(1)(0) = up(X)
    result(1)(1) = up(Y)
    result(1)(2) = up(Y)
    result(1)(3) = 0
    result(2)(0) = forward(X)
    result(2)(1) = forward(Y)
    result(2)(2) = forward(Z)
    result(2)(3) = 0
    result(3)(0) = 0
    result(3)(1) = 0
    result(3)(2) = 0
    result(3)(3) = 1
    new MatrixI(result)
  }

  /**
    * Initializes rotation matrix using a rotation quaternion.
    *
    * @param quaternion quaternion to use for initialization.
    * @return rotation matrix.
    */
  def initRotationMatrix(quaternion: VectorI): MatrixI = {
    val forward = VectorI(2 * (quaternion(0) * quaternion(2) - quaternion(3) * quaternion(1)),
      2 * (quaternion(1) * quaternion(2) + quaternion(3) * quaternion(0)),
      1 - 2 * (quaternion(0) * quaternion(0) + quaternion(1) * quaternion(1)))
    val up = VectorI(2 * (quaternion(0) * quaternion(1) + quaternion(3) * quaternion(2)),
      1 - 2 * (quaternion(0) * quaternion(0) + quaternion(2) * quaternion(2)),
      2 * (quaternion(1) * quaternion(2) - quaternion(3) * quaternion(0)))
    val right = VectorI(1 - 2 * (quaternion(1) * quaternion(1) + quaternion(2) * quaternion(2)),
      2 * (quaternion(0) * quaternion(1) - quaternion(3) * quaternion(2)),
      2 * (quaternion(0) * quaternion(2) + quaternion(3) * quaternion(1)))

    Matrix4I.initRotationMatrix(forward, up, right)
  }

  /**
    * Initializes a rotation matrix.
    *
    * @param a     first argument.
    * @param b     second argument.
    * @param angle degrees to rotate in objects multiplied by this rotation matrix.
    * @return plane rotation matrix.
    */
  def initPlaneRotation(a: Int, b: Int, angle: Int): MatrixI = {
    val ai = a - 1
    val bi = b - 1
    val matrix = Array.ofDim[Int](4, 4)
    for (x <- matrix.indices) {
      for (y <- matrix(0).indices) {
        val value = (y, x) match {
          case (`ai`, `ai`) => Math.cos(Math.toRadians(angle)).toInt
          case (`bi`, `bi`) => Math.cos(Math.toRadians(angle)).toInt
          case (`ai`, `bi`) => -Math.sin(Math.toRadians(angle)).toInt
          case (`bi`, `ai`) => Math.sin(Math.toRadians(angle)).toInt
          case _ => {
            if (x == y) {
              1
            } else {
              0
            }
          }
        }
        matrix(y)(x) = value
      }
    }
    MatrixI(matrix)
  }

  /**
    * Initializes a new 4x4 scaling matrix.
    *
    * @param scale scale.
    * @return scale matrix.
    */
  def initScalingMatrix(scale: Array[Int]): MatrixI = {
    assert(scale.length == 4, "Translation must have 4 values!")
    new MatrixI(Array.tabulate[Int](4, 4)((x, y) => {
      if (x == y) {
        scale(x)
      } else {
        0
      }
    }))
  }

  /**
    * Initializes a new 4x4 translation matrix.
    *
    * @param location relative location.
    * @return translation matrix.
    */
  def initTranslationMatrix(location: Array[Int]): MatrixI = {
    assert(location.length == 3, "Translation must have 3 coordinates!")
    new MatrixI(Array.tabulate[Int](4, 4)((x,
                                           y) => {
      if (x == y) {
        1
      } else if (y == location.length && x < location.length) {
        location(x)
      } else {
        0
      }
    }))
  }

  /**
    * Initializes a new 4x4 identity matrix.
    *
    * @return identity matrix.
    */
  def initIdentityMatrix(): MatrixI = {
    new MatrixI(Array.tabulate[Int](4, 4)((x, y) => {
      if (x == y) {
        1
      } else {
        0
      }
    }))
  }
}
