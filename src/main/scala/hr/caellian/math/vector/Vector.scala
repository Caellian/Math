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

import java.nio.Buffer

import hr.caellian.math.matrix.Matrix
import hr.caellian.math.util.{DataWrapper, Replicable}

/**
  * Generic Vector trait defined for stable code infrastructure and consistency.
  * Most basic Vector variables and functions can be found here.
  *
  * @tparam T vector data type.
  * @author Caellian
  */
trait Vector[T <: AnyVal] extends Replicable[Vector[T]] with DataWrapper[Vector[T], Array[T]] {
  /**
    * Size of this vector.
    */
  val size: Int = data.length

  def apply(index: Int): T = data(index)

  /**
    * @return this vector.
    */
  def unary_+ = this

  /**
    * @return new conjugated vector.
    */
  def unary_- : Vector[T]

  /**
    * Adds two vectors together and returns resulting vector.
    * In order to add to matrices together, they must be of same size.
    *
    * @return result of vector addition.
    */
  def +(other: Vector[T]): Vector[T]

  /**
    * Subtracts other vector from this one and returns resulting vector.
    * In order to subtract one vector from another, both vectors must be of same size.
    *
    * @return result of vector subtraction.
    */
  def -(other: Vector[T]): Vector[T]

  /**
    * Multiplies two vectors together and returns resulting vector.
    * In order to add to multiply vectors together, they must be of same size.
    *
    * @return result of vector multiplication.
    */
  def *(other: Vector[T]): Vector[T]

  /**
    * Divides this vector with other and returns resulting vector.
    * In order to divide one vector from another, both vectors must be of same size.
    *
    * @return result of vector division.
    */
  def /(other: Vector[T]): Vector[T]

  /**
    * Performs scalar addition on this vector and returns resulting vector.
    *
    * @return result of scalar vector addition.
    */
  def +(value: T): Vector[T]

  /**
    * Performs scalar subtraction on this vector and returns resulting vector.
    *
    * @return result of scalar vector subtraction.
    */
  def -(value: T): Vector[T]

  /**
    * Performs scalar multiplication on this vector and returns resulting vector.
    *
    * @return result of scalar vector multiplication.
    */
  def *(value: T): Vector[T]

  /**
    * Performs scalar division on this vector and returns resulting vector.
    *
    * @return result of scalar vector division.
    */
  def /(value: T): Vector[T]

  /**
    * @return true if this vector is equal to other vector.
    */
  def ==(o: Vector[T]): Boolean = {
    equals(o)
  }

  /**
    * @return max value of member of this vector.
    */
  def max: T

  /**
    * @return new vector containing absolute values of this vector.
    */
  def absolute: Vector[T]

  /**
    * @return new vector with normalized values of this one.
    */
  def normalized: Vector[T]

  /**
    * @return magnitude of this vector.
    */
  def magnitude: T

  /**
    * Calculates distance from this to other vector.
    *
    * @return distance between this and other vector.
    */
  def distanceTo(other: Vector[T]): T

  /**
    *
    * @return dot product of two vectors.
    */
  def dot(other: Vector[T]): T

  /**
    * Returns cross product of this and other vector.
    *
    * @return cross product.
    */
  def cross(other: Vector[T]): Vector[T]

  /**
    * Rotates this vector using rotation matrix.
    *
    * @return rotated vector.
    */
  def rotated(rotationMatrix: Matrix[T]): Vector[T]

  /**
    * Linearly interpolates between two vectors.
    *
    * @return linear interpolation.
    */
  def lerp(destination: Vector[T], percent: T): Vector[T]

  /**
    * Vertical matrix containing data of this vector.
    */
  val verticalMatrix: Matrix[T]

  /**
    * Horizontal matrix containing data of this vector.
    */
  val horizontalMatrix: Matrix[T]

  /**
    * @return array containing data of this vector.
    */
  def asArray: Array[T] = {
    data.clone()
  }

  /**
    * @return buffer containing data of this vector.
    */
  def asBuffer: Buffer

  /**
    * @return clone of this vector.
    */
  override def replicated: Vector[T]

  /**
    * @return true if this vector is equal to other vector.
    */
  override def equals(o: Any): Boolean = {
    if (this == o) {
      return true
    }
    if (!o.isInstanceOf[Vector[T]]) {
      return false
    }
    val vector = o.asInstanceOf[Vector[T]]
    data sameElements vector.data
  }

  /**
    * Vector hashcode depends on vector data and will change is vector data is modified!
    *
    * @return hashcode of this vector.
    */
  override def hashCode: Int

  /**
    * @return string representation of this vector.
    */
  override def toString: String = {
    val builder = new StringBuilder()

    builder.append("(")
    for (it <- this.data) {
      builder.append(s"$it, ")
    }
    builder.setLength(builder.lastIndexOf(","))
    builder.append(")")

    builder.mkString
  }
}
