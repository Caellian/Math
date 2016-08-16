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

package hr.caellian.math.geometry

import java.nio.{Buffer, ByteBuffer, ByteOrder, DoubleBuffer}

import hr.caellian.math.util.Data
import org.apache.commons.math3.util.FastMath

/**
  * @author caellian
  *         Created on 31/07/16 at 17:05 CET.
  */
/**
  * Vector class for double N-dimensional vectors.
  *
  * @author Caellian
  */
class VectorD(val data: Array[Double]) extends Vector[Double] {

  val size = data.length

  /**
    * @return this vector.
    */
  def unary_+ = {
    this
  }

  /**
    * @return new vector with all data negated.
    */
  def unary_- = {
    new VectorD(asArray.map(-_))
  }

  /**
    * Adds two vectors together and returns resulting vector.
    * In order to add to matrices together, they must be of same size.
    *
    * @param other vector to add to this one.
    * @return result of vector addition.
    */
  def +(other: VectorD): VectorD = {
    require(size == other.size, "Invalid argument vector size!")
    new VectorD(data zip other.data map Function.tupled(_ + _))
  }

  /**
    * Subtracts other vector from this one and returns resulting vector.
    * In order to subtract one vector from another, both vectors must be of same size.
    *
    * @param other vector to subtract from this one.
    * @return result of vector subtraction.
    */
  def -(other: VectorD): VectorD = {
    require(size == other.size, "Invalid argument vector size!")
    new VectorD(data zip other.data map Function.tupled(_ - _))
  }

  /**
    * Multiplies two vectors together and returns resulting vector.
    * In order to add to multiply vectors together, they must be of same size.
    *
    * @param other vector to multiply with this one.
    * @return result of vector multiplication.
    */
  def *(other: VectorD): VectorD = {
    require(size == other.size, "Invalid argument vector size!")
    new VectorD(data zip other.data map Function.tupled(_ * _))
  }

  /**
    * Divides this vector with other and returns resulting vector.
    * In order to divide one vector from another, both vectors must be of same size.
    *
    * @param other vector dividend.
    * @return result of vector division.
    */
  def /(other: VectorD): VectorD = {
    require(size == other.size, "Invalid argument vector size!")
    new VectorD(data zip other.data map Function.tupled(_ / _))
  }

  /**
    * Performs scalar addition on this vector and returns resulting vector.
    *
    * @param value scalar value.
    * @return result of scalar vector addition.
    */
  def +(value: Double): VectorD = {
    new VectorD(asArray.map(_ + value))
  }

  /**
    * Performs scalar subtraction on this vector and returns resulting vector.
    *
    * @param value scalar value.
    * @return result of scalar vector subtraction.
    */
  def -(value: Double): VectorD = {
    new VectorD(asArray.map(_ - value))
  }

  /**
    * Performs scalar multiplication on this vector and returns resulting vector.
    *
    * @param value scalar value.
    * @return result of scalar vector multiplication.
    */
  def *(value: Double): VectorD = {
    new VectorD(asArray.map(_ * value))
  }

  /**
    * Performs scalar division on this vector and returns resulting vector.
    *
    * @param value scalar value.
    * @return result of scalar vector division.
    */
  def /(value: Double): VectorD = {
    new VectorD(asArray.map(_ / value))
  }

  /**
    * @param o other vector or object instance of type extending vector.
    * @return true if this vector is equal to other vector.
    */
  def ==(o: VectorD): Boolean = {
    equals(o)
  }

  /**
    * @return max value of member of this vector.
    */
  def max: Double = {
    data.toList.max
  }

  /**
    * @return new vector containing absolute values of this vector.
    */
  def abs: VectorD = {
    new VectorD(asArray.map(FastMath.abs))
  }

  /**
    * @return new vector with normalized values of this one.
    */
  def normalized: VectorD = {
    this / this.magnitude
  }

  /**
    * @return magnitude of this vector.
    */
  def magnitude: Double = {
    distanceTo(this)
  }

  /**
    * Calculates distance from this to other vector.
    *
    * @param other vector to calculate distance to.
    * @return distance between this and other vector.
    */
  def distanceTo(other: VectorD): Double = {
    FastMath.sqrt(this dot other).toDouble
  }

  /**
    *
    * @param other other vector used to determine dot product.
    * @return dot product of two vectors
    */
  def dot(other: VectorD): Double = {
    require(size == other.size, "Vectors must be of same size.")
    (data zip other.data map Function.tupled(_ * _)).sum
  }

  /**
    * Returns cross product of this and other vector.
    *
    * @param other other vector used to determine cross product.
    * @return cross product.
    */
  def cross(other: VectorD): VectorD = {
    require(size == other.size, "Vectors must be of same size.")
    // Some cross product improvisations do exist for 2D space, but they are mathematically incorrect.
    require(size == 7 || size == 3, s"Cross product does not exist in $size-dimensional space!")

    val result = Array.ofDim[Double](this.size)
    this.size match {
      case 7 => {
        result(X) = this(Y) * other(W) - this(W) * other(Y) + this(Z) * other(6) - this(6) * other(Z) + this(4) * other(5) - this(5) * other(4)
        result(Y) = this(Z) * other(4) - this(4) * other(Z) + this(W) * other(X) - this(X) * other(W) + this(5) * other(6) - this(6) * other(5)
        result(Z) = this(W) * other(5) - this(5) * other(W) + this(4) * other(Y) - this(Y) * other(4) + this(6) * other(X) - this(X) * other(6)
        result(W) = this(4) * other(6) - this(6) * other(4) + this(5) * other(Z) - this(Z) * other(5) + this(X) * other(Y) - this(Y) * other(X)
        result(4) = this(5) * other(X) - this(X) * other(5) + this(6) * other(W) - this(W) * other(6) + this(Y) * other(Z) - this(Z) * other(Y)
        result(5) = this(6) * other(Y) - this(Y) * other(6) + this(X) * other(4) - this(4) * other(X) + this(Z) * other(W) - this(W) * other(Z)
        result(6) = this(X) * other(Z) - this(Z) * other(X) + this(Y) * other(5) - this(5) * other(Y) + this(W) * other(4) - this(4) * other(W)
      }
      case 3 => {
        result(X) = this(Y) * other(Z) - this(Z) * other(Y)
        result(Y) = this(Z) * other(X) - this(X) * other(Z)
        result(Z) = this(X) * other(Y) - this(Y) * other(X)
      }
    }
    new VectorD(result)
  }

  /**
    * Rotates this vector using rotation matrix.
    *
    * @param rotationMatrix rotation matrix to use.
    * @return rotated vector.
    */
  def rotated(rotationMatrix: MatrixD): VectorD = {
    (rotationMatrix * verticalMatrix).toVector
  }

  /**
    * Linearly interpolates between two vectors.
    *
    * @param destination vector with destination coordinates.
    * @param percent     percentage of
    * @return linear interpolation.
    */
  def lerp(destination: VectorD, percent: Double): VectorD = {
    this + (destination - this) * percent
  }

  /**
    * Vertical matrix containing data of this vector.
    */
  val verticalMatrix = MatrixD(true, asArray)

  /**
    * Horizontal matrix containing data of this vector.
    */
  val horizontalMatrix = MatrixD(false, asArray)

  /**
    * @return buffer containing data of this vector.
    */
  def asBuffer: Buffer = {
    ByteBuffer.allocateDirect(data.length << 2).order(ByteOrder.nativeOrder).asDoubleBuffer().put(data).flip
  }

  /**
    * @return clone of this vector.
    */
  override def replicate(): VectorD = {
    new VectorD(asArray)
  }

  /**
    * Vector hashcode depends on vector data and will change is vector data is modified!
    *
    * @return hashcode of this vector.
    */
  override def hashCode: Int = {
    Data.hashCode(data)
  }

  /**
    * @param o other vector or object instance of type extending vector.
    * @return true if this vector is equal to other vector.
    */
  override def equals(o: Any): Boolean = {
    if (this == o) {
      return true
    }
    if (!o.isInstanceOf[VectorD]) {
      return false
    }
    val vector = o.asInstanceOf[VectorD]
    data sameElements vector.data
  }
}

/**
  * Object used to create {@link VectorD} vectors and ensure their consistency.
  * Use initializer functions from this object only if there is no alternative ones in more specific objects as some
  * of these are very CPU intensive.
  *
  * @author Caellian
  */
object VectorD {
  type VecF = VectorD

  /**
    * Creates a new vector using given values.
    *
    * @param values values to create a new vector from.
    * @return created vector.
    */
  def apply(values: Double*): VectorD = {
    new VectorD(values.toArray[Double])
  }

  /**
    * Creates a new vector using array values.
    *
    * @param values value array to create a new vector from.
    * @return created vector.
    */
  def apply(values: Array[Double]): VectorD = {
    new VectorD(values)
  }

  /**
    * Creates a new vector using buffer values.
    *
    * @param buffer buffer to create a new vector from.
    * @return created vector.
    */
  def apply(buffer: DoubleBuffer): VectorD = {
    new VectorD((0 until buffer.capacity()).map(buffer.get).toArray)
  }
}

/**
  * Object containing utility functions for quaternion initialization.
  *
  * @author Caellian
  */
object QuaternionD {
  /**
    * Creates a new quaternion (4D vector) using {@link VectorD} class from
    * given rotation matrix.
    *
    * @param rotationMatrix rotation matrix to use for quaternion creation.
    * @return resulting quaternion.
    */
  def fromRotationMatrix(rotationMatrix: MatrixD): VectorD = {
    assert(rotationMatrix.matrix.length == 4 && rotationMatrix.matrix.forall(_.length == 4), "")
    var x = 0d
    var y = 0d
    var z = 0d
    var w = 0d
    val trace: Double = rotationMatrix.get(0, 0) + rotationMatrix.get(1, 1) + rotationMatrix.get(2, 2)

    if (trace > 0) {
      val s: Double = 0.5f / FastMath.sqrt(trace + 1.0f)
      x = 0.25f / s
      y = (rotationMatrix.get(1, 2) - rotationMatrix.get(2, 1)) * s
      z = (rotationMatrix.get(2, 0) - rotationMatrix.get(0, 2)) * s
      w = (rotationMatrix.get(0, 1) - rotationMatrix.get(1, 0)) * s
    }
    else {
      if (rotationMatrix.get(0, 0) > rotationMatrix.get(1, 1) && rotationMatrix.get(0, 0) > rotationMatrix.get(2, 2)) {
        val s: Double = 2.0f * FastMath.sqrt(1.0f + rotationMatrix.get(0, 0) - rotationMatrix.get(1, 1) - rotationMatrix.get(2, 2))
        x = (rotationMatrix.get(1, 2) - rotationMatrix.get(2, 1)) / s
        y = 0.25f * s
        z = (rotationMatrix.get(1, 0) + rotationMatrix.get(0, 1)) / s
        w = (rotationMatrix.get(2, 0) + rotationMatrix.get(0, 2)) / s
      }
      else if (rotationMatrix.get(1, 1) > rotationMatrix.get(2, 2)) {
        val s: Double = 2.0f * FastMath.sqrt(1.0f + rotationMatrix.get(1, 1) - rotationMatrix.get(0, 0) - rotationMatrix.get(2, 2))
        x = (rotationMatrix.get(2, 0) - rotationMatrix.get(0, 2)) / s
        y = (rotationMatrix.get(1, 0) + rotationMatrix.get(0, 1)) / s
        z = 0.25f * s
        w = (rotationMatrix.get(2, 1) + rotationMatrix.get(1, 2)) / s
      }
      else {
        val s: Double = 2.0f * FastMath.sqrt(1.0f + rotationMatrix.get(2, 2) - rotationMatrix.get(0, 0) - rotationMatrix.get(1, 1))
        x = (rotationMatrix.get(0, 1) - rotationMatrix.get(1, 0)) / s
        y = (rotationMatrix.get(2, 0) + rotationMatrix.get(0, 2)) / s
        z = (rotationMatrix.get(1, 2) + rotationMatrix.get(2, 1)) / s
        w = 0.25f * s
      }
    }

    VectorD(x, y, z, w).normalized
  }

  def initRotationQuaternion(axis: VectorD, angle: Double): VectorD = {
    val sinHalfAngle = Math.sin(angle / 2)
    VectorD(axis(X) * sinHalfAngle, axis(Y) * sinHalfAngle, axis(Z) * sinHalfAngle, Math.cos(angle / 2))
  }
}
