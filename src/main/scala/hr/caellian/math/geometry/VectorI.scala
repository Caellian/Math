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

import java.nio.{Buffer, ByteBuffer, ByteOrder, IntBuffer}

import hr.caellian.math.util.Data
import org.apache.commons.math3.util.FastMath

/**
  * Protected vector class for {@link Int} N-dimensional vectors.
  *
  * @author Caellian
  */
class VectorI(val data: Array[Int]) extends Vector[Int] {

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
    new VectorI(asArray.map(-_))
  }

  /**
    * Adds two vectors together and returns resulting vector.
    * In order to add to matrices together, they must be of same size.
    *
    * @param other vector to add to this one.
    *
    * @return result of vector addition.
    */
  def +(other: VectorI): VectorI = {
    require(size == other.size, "Invalid argument vector size!")
    new VectorI(data zip other.data map Function.tupled(_ + _))
  }

  /**
    * Subtracts other vector from this one and returns resulting vector.
    * In order to subtract one vector from another, both vectors must be of same size.
    *
    * @param other vector to subtract from this one.
    *
    * @return result of vector subtraction.
    */
  def -(other: VectorI): VectorI = {
    require(size == other.size, "Invalid argument vector size!")
    new VectorI(data zip other.data map Function.tupled(_ - _))
  }

  /**
    * Multiplies two vectors together and returns resulting vector.
    * In order to add to multiply vectors together, they must be of same size.
    *
    * @param other vector to multiply with this one.
    *
    * @return result of vector multiplication.
    */
  def *(other: VectorI): VectorI = {
    require(size == other.size, "Invalid argument vector size!")
    new VectorI(data zip other.data map Function.tupled(_ * _))
  }

  /**
    * Divides this vector with other and returns resulting vector.
    * In order to divide one vector from another, both vectors must be of same size.
    *
    * @param other vector dividend.
    *
    * @return result of vector division.
    */
  def /(other: VectorI): VectorI = {
    require(size == other.size, "Invalid argument vector size!")
    new VectorI(data zip other.data map Function.tupled(_ / _))
  }

  /**
    * Performs scalar addition on this vector and returns resulting vector.
    *
    * @param value scalar value.
    *
    * @return result of scalar vector addition.
    */
  def +(value: Int): VectorI = {
    new VectorI(asArray.map(_ + value))
  }

  /**
    * Performs scalar subtraction on this vector and returns resulting vector.
    *
    * @param value scalar value.
    *
    * @return result of scalar vector subtraction.
    */
  def -(value: Int): VectorI = {
    new VectorI(asArray.map(_ - value))
  }

  /**
    * Performs scalar multiplication on this vector and returns resulting vector.
    *
    * @param value scalar value.
    *
    * @return result of scalar vector multiplication.
    */
  def *(value: Int): VectorI = {
    new VectorI(asArray.map(_ * value))
  }

  /**
    * Performs scalar division on this vector and returns resulting vector.
    *
    * @param value scalar value.
    *
    * @return result of scalar vector division.
    */
  def /(value: Int): VectorI = {
    new VectorI(asArray.map(_ / value))
  }

  /**
    * @param o other vector or object instance of type extending vector.
    *
    * @return true if this vector is equal to other vector.
    */
  def ==(o: VectorI): Boolean = {
    equals(o)
  }

  /**
    * @return max value of member of this vector.
    */
  def max: Int = {
    data.toList.max
  }

  /**
    * @return new vector containing absolute values of this vector.
    */
  def abs: VectorI = {
    new VectorI(asArray.map(FastMath.abs))
  }

  /**
    * @return new vector with normalized values of this one.
    */
  def normalized: VectorI = {
    this / this.magnitude
  }

  /**
    * @return magnitude of this vector.
    */
  def magnitude: Int = {
    distanceTo(this)
  }

  /**
    * Calculates distance from this to other vector.
    *
    * @param other vector to calculate distance to.
    * @return distance between this and other vector.
    */
  def distanceTo(other: VectorI): Int = {
    FastMath.sqrt(this dot other).toInt
  }

  /**
    *
    * @param other other vector used to determine dot product.
    *
    * @return dot product of two vectors
    */
  def dot(other: VectorI): Int = {
    require(size == other.size, "Vectors must be of same size.")
    (data zip other.data map Function.tupled(_ * _)).sum
  }

  /**
    * Returns cross product of this and other vector.
    *
    * @param other other vector used to determine cross product.
    *
    * @return cross product.
    */
  def cross(other: VectorI): VectorI = {
    require(size == other.size, "Vectors must be of same size.")
    // Some cross product improvisations do exist for 2D space, but they are mathematically incorrect.
    require(size == 7 || size == 3, s"Cross product does not exist in $size-dimensional space!")

    val result = Array.ofDim[Int](this.size)
    this.size match {
      case 7 => {
        result(0) = this.data(1) * other.data(3) - this.data(3) * other.data(1) + this.data(2) * other.data(6) - this.data(6) * other.data(2) + this.data(4) * other.data(5) - this.data(5) * other.data(4)
        result(1) = this.data(2) * other.data(4) - this.data(4) * other.data(2) + this.data(3) * other.data(0) - this.data(0) * other.data(3) + this.data(5) * other.data(6) - this.data(6) * other.data(5)
        result(2) = this.data(3) * other.data(5) - this.data(5) * other.data(3) + this.data(4) * other.data(1) - this.data(1) * other.data(4) + this.data(6) * other.data(0) - this.data(0) * other.data(6)
        result(3) = this.data(4) * other.data(6) - this.data(6) * other.data(4) + this.data(5) * other.data(2) - this.data(2) * other.data(5) + this.data(0) * other.data(1) - this.data(1) * other.data(0)
        result(4) = this.data(5) * other.data(0) - this.data(0) * other.data(5) + this.data(6) * other.data(3) - this.data(3) * other.data(6) + this.data(1) * other.data(2) - this.data(2) * other.data(1)
        result(5) = this.data(6) * other.data(1) - this.data(1) * other.data(6) + this.data(0) * other.data(4) - this.data(4) * other.data(0) + this.data(2) * other.data(3) - this.data(3) * other.data(2)
        result(6) = this.data(0) * other.data(2) - this.data(2) * other.data(0) + this.data(1) * other.data(5) - this.data(5) * other.data(1) + this.data(3) * other.data(4) - this.data(4) * other.data(3)
      }
      case 3 => {
        result(0) = this.data(1) * other.data(2) - this.data(2) * other.data(1)
        result(1) = this.data(2) * other.data(0) - this.data(0) * other.data(2)
        result(2) = this.data(0) * other.data(1) - this.data(1) * other.data(0)
      }
    }
    new VectorI(result)
  }

  /**
    * Rotates this vector using rotation matrix.
    *
    * @param rotationMatrix rotation matrix to use.
    * @return rotated vector.
    */
  def rotated(rotationMatrix: MatrixI): VectorI = {
    (rotationMatrix * verticalMatrix).toVector
  }

  /**
    * Linearly interpolates between two vectors.
    *
    * @param destination vector with destination coordinates.
    * @param percent percentage of
    *
    * @return linear interpolation.
    */
  def lerp(destination: VectorI, percent: Int): VectorI = {
    this + (destination - this) * percent
  }

  /**
    * Vertical matrix containing data of this vector.
    */
  val verticalMatrix = MatrixI(true, asArray)

  /**
    * Horizontal matrix containing data of this vector.
    */
  val horizontalMatrix = MatrixI(false, asArray)

  /**
    * @return buffer containing data of this vector.
    */
  def asBuffer: Buffer = {
    ByteBuffer.allocateDirect(data.length << 2).order(ByteOrder.nativeOrder).asIntBuffer().put(data).flip
  }

  /**
    * @return clone of this vector.
    */
  override def replicate(): VectorI = {
    new VectorI(asArray)
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
    *
    * @return true if this vector is equal to other vector.
    */
  override def equals(o: Any): Boolean = {
    if (this == o) {
      return true
    }
    if (!o.isInstanceOf[VectorI]) {
      return false
    }
    val vector = o.asInstanceOf[VectorI]
    data sameElements vector.data
  }
}

/**
  * Object used to create {@link VectorI} vectors and ensure their consistency.
  * Use initializer functions from this object only if there is no alternative ones in more specific objects as some
  * of these are very CPU intensive.
  *
  * @author Caellian
  */
object VectorI {
  type VecF = VectorI

  /**
    * Creates a new vector using given values.
    *
    * @param values values to create a new vector from.
    *
    * @return created vector.
    */
  def apply(values: Int*): VectorI = {
    new VectorI(values.toArray[Int])
  }

  /**
    * Creates a new vector using array values.
    *
    * @param values value array to create a new vector from.
    *
    * @return created vector.
    */
  def apply(values: Array[Int]): VectorI = {
    new VectorI(values)
  }

  /**
    * Creates a new vector using buffer values.
    *
    * @param buffer buffer to create a new vector from.
    *
    * @return created vector.
    */
  def apply(buffer: IntBuffer): VectorI = {
    new VectorI((0 until buffer.capacity()).map(buffer.get).toArray)
  }
}

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
    assert(rotationMatrix.matrix.length == 4 && rotationMatrix.matrix.forall(_.length == 4), "")
    var x = 0
    var y = 0
    var z = 0
    var w = 0
    val trace: Int = rotationMatrix.get(0, 0) + rotationMatrix.get(1, 1) + rotationMatrix.get(2, 2)

    if (trace > 0) {
      val s: Int = (0.5f / FastMath.sqrt(trace + 1.0f)).toInt
      x = (0.25f / s).toInt
      y = (rotationMatrix.get(1, 2) - rotationMatrix.get(2, 1)) * s
      z = (rotationMatrix.get(2, 0) - rotationMatrix.get(0, 2)) * s
      w = (rotationMatrix.get(0, 1) - rotationMatrix.get(1, 0)) * s
    }
    else {
      if (rotationMatrix.get(0, 0) > rotationMatrix.get(1, 1) && rotationMatrix.get(0, 0) > rotationMatrix.get(2, 2)) {
        val s: Int = (2.0f * FastMath.sqrt(1.0f + rotationMatrix.get(0, 0) - rotationMatrix.get(1, 1) - rotationMatrix.get(2, 2))).toInt
        x = (rotationMatrix.get(1, 2) - rotationMatrix.get(2, 1)) / s
        y = (0.25f * s).toInt
        z = (rotationMatrix.get(1, 0) + rotationMatrix.get(0, 1)) / s
        w = (rotationMatrix.get(2, 0) + rotationMatrix.get(0, 2)) / s
      }
      else if (rotationMatrix.get(1, 1) > rotationMatrix.get(2, 2)) {
        val s: Int = (2.0f * FastMath.sqrt(1.0f + rotationMatrix.get(1, 1) - rotationMatrix.get(0, 0) - rotationMatrix.get(2, 2))).toInt
        x = (rotationMatrix.get(2, 0) - rotationMatrix.get(0, 2)) / s
        y = (rotationMatrix.get(1, 0) + rotationMatrix.get(0, 1)) / s
        z = (0.25f * s).toInt
        w = (rotationMatrix.get(2, 1) + rotationMatrix.get(1, 2)) / s
      }
      else {
        val s: Int = (2.0f * FastMath.sqrt(1.0f + rotationMatrix.get(2, 2) - rotationMatrix.get(0, 0) - rotationMatrix.get(1, 1))).toInt
        x = (rotationMatrix.get(0, 1) - rotationMatrix.get(1, 0)) / s
        y = (rotationMatrix.get(2, 0) + rotationMatrix.get(0, 2)) / s
        z = (rotationMatrix.get(1, 2) + rotationMatrix.get(2, 1)) / s
        w = (0.25f * s).toInt
      }
    }

    VectorI(x, y, z, w).normalized
  }
}
