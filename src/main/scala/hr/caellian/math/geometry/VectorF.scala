/*
 * CaellianMath, math library
 * Copyright (C) 2016 Caellian
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package hr.caellian.math.geometry

import java.nio._

import hr.caellian.math.util.DataUtil
import org.apache.commons.math3.util.FastMath

/**
  * Protected vector class for {@link Float} N-dimensional vectors.
  *
  * @author Caellian
  */
protected class VectorF(val data: Array[Float]) extends Vector[Float] {

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
		new VectorF(asArray.map(-_))
	}

	/**
	  * Adds two vectors together and returns resulting vector.
	  * In order to add to matrices together, they must be of same size.
	  *
	  * @param other vector to add to this one.
	  *
	  * @return result of vector addition.
	  */
	def +(other: VectorF): VectorF = {
		require(size == other.size, "Invalid argument vector size!")
		new VectorF(data zip other.data map Function.tupled(_ + _))
	}

	/**
	  * Subtracts other vector from this one and returns resulting vector.
	  * In order to subtract one vector from another, both vectors must be of same size.
	  *
	  * @param other vector to subtract from this one.
	  *
	  * @return result of vector subtraction.
	  */
	def -(other: VectorF): VectorF = {
		require(size == other.size, "Invalid argument vector size!")
		new VectorF(data zip other.data map Function.tupled(_ - _))
	}

	/**
	  * Multiplies two vectors together and returns resulting vector.
	  * In order to add to multiply vectors together, they must be of same size.
	  *
	  * @param other vector to multiply with this one.
	  *
	  * @return result of vector multiplication.
	  */
	def *(other: VectorF): VectorF = {
		require(size == other.size, "Invalid argument vector size!")
		new VectorF(data zip other.data map Function.tupled(_ * _))
	}

	/**
	  * Divides this vector with other and returns resulting vector.
	  * In order to divide one vector from another, both vectors must be of same size.
	  *
	  * @param other vector dividend.
	  *
	  * @return result of vector division.
	  */
	def /(other: VectorF): VectorF = {
		require(size == other.size, "Invalid argument vector size!")
		new VectorF(data zip other.data map Function.tupled(_ / _))
	}

	/**
	  * Performs scalar addition on this vector and returns resulting vector.
	  *
	  * @param value scalar value.
	  *
	  * @return result of scalar vector addition.
	  */
	def +(value: Float): VectorF = {
		new VectorF(asArray.map(_ + value))
	}

	/**
	  * Performs scalar subtraction on this vector and returns resulting vector.
	  *
	  * @param value scalar value.
	  *
	  * @return result of scalar vector subtraction.
	  */
	def -(value: Float): VectorF = {
		new VectorF(asArray.map(_ - value))
	}

	/**
	  * Performs scalar multiplication on this vector and returns resulting vector.
	  *
	  * @param value scalar value.
	  *
	  * @return result of scalar vector multiplication.
	  */
	def *(value: Float): VectorF = {
		new VectorF(asArray.map(_ * value))
	}

	/**
	  * Performs scalar division on this vector and returns resulting vector.
	  *
	  * @param value scalar value.
	  *
	  * @return result of scalar vector division.
	  */
	def /(value: Float): VectorF = {
		new VectorF(asArray.map(_ / value))
	}

	/**
	  * @param o other vector or object instance of type extending vector.
	  *
	  * @return true if this vector is equal to other vector.
	  */
	def ==(o: VectorF): Boolean = {
		equals(o)
	}

	/**
	  * @return max value of member of this vector.
	  */
	def max: Float = {
		data.toList.max
	}

	/**
	  * @return new vector containing absolute values of this vector.
	  */
	def abs: VectorF = {
		new VectorF(asArray.map(FastMath.abs))
	}

	/**
	  * @return new vector with normalized values of this one.
	  */
	def normalized: VectorF = {
		this / this.magnitude
	}

	/**
	  * @return magnitude of this vector.
	  */
	def magnitude: Float = {
		distanceTo(this)
	}

	/**
	  * Calculates distance from this to other vector.
	  *
	  * @param other vector to calculate distance to.
	  * @return distance between this and other vector.
	  */
	def distanceTo(other: VectorF): Float = {
		FastMath.sqrt(this dot other).toFloat
	}

	/**
	  *
	  * @param other other vector used to determine dot product.
	  *
	  * @return dot product of two vectors
	  */
	def dot(other: VectorF): Float = {
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
	def cross(other: VectorF): VectorF = {
		require(size == other.size, "Vectors must be of same size.")
		// Some cross product improvisations do exist for 2D space, but they are mathematically incorrect.
		require(size == 7 || size == 3, s"Cross product does not exist in $size-dimensional space!")

		val result = Array.ofDim[Float](this.size)
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
		new VectorF(result)
	}

	/**
	  * Rotates this vector using rotation matrix.
	  *
	  * @param rotationMatrix rotation matrix to use.
	  * @return rotated vector.
	  */
	def rotated(rotationMatrix: MatrixF): VectorF = {
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
	def lerp(destination: VectorF, percent: Float): VectorF = {
		this + (destination - this) * percent
	}

	/**
	  * Vertical matrix containing data of this vector.
	  */
	val verticalMatrix = MatrixF(true, asArray)

	/**
	  * Horizontal matrix containing data of this vector.
	  */
	val horizontalMatrix = MatrixF(false, asArray)

	/**
	  * @return buffer containing data of this vector.
	  */
	def asBuffer: Buffer = {
		ByteBuffer.allocateDirect(data.length << 2).order(ByteOrder.nativeOrder).asFloatBuffer().put(data).flip
	}

	/**
	  * @return clone of this vector.
	  */
	override def replicate(): VectorF = {
		new VectorF(asArray)
	}

	/**
	  * Vector hashcode depends on vector data and will change is vector data is modified!
	  *
	  * @return hashcode of this vector.
	  */
	override def hashCode: Int = {
		DataUtil.hashCode(data)
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
		if (!o.isInstanceOf[VectorF]) {
			return false
		}
		val vector = o.asInstanceOf[VectorF]
		data sameElements vector.data
	}
}

/**
  * Object used to create {@link hr.caellian.math.geometry.VectorF VectorF} vectors and ensure their consistency.
  * Use initializer functions from this object only if there is no alternative ones in more specific objects as some
  * of these are very CPU intensive.
  *
  * @author Caellian
  */
object VectorF {
	/**
	  * Creates a new vector using given values.
	  *
	  * @param values values to create a new vector from.
	  *
	  * @return created vector.
	  */
	def apply(values: Float*): VectorF = {
		new VectorF(values.toArray[Float])
	}

	/**
	  * Creates a new vector using array values.
	  *
	  * @param values value array to create a new vector from.
	  *
	  * @return created vector.
	  */
	def apply(values: Array[Float]): VectorF = {
		new VectorF(values)
	}

	/**
	  * Creates a new vector using buffer values.
	  *
	  * @param buffer buffer to create a new vector from.
	  *
	  * @return created vector.
	  */
	def apply(buffer: FloatBuffer): VectorF = {
		new VectorF((0 until buffer.capacity()).map(buffer.get).toArray)
	}
}