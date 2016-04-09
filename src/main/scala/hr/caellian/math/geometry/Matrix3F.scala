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

import org.apache.commons.math3.util.FastMath

/**
  * Utility object containing initializers for basic 3x3 matrices.
  * These functions should be used instead of any provided by {@link hr.caellian.math.geometry.MatrixF MatrixF}
  * wherever possible as they are supposed to perform faster.
  *
  * @author Caellian
  */
object Matrix3F {
	/**
	  * Initializes rotation matrix using forward, up and right vector.
	  *
	  * @param forward forward vector
	  * @param up      up vector
	  * @param right   right vector
	  *
	  * @return rotation matrix
	  */
	def initRotationMatrix(forward: VectorF, up: VectorF, right: VectorF): MatrixF = {
		new MatrixF(Array(right.asArray, up.asArray, forward.asArray))
	}

	/**
	  * Turns this matrix into a rotation matrix.
	  *
	  * @param a     first argument.
	  * @param b     second argument.
	  * @param angle degrees to rotate in objects multiplied by this rotation matrix.
	  *
	  * @return plane rotation matrix.
	  */
	def initPlaneRotation(a: Int, b: Int, angle: Float): MatrixF = {
		val ai = a - 1
		val bi = b - 1
		val matrix = Array.ofDim[Float](4,4)
		for (x <- matrix.indices) {
			for (y <- matrix(0).indices) {
				val value = (y, x) match {
					case (`ai`, `ai`) => FastMath.cos(FastMath.toRadians(angle)).toFloat
					case (`bi`, `bi`) => FastMath.cos(FastMath.toRadians(angle)).toFloat
					case (`ai`, `bi`) => -FastMath.sin(FastMath.toRadians(angle)).toFloat
					case (`bi`, `ai`) => FastMath.sin(FastMath.toRadians(angle)).toFloat
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
		MatrixF(matrix)
	}

	/**
	  * Initializes rotation matrix using degree rotation in x, y & z direction.
	  *
	  * @param x degree rotation in x direction
	  * @param y degree rotation in y direction
	  * @param z degree rotation in z direction
	  *
	  * @return rotation matrix
	  */
	def initRotationMatrix(x: Float, y: Float, z: Float): MatrixF = {
		val rx = Matrix3F.initPlaneRotation(2, 3, x)
		val ry = Matrix3F.initPlaneRotation(3, 1, y)
		val rz = Matrix3F.initPlaneRotation(1, 2, z)
		MatrixF((rz * ry * rx).asArray)
	}

	/**
	  * Initializes a new 3x3 scaling matrix.
	  *
	  * @param scale scale.
	  *
	  * @return scale matrix.
	  */
	def initScalingMatrix(scale: Array[Float]): MatrixF = {
		assert(scale.length == 3, "Translation must have 3 values!")
		new MatrixF(Array.tabulate[Float](3, 3)((x, y) => {
			if (x == y) {
				scale(x)
			} else {
				0
			}
		}))
	}

	/**
	  * Initializes a new 3x3 translation matrix.
	  *
	  * @param location relative location.
	  *
	  * @return translation matrix.
	  */
	def initTranslationMatrix(location: Array[Float]): MatrixF = {
		assert(location.length == 2, "Translation must have 2 coordinates!")
		new MatrixF(Array.tabulate[Float](3, 3)((x,
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
	  * Initializes a new 3x3 identity matrix.
	  *
	  * @return identity matrix.
	  */
	def initIdentityMatrix(): MatrixF = {
		new MatrixF(Array.tabulate[Float](3, 3)((x, y) => {
			if (x == y) {
				1
			} else {
				0
			}
		}))
	}
}
