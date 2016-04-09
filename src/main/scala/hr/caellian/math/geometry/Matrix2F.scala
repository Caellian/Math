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
  * Utility object containing initializers for basic 2x2 matrices.
  * These functions should be used instead of any provided by {@link hr.caellian.math.geometry.MatrixF MatrixF}
  * wherever possible as they are supposed to perform faster.
  *
  * @author Caellian
  */
object Matrix2F {
	/**
	  * Initializes rotation matrix using degrees.
	  *
	  * @param degrees Degrees to rotate objects multiplied by this matrix in positive direction.
	  *
	  * @return rotation matrix
	  */
	def initRotation(degrees: Float): MatrixF = {
		if (degrees == 0) {
			return initIdentityMatrix()
		}

		val result = Array.ofDim[Float](2, 2)
		result(0)(0) = FastMath.cos(FastMath.toRadians(degrees)).toFloat
		result(0)(1) = FastMath.sin(FastMath.toRadians(degrees)).toFloat
		result(1)(0) = -FastMath.sin(FastMath.toRadians(degrees)).toFloat
		result(1)(1) = FastMath.cos(FastMath.toRadians(degrees)).toFloat

		new MatrixF(result)
	}

	/**
	  * Initializes a new 2x2 identity matrix.
	  *
	  * @return identity matrix.
	  */
	def initIdentityMatrix(): MatrixF = {
		new MatrixF(Array.tabulate[Float](2, 2)((x, y) => {
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
	  *
	  * @return scale matrix.
	  */
	def initScalingMatrix(scale: Array[Float]): MatrixF = {
		assert(scale.length == 2, "Translation must have 2 values!")
		new MatrixF(Array.tabulate[Float](2, 2)((x, y) => {
			if (x == y) {
				scale(x)
			} else {
				0
			}
		}))
	}
}