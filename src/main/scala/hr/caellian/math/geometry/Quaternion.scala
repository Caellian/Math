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
  * Object containing utility functions for quaternion initialization.
  *
  * @author Caellian
  */
object Quaternion {
	/**
	  * Creates a new quaternion (4D vector) using {@link hr.caellian.math.geometry.VectorF VectorF} class from
	  * given rotation matrix.
	  *
	  * @param rotationMatrix rotation matrix to use for quaternion creation.
	  * @return resulting quaternion.
	  */
	def fromRotationMatrix(rotationMatrix: MatrixF): VectorF = {
		assert(rotationMatrix.matrix.length == 4 && rotationMatrix.matrix.forall(_.length == 4), "")
		var x = 0f
		var y = 0f
		var z = 0f
		var w = 0f
		val trace: Float = rotationMatrix.get(0, 0) + rotationMatrix.get(1, 1) + rotationMatrix.get(2, 2)

		if (trace > 0) {
			val s: Float = 0.5f / FastMath.sqrt(trace + 1.0f).toFloat
			x = 0.25f / s
			y = (rotationMatrix.get(1, 2) - rotationMatrix.get(2, 1)) * s
			z = (rotationMatrix.get(2, 0) - rotationMatrix.get(0, 2)) * s
			w = (rotationMatrix.get(0, 1) - rotationMatrix.get(1, 0)) * s
		}
		else {
			if (rotationMatrix.get(0, 0) > rotationMatrix.get(1, 1) && rotationMatrix.get(0, 0) > rotationMatrix.get(2, 2)) {
				val s: Float = 2.0f * FastMath.sqrt(1.0f + rotationMatrix.get(0, 0) - rotationMatrix.get(1, 1) - rotationMatrix.get(2, 2)).toFloat
				x = (rotationMatrix.get(1, 2) - rotationMatrix.get(2, 1)) / s
				y = 0.25f * s
				z = (rotationMatrix.get(1, 0) + rotationMatrix.get(0, 1)) / s
				w = (rotationMatrix.get(2, 0) + rotationMatrix.get(0, 2)) / s
			}
			else if (rotationMatrix.get(1, 1) > rotationMatrix.get(2, 2)) {
				val s: Float = 2.0f * FastMath.sqrt(1.0f + rotationMatrix.get(1, 1) - rotationMatrix.get(0, 0) - rotationMatrix.get(2, 2)).toFloat
				x = (rotationMatrix.get(2, 0) - rotationMatrix.get(0, 2)) / s
				y = (rotationMatrix.get(1, 0) + rotationMatrix.get(0, 1)) / s
				z = 0.25f * s
				w = (rotationMatrix.get(2, 1) + rotationMatrix.get(1, 2)) / s
			}
			else {
				val s: Float = 2.0f * FastMath.sqrt(1.0f + rotationMatrix.get(2, 2) - rotationMatrix.get(0, 0) - rotationMatrix.get(1, 1)).toFloat
				x = (rotationMatrix.get(0, 1) - rotationMatrix.get(1, 0)) / s
				y = (rotationMatrix.get(2, 0) + rotationMatrix.get(0, 2)) / s
				z = (rotationMatrix.get(1, 2) + rotationMatrix.get(2, 1)) / s
				w = 0.25f * s
			}
		}

		VectorF(x, y, z, w).normalized
	}
}
