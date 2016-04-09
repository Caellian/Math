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

import java.nio.Buffer

import hr.caellian.math.util.Replicable


/**
  * Generic Matrix class created for stable code infrastructure and consistency.
  * Most basic Matrix variables and functions can be found here.
  *
  * @author Caellian
  */
abstract class Matrix[T <: AnyVal] extends Replicable[Matrix[T]] {

	/**
	  * Matrix data.
	  */
	var matrix: Array[Array[T]]

	/**
	  * Number of rows in this matrix.
	  */
	val rowCount: Int
	/**
	  * Number of columns in this matrix.
	  */
	val columnCount: Int

	/**
	  * @param column outer array index.
	  * @param row inner array index.
	  * @return data stored at given column and row.
	  */
	def get(column: Int, row: Int): T = {
		matrix(column)(row)
	}

	/**
	  * @return 2D array containing data of this matrix.
	  */
	def asArray: Array[Array[T]] = {
		matrix.clone()
	}
	/**
	  * @return buffer containing data of this matrix.
	  */
	def asBuffer: Buffer

	/**
	  * @return string representation of this matrix.
	  */
	override def toString: String = {
		val builder = new StringBuilder()

		builder.append("{")
		for (row <- matrix) {
			builder.append("[")
			for (it <- row) {
				builder.append(s" $it ,")
			}
			builder.setLength(builder.lastIndexOf(","))
			builder.append("],\n")
		}
		builder.setLength(builder.lastIndexOf(","))
		builder.append("}")

		builder.mkString
	}
}
