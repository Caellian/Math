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

import hr.caellian.math.util.Replicable

/**
  * Generic Vector class created for stable code infrastructure and consistency.
  * Most basic Vector variables and functions can be found here.
  *
  * @author Caellian
  */
abstract class Vector[T <: AnyVal] extends Replicable[Vector[T]] {

	/**
	  * Vector data.
	  */
	val data: Array[T]
	/**
	  * Size of this vector.
	  */
	val size: Int

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
