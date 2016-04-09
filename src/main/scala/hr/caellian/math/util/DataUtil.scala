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

package hr.caellian.math.util

/**
  * Utility object containing functions for data comparison and hash code generation.
  *
  * @author Caellian
  */
object DataUtil {
	/**
	  * @param array object to calculate hash code of.
	  * @return hash code of given array.
	  */
	def hashCode(array: Array[Float]): Int ={
		var res = 1
		for (elem <- array) {
			res = 37 * res + elem.##
		}
		res
	}

	/**
	  * @param array object to calculate hash code of.
	  * @return hash code of given 2D array.
	  */
	def hashCode(array: Array[Array[Float]]): Int ={
		var res = 1
		for (elem <- array) {
			res = 37 * res + hashCode(elem)
		}
		res
	}

	/**
	  * @param first first array.
	  * @param second second array.
	  * @return true if 2D arrays are equal.
	  */
	def equals(first: Array[Array[Float]], second: Array[Array[Float]]): Boolean ={
		if (first.length != second.length) {
			false
		} else {
			for (i <- first.indices) {
				if (first(i) sameElements second(i)){
					return false
				}
			}
			true
		}
	}
}
