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
