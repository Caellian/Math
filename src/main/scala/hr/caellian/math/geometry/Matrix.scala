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
    * @param row    inner array index.
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
