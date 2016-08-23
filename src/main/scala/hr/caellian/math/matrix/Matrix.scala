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

package hr.caellian.math.matrix

import java.nio.Buffer

import hr.caellian.math.util._
import hr.caellian.math.vector.Vector


/**
  * Generic Matrix trait defined for stable code infrastructure and consistency.
  * Most basic Matrix variables and functions can be found here.
  *
  * @author Caellian
  */
trait Matrix[T <: AnyVal] extends Serializable with Replicable[Matrix[T]] with DataWrapper[Matrix[T], Array[Array[T]]] {
  /**
    * @return number of rows in this matrix.
    */
  def rowCount: Int = data.length

  /**
    * @return number of columns in this matrix.
    */
  def columnCount: Int = data(0).length

  /**
    * @return true if this is square matrix.
    */
  def isSquare: Boolean = rowCount == columnCount

  /**
    * @param row row column entries to return
    * @return all column entries at argument row
    */
  def apply(row: Int): Array[T] = data(row)


  def memLoc(pos: Int): T = {
    val row = pos / columnCount
    this(row)(pos - columnCount * row)
  }

  def memSet(pos: Int, value: T): Unit = {
    val row = pos / columnCount
    this(row)(pos - columnCount * row) = value
  }

  /**
    * @return this matrix.
    */
  def unary_+ = this

  /**
    * @return new conjugated matrix.
    */
  def unary_- : Matrix[T]

  /**
    * Performs matrix addition and returns resulting matrix.
    * In order to add to matrices together, they must be of same size.
    *
    * @param other matrix to add to this one.
    * @return resulting of matrix addition.
    */
  def +(other: Matrix[T]): Matrix[T]

  /**
    * Performs matrix subtraction and returns resulting matrix.
    * In order to subtract one matrix from another, matrices must be of same size.
    *
    * @param other matrix to subtract from this one.
    * @return resulting of matrix subtraction.
    */
  def -(other: Matrix[T]): Matrix[T]

  /**
    * Performs matrix multiplication on this matrix.
    * Returns C from 'C = A×B' where A is this matrix and B is the other / argument matrix.
    *
    * @param other matrix to multiply this matrix with.
    * @return result of matrix multiplication.
    */
  def *(other: Matrix[T]): Matrix[T]

  /**
    * Performs matrix multiplication on this matrix.
    * Returns C from 'C = A×B' where A is this matrix and B is the other / argument vector.
    *
    * @param other vector to multiply this matrix with.
    * @return result of matrix multiplication.
    */
  def *(other: Vector[T]): Vector[T]

  /**
    * Performs scalar multiplication on this matrix and returns resulting matrix.
    *
    * @param scalar scalar to multiply every member of this matrix with.
    * @return result of scalar matrix multiplication.
    */
  def *(scalar: T): Matrix[T]

  /**
    * @return inverse matrix.
    */
  def unary_! : Matrix[T] = inverse()

  /**
    * @return inverse matrix.
    */
  def inverse(singularityThreshold: Double = 1e-11): Matrix[T] = {
    withData(Inverse.inverseMatrix(arrayClone.asInstanceOf[Array[Array[Double]]], singularityThreshold).asInstanceOf[Array[Array[T]]])
  }

  /**
    * This method replaces data of this matrix with LU decomposition data.
    *
    * @return inverse matrix.
    */
  def inverseUnsafe(singularityThreshold: Double = 1e-11): Matrix[T] = {
    withData(Inverse.inverseMatrix(data.asInstanceOf[Array[Array[Double]]], singularityThreshold).asInstanceOf[Array[Array[T]]])
  }

  /**
    * Switches two rows together.
    *
    * @param rowA row to be switched with rowB.
    * @param rowB row to be switched with rowA.
    * @return resulting matrix.
    */
  def switchRows(rowA: Int, rowB: Int): Matrix[T]

  /**
    * Multiplies all entries of a row with given scalar.
    *
    * @param row        row to multiply.
    * @param multiplier scalar to multiply rows entries with.
    * @return resulting matrix.
    */
  def multiplyRow(row: Int, multiplier: T): Matrix[T]

  /**
    * Adds one row from matrix to another.
    *
    * @param from       row to add to another row.
    * @param to         row to add another row to; data will be stored on this row.
    * @param multiplier scalar to multiply all members of added row with on addition. It equals to 1 by default.
    * @return new matrix.
    */
  def addRows(from: Int, to: Int, multiplier: T = 1.asInstanceOf[T]): Matrix[T]

  /**
    * Inserts given row data at given index shifting rest of the matrix to the next index.
    *
    * @param index index at which added row data will be stored.
    * @param data  row data to store at given index.
    * @return new matrix with extended data.
    */
  def withRow(index: Int, data: Array[T]): Matrix[T]

  /**
    * Inserts given column data at given index shifting rest of the matrix to the next index.
    *
    * @param index index at which added column data will be stored.
    * @param data  column data to store at given index.
    * @return new matrix with extended data.
    */
  def withColumn(index: Int, data: Array[T]): Matrix[T]

  /**
    * Creates a new matrix without specified rows & columns.
    *
    * @param deletedRows    rows to exclude from submatrix.
    * @param deletedColumns columns to exclude from submatrix.
    * @return defined submatrix.
    */
  def submatrix(deletedRows: Array[Int], deletedColumns: Array[Int]): Matrix[T]

  /**
    * Constructs a new vector out of column / row vector matrix.
    *
    * @return vector containing matrix data.
    */
  def toVector: Vector[T]

  /**
    * Constructs a new vector out of any matrix dismissing extra data.
    *
    * @return vector containing only first column of matrix data.
    */
  def forceToVector: Vector[T]

  /**
    * @return a new matrix containing only the first row of this matrix.
    */
  def firstRow: Matrix[T]

  /**
    * @return a new matrix containing only the first column of this matrix.
    */
  def firstColumn: Matrix[T]

  /**
    * @return transposed matrix.
    */
  def transpose: Matrix[T]

  /**
    * @return true if matrix is valid
    */
  def ? : Boolean = {
    validate
  }

  /**
    * @return true if matrix is valid
    */
  def validate: Boolean = {
    data.forall(_.length == data(0).length)
  }

  /**
    * @return 2D array containing data of this matrix.
    */
  def arrayClone: Array[Array[T]]

  /**
    * @return buffer containing data of this matrix.
    */
  def asBuffer: Buffer

  /**
    * @return clone of this matrix.
    */
  override def replicated: Matrix[T]

  /**
    * Matrix hashcode depends on matrix data and will change is matrix data is modified!
    *
    * @return hashcode of this matrix.
    */
  override def hashCode: Int

  /**
    * @return string representation of this matrix.
    */
  override def toString: String = {
    val builder = new StringBuilder()

    builder.append("{")
    for (row <- data) {
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
