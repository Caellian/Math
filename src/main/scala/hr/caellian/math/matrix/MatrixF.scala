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

import java.nio._

import hr.caellian.math.util.DataUtil
import hr.caellian.math.vector.{Vector, VectorF}

/**
  * Matrix class for float N-dimensional matrices.
  *
  * @author Caellian
  */
class MatrixF(override val data: Array[Array[Float]]) extends Matrix[Float] {

  /**
    * @return new conjugated matrix.
    */
  def unary_- : MatrixF = new MatrixF(arrayClone.map(_.map(-_)))

  /**
    * Performs matrix addition and returns resulting matrix.
    * In order to add to matrices together, they must be of same size.
    *
    * @param other matrix to add to this one.
    * @return resulting of matrix addition.
    */
  def +(other: Matrix[Float]): MatrixF = {
    require(columnCount == other.columnCount && rowCount == other.rowCount, "Matrices must be of same size!")
    new MatrixF((data zip other.data).map { case (rowA, rowB) => rowA zip rowB map Function.tupled(_ + _) })
  }

  /**
    * Performs matrix subtraction and returns resulting matrix.
    * In order to subtract one matrix from another, matrices must be of same size.
    *
    * @param other matrix to subtract from this one.
    * @return resulting of matrix subtraction.
    */
  def -(other: Matrix[Float]): MatrixF = {
    require(columnCount == other.columnCount && rowCount == other.rowCount, "Matrices must be of same size!")
    new MatrixF((data zip other.data).map { case (rowA, rowB) => rowA zip rowB map Function.tupled(_ - _) })
  }

  /**
    * Performs matrix multiplication on this matrix.
    * Returns C from 'C = A×B' where A is this matrix and B is the other / argument matrix.
    *
    * @param other matrix to multiply this matrix with.
    * @return result of matrix multiplication.
    */
  def *(other: Matrix[Float]): MatrixF = {
    require(columnCount == other.rowCount, s"Invalid multiplication ($rowCount x $columnCount) * (${other.rowCount} x ${other.columnCount})!")
    new MatrixF(for (row <- data) yield {
      for (col <- other.data.transpose) yield {
        (row zip col map Function.tupled(_ * _)).sum
      }
    })
  }

  /**
    * Performs matrix multiplication on this matrix.
    * Returns C from 'C = A×B' where A is this matrix and B is the other / argument vector.
    *
    * @param other vector to multiply this matrix with.
    * @return result of matrix multiplication.
    */
  def *(other: Vector[Float]): VectorF = {
    this * other.verticalMatrix toVector
  }

  /**
    * Performs scalar multiplication on this matrix and returns resulting matrix.
    *
    * @param scalar scalar to multiply every member of this matrix with.
    * @return result of scalar matrix multiplication.
    */
  def *(scalar: Float): MatrixF = {
    new MatrixF(arrayClone.map(_.map(_ * scalar)))
  }

  /**
    * Switches two rows together.
    *
    * @param rowA row to be switched with rowB.
    * @param rowB row to be switched with rowA.
    * @return resulting matrix.
    */
  def switchRows(rowA: Int, rowB: Int): MatrixF = {
    require(rowA <= columnCount && rowB <= columnCount && rowA >= 0 && rowB >= 0 && rowA != rowB, "Illegal row argument(s)!")
    val result = data.clone
    val buff = result(rowA)
    result(rowA) = result(rowB)
    result(rowB) = buff
    new MatrixF(result)
  }

  /**
    * Multiplies all entries of a row with given scalar.
    *
    * @param row        row to multiply.
    * @param multiplier scalar to multiply rows entries with.
    * @return resulting matrix.
    */
  def multiplyRow(row: Int, multiplier: Float): MatrixF = {
    require(row <= columnCount && row >= 0, "Illegal row argument!")
    require(multiplier != 0, "Multiplier can't be 0!")
    val result = data.clone
    for (posY <- data.indices) {
      result(row)(posY) = data(row)(posY) * multiplier
    }
    new MatrixF(result)

  }

  /**
    * Adds one row from matrix to another.
    *
    * @param from       row to add to another row.
    * @param to         row to add another row to; data will be stored on this row.
    * @param multiplier scalar to multiply all members of added row with on addition. It equals to 1 by default.
    * @return new matrix.
    */
  def addRows(from: Int, to: Int, multiplier: Float = 1): MatrixF = {
    require(from <= columnCount && to <= columnCount && from >= 0 && to >= 0, "Illegal row argument(s)!")
    val result = data.clone
    for (posY <- data.indices) {
      result(to)(posY) += data(from)(posY) * multiplier
    }
    new MatrixF(result)
  }

  /**
    * Inserts given row data at given index shifting rest of the matrix to the next index.
    *
    * @param index index at which added row data will be stored.
    * @param data  row data to store at given index.
    * @return new matrix with extended data.
    */
  def withRow(index: Int, data: Array[Float]): MatrixF = {
    val parts = this.data.splitAt(index)
    val result = (parts._1 :+ data) ++ parts._2
    new MatrixF(result)
  }

  /**
    * Inserts given column data at given index shifting rest of the matrix to the next index.
    *
    * @param index index at which added column data will be stored.
    * @param data  column data to store at given index.
    * @return new matrix with extended data.
    */
  def withColumn(index: Int, data: Array[Float]): MatrixF = {
    val parts = this.data.transpose.splitAt(index)
    val result = (parts._1 :+ data) ++ parts._2
    new MatrixF(result.transpose)
  }

  /**
    * Creates a new matrix without specified rows & columns.
    *
    * @param deletedRows    rows to exclude from submatrix.
    * @param deletedColumns columns to exclude from submatrix.
    * @return defined submatrix.
    */
  def submatrix(deletedRows: Array[Int], deletedColumns: Array[Int]): MatrixF = {
    val result = Array.ofDim[Float](rowCount - deletedRows.count(rowCount >= _), columnCount - deletedColumns.count(columnCount >= _))
    data.indices.filterNot(deletedRows contains _ + 1).zipWithIndex.foreach { case (row, i) =>
      data(0).indices.filterNot(deletedColumns contains _ + 1).zipWithIndex.foreach { case (col, j) =>
        result(i)(j) = data(row)(col)
      }
    }
    new MatrixF(result)
  }

  /**
    * Constructs a new vector out of column / row vector matrix.
    *
    * @return vector containing matrix data.
    */
  def toVector: VectorF = {
    require(columnCount == 1 || rowCount == 1 && !(columnCount > 1 && rowCount > 1), "Matrix cannot be turned into a vector!")
    if (columnCount > rowCount) {
      new VectorF(firstRow(0))
    } else {
      new VectorF(firstColumn.transpose(0))
    }
  }

  /**
    * Constructs a new vector out of any matrix dismissing extra data.
    *
    * @return vector containing only first column of matrix data.
    */
  def forceToVector: VectorF = {
    new VectorF(firstColumn.transpose(0))
  }

  /**
    * @return a new matrix containing only the first row of this matrix.
    */
  def firstRow: MatrixF = {
    new MatrixF(Array(arrayClone(0)))
  }

  /**
    * @return a new matrix containing only the first column of this matrix.
    */
  def firstColumn: MatrixF = {
    new MatrixF(Array({
      val result = arrayClone.transpose;
      result(0)
    }).transpose)
  }

  /**
    * @return transposed matrix.
    */
  def transpose: MatrixF = {
    new MatrixF(arrayClone.transpose)
  }

  /**
    * @return 2D array containing data of this matrix.
    */
  def arrayClone: Array[Array[Float]] = {
    data.map(_.map(_ * 1))
  }

  /**
    * @return buffer containing data of this matrix.
    */
  def asBuffer: Buffer = {
    val buffer = ByteBuffer.allocateDirect((columnCount * rowCount) << 2).order(ByteOrder.nativeOrder).asFloatBuffer()
    data.foreach(buffer.put)
    buffer.flip
  }

  /**
    * @return clone of this matrix.
    */
  override def replicated: MatrixF = {
    new MatrixF(arrayClone)
  }

  /**
    * Creates a new instance of wrapper containing given data.
    *
    * @param data data of new wrapper.
    * @return new instance of wrapper containing argument data.
    */
  override def withData(data: Array[Array[Float]]): MatrixF = new MatrixF(data)

  /**
    * Matrix hashcode depends on matrix data and will change is matrix data is modified!
    *
    * @return hashcode of this matrix.
    */
  override def hashCode: Int = {
    DataUtil.hashCode(data)
  }

  /**
    * @param o other matrix or object instance of type extending matrix.
    * @return true if this matrix is equal to other matrix.
    */
  override def equals(o: Any): Boolean = {
    if (this == o) {
      return true
    }
    if (!o.isInstanceOf[VectorF]) {
      return false
    }
    DataUtil.equals(data, o.asInstanceOf[MatrixF].data)
  }
}

/**
  * Object used to create {@link MatrixF} matrices and ensure their consistency.
  * Use initializer functions from this object only if there is no alternative ones in more specific objects as some
  * of these are very CPU intensive.
  *
  * @author Caellian
  */
object MatrixF {
  type MatF = MatrixF

  /**
    * Creates a new blank square matrix.
    *
    * @param size width & height of new matrix.
    */
  def apply(size: Int) = {
    new MatrixF(Array.ofDim[Float](size, size))
  }

  /**
    * Creates a new matrix containing given data.
    *
    * @param rows    number of rows matrix should store.
    * @param columns number of columns matrix should store.
    */
  def apply(rows: Int, columns: Int) = {
    new MatrixF(Array.ofDim[Float](rows, columns))
  }

  /**
    * Creates a new matrix containing given data.
    *
    * @param matrix data stored in new matrix.
    */
  def apply(matrix: Array[Array[Float]]): MatrixF = {
    require(matrix.forall(_.length == matrix(0).length), "Matrix rows must be of equal length!")
    new MatrixF(matrix)
  }

  /**
    * Creates a new matrix from given arrays.
    *
    * @param vertical if true, arrays will represent columns; if false, they will represent rows.
    * @param arrays   data stored in new matrix.
    *
    */
  def apply(vertical: Boolean, arrays: Array[Float]*): MatrixF = {
    require(arrays.forall(_.length == arrays(0).length), if (vertical) {
      "Matrix columns must be of equal length!"
    } else {
      "Matrix rows must be of equal length!"
    })
    new MatrixF(if (vertical) {
      arrays.toArray.transpose
    } else {
      arrays.toArray
    })
  }

  /**
    * Creates a new matrix using buffer values.
    *
    * @param buffer      buffer to create a new matrix from.
    * @param rowCount    number of rows new matrix will have.
    * @param columnCount number of columns new matrix will have.
    * @return created matrix.
    */
  def apply(buffer: FloatBuffer, rowCount: Int, columnCount: Int): MatrixF = {
    new MatrixF((0 until buffer.capacity()).map(buffer.get).toList.grouped(columnCount).map(_.toArray).toArray)
  }

  /**
    * Creates a new square matrix using buffer values.
    *
    * @param buffer buffer to create a new matrix from.
    * @return created matrix.
    */
  def apply(buffer: FloatBuffer): MatrixF = {
    val rows = Math.sqrt(buffer.capacity())
    // This might happen if buffer has more space allocated than it's being used.
    require(rows.isValidInt, "Acquired buffer can't be used to create a square matrix.")
    new MatrixF((0 until buffer.capacity()).map(buffer.get).toList.grouped(rows.toInt).map(_.toArray).toArray)
  }

  /**
    * Initializes a new n-dimensional rotation matrix.<br>
    * For details see: <a href="http://wscg.zcu.cz/wscg2004/Papers_2004_Short/N29.pdf">Aguilera - Perez Algorithm</a>
    *
    * @param rotationData defining data. Rows of this matrix represent points defining
    *                     simplex to perform this rotation around. Points must have their
    *                     position in all 'n' dimensions defined and there must be 'n-1'
    *                     points to define rotation simplex.
    * @param angle        degrees to rotate with objects multiplied by this rotation matrix.
    * @return rotation matrix.
    */
  def initRotation(rotationData: MatrixF, angle: Float): MatrixF = {
    val n = rotationData.columnCount
    require(n >= 2, s"Can't do rotation in $n-dimensional space!")
    require(rotationData.rowCount == n - 1, s"Insufficient / invalid data! Can't perform rotation.")

    val v = Array.ofDim[MatrixF](n * (n - 1) / 2 + 1)
    val M = Array.ofDim[MatrixF](n * (n - 1) / 2 + 1)

    v(0) = rotationData
    M(0) = MatrixF.initTranslationMatrix((-rotationData.firstRow.toVector).asArray).transpose

    v(1) = (v(0).withColumn(n, Array.fill(n - 1)(1)) * M(0)).submatrix(Array(), Array(n + 1))

    var me = new MatrixF(M(0).data)
    var k = 1
    for (r <- 2 until n) {
      for (c <- n to r by -1) {
        k += 1
        M(k - 1) = MatrixF.initPlaneRotation(n + 1, c, c - 1, Math.atan2(v(k - 1)(r - 1)(c - 1), v(k - 1)(r - 1)(c - 2)).toFloat)
        v(k) = (v(k - 1).withColumn(n, Array.fill(n - 1)(1)) * M(k - 1)).submatrix(Array(), Array(n + 1))
        me = me * M(k - 1)
      }
    }
    new MatrixF((me * MatrixF.initPlaneRotation(n + 1, n - 1, n, angle) * me.inverseUnsafe()).submatrix(Array(n + 1), Array(n + 1)).data)
  }

  /**
    * Initializes a plane rotation matrix.
    *
    * @param a     first argument.
    * @param b     second argument.
    * @param angle degrees to rotate in objects multiplied by this rotation matrix.
    * @return plane rotation matrix.
    */
  def initPlaneRotation(size: Int, a: Int, b: Int, angle: Float): MatrixF = {
    val result = new MatrixF(Array.ofDim[Float](size, size))
    val ai = a - 1
    val bi = b - 1
    for (x <- result.data.indices) {
      for (y <- result(0).indices) {
        val value = (y, x) match {
          case (`ai`, `ai`) => Math.cos(Math.toRadians(angle))
          case (`bi`, `bi`) => Math.cos(Math.toRadians(angle))
          case (`ai`, `bi`) => -Math.sin(Math.toRadians(angle))
          case (`bi`, `ai`) => Math.sin(Math.toRadians(angle))
          case _ =>
            if (x == y) {
              1
            } else {
              0
            }
        }
        result(y)(x) = value.toFloat
      }
    }
    result
  }

  /**
    * Initializes a new translation matrix.
    *
    * @param location relative location.
    * @return translation matrix.
    */
  def initTranslationMatrix(location: Array[Float]): MatrixF = {
    new MatrixF(Array.tabulate[Float](location.length + 1, location.length + 1)((x,
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
    * Initializes a new scaling matrix.
    *
    * @param scale scale.
    * @return scale matrix.
    */
  def initScalingMatrix(scale: Array[Float]): MatrixF = {
    new MatrixF(Array.tabulate[Float](scale.length, scale.length)((x, y) => {
      if (x == y) {
        scale(x)
      } else {
        0
      }
    }))
  }

  /**
    * Initializes a new identity matrix.
    *
    * @param n matrix size.
    * @return identity matrix.
    */
  def initIdentityMatrix(n: Int): MatrixF = {
    new MatrixF(Array.tabulate[Float](n, n)((x, y) => {
      if (x == y) {
        1
      } else {
        0
      }
    }))
  }
}






