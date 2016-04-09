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
import java.util

import hr.caellian.math.util.DataUtil
import org.apache.commons.math3.linear._
import org.apache.commons.math3.util.FastMath

/**
  * Protected matrix class for {@link Float} N-dimensional matrices.
  *
  * @author Caellian
  */
protected class MatrixF(var matrix: Array[Array[Float]]) extends Matrix[Float] {

	/**
	  * @return this matrix.
	  */
	def unary_+ = {
		this
	}

	/**
	  * @return new matrix with all data negated.
	  */
	def unary_- : MatrixF = new MatrixF(asArray.map(_.map(-_)))

	/**
	  * Performs matrix addition and returns resulting matrix.
	  * In order to add to matrices together, they must be of same size.
	  *
	  * @param other matrix to add to this one.
	  *
	  * @return resulting of matrix addition.
	  */
	def +(other: MatrixF): MatrixF = {
		require(columnCount == other.columnCount && rowCount == other.rowCount, "Matrices must be of same size!")
		new MatrixF((matrix zip other.matrix).map{case (rowA, rowB) => rowA zip rowB map Function.tupled(_ + _)})
	}

	/**
	  * Performs matrix subtraction and returns resulting matrix.
	  * In order to subtract one matrix from another, matrices must be of same size.
	  *
	  * @param other matrix to subtract from this one.
	  *
	  * @return resulting of matrix subtraction.
	  */
	def -(other: MatrixF): MatrixF = {
		require(columnCount == other.columnCount && rowCount == other.rowCount, "Matrices must be of same size!")
		new MatrixF((matrix zip other.matrix).map{case (rowA, rowB) => rowA zip rowB map Function.tupled(_ - _)})
	}

	/**
	  * Performs matrix multiplication on this matrix.
	  * Returns C from 'C = A×B' where A is this matrix and B is the other / argument matrix.
	  *
	  * @param other matrix to multiply this matrix with.
	  *
	  * @return result of matrix multiplication.
	  */
	def *(other: MatrixF): MatrixF = {
		require(columnCount == other.rowCount , s"Invalid multiplication ($rowCount x $columnCount) * (${other.rowCount} x ${other.columnCount})!")
		new MatrixF(for (row <- matrix) yield {
			for (col <- other.matrix.transpose) yield {
				(row zip col map Function.tupled(_ * _)).sum
			}
		})
	}

	/**
	  * Performs scalar multiplication on this matrix and returns resulting matrix.
	  *
	  * @param scalar scalar to multiply every member of this matrix with.
	  *
	  * @return result of scalar matrix multiplication.
	  */
	def *(scalar: Float): MatrixF = {
		new MatrixF(asArray.map(_.map(_ * scalar)))
	}

	/**
	  * Number of columns this matrix has.
	  */
	val columnCount: Int = matrix(0).length

	/**
	  * Number of rows this matrix has.
	  */
	val rowCount: Int = matrix.length

	/**
	  * @return inverse matrix.
	  */
	def unary_! : MatrixF = {
		inverse
	}

	/**
	  * @return inverse matrix.
	  */
	def inverse: MatrixF = {
		new MatrixF(new LUDecomposition(new Array2DRowRealMatrix(asArray.map(_.map(_.toDouble)))).getSolver.getInverse.getData.map(_.map(_.toFloat)))
	}

	/**
	  * Switches two rows together.
	  *
	  * @param rowA row to be switched with rowB.
	  * @param rowB row to be switched with rowA.
	  *
	  * @return resulting matrix.
	  */
	def switchRows(rowA: Int, rowB: Int): MatrixF = {
		require(rowA <= columnCount && rowB <= columnCount && rowA >= 0 && rowB >= 0 && rowA != rowB, "Illegal row argument(s)!")
		val result = matrix.clone
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
	  *
	  * @return resulting matrix.
	  */
	def multiplyRow(row: Int, multiplier: Float): MatrixF = {
		require(row <= columnCount && row >= 0, "Illegal row argument!")
		require(multiplier != 0, "Multiplier can't be 0!")
		val result = matrix.clone
		for (posY <- matrix.indices) {
			result(row)(posY) = matrix(row)(posY) * multiplier
		}
		new MatrixF(result)

	}

	/**
	  * Adds one row from matrix to another.
	  *
	  * @param from       row to add to another row.
	  * @param to         row to add another row to; data will be stored on this row.
	  * @param multiplier scalar to multiply all members of added row with on addition. It equals to 1 by default.
	  *
	  * @return new matrix.
	  */
	def addRows(from: Int, to: Int, multiplier: Float = 1): MatrixF = {
		require(from <= columnCount && to <= columnCount && from >= 0 && to >= 0, "Illegal row argument(s)!")
		val result = matrix.clone
		for (posY <- matrix.indices) {
			result(to)(posY) += matrix(from)(posY) * multiplier
		}
		new MatrixF(result)
	}

	/**
	  * Inserts given row data at given index shifting rest of the matrix to the next index.
	  *
	  * @param index index at which added row data will be stored.
	  * @param data  row data to store at given index.
	  *
	  * @return new matrix with extended data.
	  */
	def withRow(index: Int, data: Array[Float]): MatrixF = {
		val parts = this.matrix.splitAt(index)
		val result = (parts._1 :+ data) ++ parts._2
		new MatrixF(result)
	}

	/**
	  * Inserts given column data at given index shifting rest of the matrix to the next index.
	  *
	  * @param index index at which added column data will be stored.
	  * @param data  column data to store at given index.
	  *
	  * @return new matrix with extended data.
	  */
	def withColumn(index: Int, data: Array[Float]): MatrixF = {
		val parts = this.matrix.transpose.splitAt(index)
		val result = (parts._1 :+ data) ++ parts._2
		new MatrixF(result.transpose)
	}

	/**
	  * Creates a new matrix without specified rows & columns.
	  *
	  * @param deletedRows    rows to exclude from submatrix.
	  * @param deletedColumns columns to exclude from submatrix.
	  *
	  * @return defined submatrix.
	  */
	def submatrix(deletedRows: Array[Int], deletedColumns: Array[Int]): MatrixF = {
		val result = Array.ofDim[Float](rowCount - deletedRows.count(rowCount >= _), columnCount - deletedColumns.count(columnCount >= _))
		matrix.indices.filterNot(deletedRows contains _ + 1).zipWithIndex.foreach{ case (row, i) =>
			matrix(0).indices.filterNot(deletedColumns contains _ + 1).zipWithIndex.foreach{ case (col, j) =>
				result(i)(j) = matrix(row)(col)
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
			new VectorF(firstRow.matrix(0))
		} else {
			new VectorF(firstColumn.transpose.matrix(0))
		}
	}

	/**
	  * Constructs a new vector out of any matrix dismissing extra data.
	  *
	  * @return vector containing only first column of matrix data.
	  */
	def forceToVector: VectorF = {
		new VectorF(firstColumn.transpose.matrix(0))
	}

	/**
	  * @return a new matrix containing only the first row of this matrix.
	  */
	def firstRow: MatrixF = {
		new MatrixF(Array(asArray(0)))
	}

	/**
	  * @return transposed matrix.
	  */
	def transpose: MatrixF = {
		new MatrixF(asArray.transpose)
	}

	/**
	  * @return a new matrix containing only the first column of this matrix.
	  */
	def firstColumn: MatrixF = {
		new MatrixF(Array({ val result = asArray.transpose; result(0) }).transpose)
	}

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
		matrix.forall(_.length == matrix(0).length)
	}

	/**
	  * @return buffer containing data of this matrix.
	  */
	def asBuffer: Buffer = {
		val buffer = ByteBuffer.allocateDirect((columnCount * rowCount) << 2).order(ByteOrder.nativeOrder).asFloatBuffer()
		matrix.foreach(buffer.put)
		buffer.flip
	}

	/**
	  * @return clone of this matrix.
	  */
	override def replicate(): MatrixF = {
		new MatrixF(asArray)
	}

	/**
	  * Matrix hashcode depends on matrix data and will change is matrix data is modified!
	  *
	  * @return hashcode of this matrix.
	  */
	override def hashCode: Int = {
		DataUtil.hashCode(matrix)
	}

	/**
	  * @param o other matrix or object instance of type extending matrix.
	  *
	  * @return true if this matrix is equal to other matrix.
	  */
	override def equals(o: Any): Boolean = {
		if (this == o) {
			return true
		}
		if (!o.isInstanceOf[VectorF]) {
			return false
		}
		DataUtil.equals(matrix, o.asInstanceOf[MatrixF].matrix)
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
		for (x <- this.matrix.indices) {
			for (y <- this.matrix(0).indices) {
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
				this.matrix(y)(x) = value
			}
		}
		this
	}
}

/**
  * Object used to create {@link hr.caellian.math.geometry.MatrixF MatrixF} matrices and ensure their consistency.
  * Use initializer functions from this object only if there is no alternative ones in more specific objects as some
  * of these are very CPU intensive.
  *
  * @author Caellian
  */
object MatrixF {

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
	  * @param buffer buffer to create a new matrix from.
	  * @param rowCount number of rows new matrix will have.
	  * @param columnCount number of columns new matrix will have.
	  *
	  * @return created matrix.
	  */
	def apply(buffer: FloatBuffer, rowCount: Int, columnCount: Int): MatrixF = {
		new MatrixF((0 until buffer.capacity()).map(buffer.get).toList.grouped(columnCount).map(_.toArray).toArray)
	}

	/**
	  * Creates a new square matrix using buffer values.
	  *
	  * @param buffer buffer to create a new matrix from.
	  *
	  * @return created matrix.
	  */
	def apply(buffer: FloatBuffer): MatrixF = {
		val rows = FastMath.sqrt(buffer.capacity())
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
	  *
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

		v(1) = (v(0).withColumn(n, Array.fill(n - 1)(1)) * M(0)).submatrix(Array(),Array(n + 1))

		val me = new MatrixF(M(0).matrix)
		var k = 1
		for (r <- 2 until n) {
			for (c <- n to r by -1) {
				k += 1
				M(k - 1) = MatrixF(n + 1).initPlaneRotation(c, c - 1, Math.atan2(v(k - 1).matrix(r - 1)(c-1), v(k - 1).matrix(r - 1)(c - 2)).toFloat)
				v(k) = (v(k - 1).withColumn(n, Array.fill(n - 1)(1)) * M(k - 1)).submatrix(Array(),Array(n + 1))
				me.matrix = (me * M(k - 1)).matrix
			}
		}
		new MatrixF((me * MatrixF(n + 1).initPlaneRotation(n - 1, n, angle) * !me).submatrix(Array(n+1),Array(n+1)).matrix)
	}

	/**
	  * Creates a new blank square matrix.
	  *
	  * @param size width & height of new matrix.
	  */
	def apply(size: Int) = {
		new MatrixF(Array.ofDim[Float](size, size))
	}

	/**
	  * Initializes a new translation matrix.
	  *
	  * @param location relative location.
	  *
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
	  *
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
	  *
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