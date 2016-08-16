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

import java.nio.{Buffer, ByteBuffer, ByteOrder, IntBuffer}

import hr.caellian.math.util.Data
import org.apache.commons.math3.linear.{Array2DRowRealMatrix, LUDecomposition}
import org.apache.commons.math3.util.FastMath

/**
  * Matrix class for integer N-dimensional matrices.
  *
  * @author Caellian
  */
class MatrixI(var matrix: Array[Array[Int]]) extends Matrix[Int] {

  /**
    * @return this matrix.
    */
  def unary_+ = {
    this
  }

  /**
    * @return new matrix with all data negated.
    */
  def unary_- : MatrixI = new MatrixI(asArray.map(_.map(-_)))

  /**
    * Performs matrix addition and returns resulting matrix.
    * In order to add to matrices together, they must be of same size.
    *
    * @param other matrix to add to this one.
    * @return resulting of matrix addition.
    */
  def +(other: MatrixI): MatrixI = {
    require(columnCount == other.columnCount && rowCount == other.rowCount, "Matrices must be of same size!")
    new MatrixI((matrix zip other.matrix).map { case (rowA, rowB) => rowA zip rowB map Function.tupled(_ + _) })
  }

  /**
    * Performs matrix subtraction and returns resulting matrix.
    * In order to subtract one matrix from another, matrices must be of same size.
    *
    * @param other matrix to subtract from this one.
    * @return resulting of matrix subtraction.
    */
  def -(other: MatrixI): MatrixI = {
    require(columnCount == other.columnCount && rowCount == other.rowCount, "Matrices must be of same size!")
    new MatrixI((matrix zip other.matrix).map { case (rowA, rowB) => rowA zip rowB map Function.tupled(_ - _) })
  }

  /**
    * Performs matrix multiplication on this matrix.
    * Returns C from 'C = A×B' where A is this matrix and B is the other / argument matrix.
    *
    * @param other matrix to multiply this matrix with.
    * @return result of matrix multiplication.
    */
  def *(other: MatrixI): MatrixI = {
    require(columnCount == other.rowCount, s"Invalid multiplication ($rowCount x $columnCount) * (${other.rowCount} x ${other.columnCount})!")
    new MatrixI(for (row <- matrix) yield {
      for (col <- other.matrix.transpose) yield {
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
  def *(other: VectorI): VectorI = {
    this * other.verticalMatrix toVector
  }

  /**
    * Performs scalar multiplication on this matrix and returns resulting matrix.
    *
    * @param scalar scalar to multiply every member of this matrix with.
    * @return result of scalar matrix multiplication.
    */
  def *(scalar: Int): MatrixI = {
    new MatrixI(asArray.map(_.map(_ * scalar)))
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
  def unary_! : MatrixI = {
    inverse
  }

  /**
    * @return inverse matrix.
    */
  def inverse: MatrixI = {
    new MatrixI(new LUDecomposition(new Array2DRowRealMatrix(asArray.map(_.map(_.toDouble)))).getSolver.getInverse.getData.map(_.map(_.toInt)))
  }

  /**
    * Switches two rows together.
    *
    * @param rowA row to be switched with rowB.
    * @param rowB row to be switched with rowA.
    * @return resulting matrix.
    */
  def switchRows(rowA: Int, rowB: Int): MatrixI = {
    require(rowA <= columnCount && rowB <= columnCount && rowA >= 0 && rowB >= 0 && rowA != rowB, "Illegal row argument(s)!")
    val result = matrix.clone
    val buff = result(rowA)
    result(rowA) = result(rowB)
    result(rowB) = buff
    new MatrixI(result)
  }

  /**
    * Multiplies all entries of a row with given scalar.
    *
    * @param row        row to multiply.
    * @param multiplier scalar to multiply rows entries with.
    * @return resulting matrix.
    */
  def multiplyRow(row: Int, multiplier: Int): MatrixI = {
    require(row <= columnCount && row >= 0, "Illegal row argument!")
    require(multiplier != 0, "Multiplier can't be 0!")
    val result = matrix.clone
    for (posY <- matrix.indices) {
      result(row)(posY) = matrix(row)(posY) * multiplier
    }
    new MatrixI(result)

  }

  /**
    * Adds one row from matrix to another.
    *
    * @param from       row to add to another row.
    * @param to         row to add another row to; data will be stored on this row.
    * @param multiplier scalar to multiply all members of added row with on addition. It equals to 1 by default.
    * @return new matrix.
    */
  def addRows(from: Int, to: Int, multiplier: Int = 1): MatrixI = {
    require(from <= columnCount && to <= columnCount && from >= 0 && to >= 0, "Illegal row argument(s)!")
    val result = matrix.clone
    for (posY <- matrix.indices) {
      result(to)(posY) += matrix(from)(posY) * multiplier
    }
    new MatrixI(result)
  }

  /**
    * Inserts given row data at given index shifting rest of the matrix to the next index.
    *
    * @param index index at which added row data will be stored.
    * @param data  row data to store at given index.
    * @return new matrix with extended data.
    */
  def withRow(index: Int, data: Array[Int]): MatrixI = {
    val parts = this.matrix.splitAt(index)
    val result = (parts._1 :+ data) ++ parts._2
    new MatrixI(result)
  }

  /**
    * Inserts given column data at given index shifting rest of the matrix to the next index.
    *
    * @param index index at which added column data will be stored.
    * @param data  column data to store at given index.
    * @return new matrix with extended data.
    */
  def withColumn(index: Int, data: Array[Int]): MatrixI = {
    val parts = this.matrix.transpose.splitAt(index)
    val result = (parts._1 :+ data) ++ parts._2
    new MatrixI(result.transpose)
  }

  /**
    * Creates a new matrix without specified rows & columns.
    *
    * @param deletedRows    rows to exclude from submatrix.
    * @param deletedColumns columns to exclude from submatrix.
    * @return defined submatrix.
    */
  def submatrix(deletedRows: Array[Int], deletedColumns: Array[Int]): MatrixI = {
    val result = Array.ofDim[Int](rowCount - deletedRows.count(rowCount >= _), columnCount - deletedColumns.count(columnCount >= _))
    matrix.indices.filterNot(deletedRows contains _ + 1).zipWithIndex.foreach { case (row, i) =>
      matrix(0).indices.filterNot(deletedColumns contains _ + 1).zipWithIndex.foreach { case (col, j) =>
        result(i)(j) = matrix(row)(col)
      }
    }
    new MatrixI(result)
  }

  /**
    * Constructs a new vector out of column / row vector matrix.
    *
    * @return vector containing matrix data.
    */
  def toVector: VectorI = {
    require(columnCount == 1 || rowCount == 1 && !(columnCount > 1 && rowCount > 1), "Matrix cannot be turned into a vector!")
    if (columnCount > rowCount) {
      new VectorI(firstRow(0))
    } else {
      new VectorI(firstColumn.transpose(0))
    }
  }

  /**
    * Constructs a new vector out of any matrix dismissing extra data.
    *
    * @return vector containing only first column of matrix data.
    */
  def forceToVector: VectorI = {
    new VectorI(firstColumn.transpose(0))
  }

  /**
    * @return a new matrix containing only the first row of this matrix.
    */
  def firstRow: MatrixI = {
    new MatrixI(Array(asArray(0)))
  }

  /**
    * @return transposed matrix.
    */
  def transpose: MatrixI = {
    new MatrixI(asArray.transpose)
  }

  /**
    * @return a new matrix containing only the first column of this matrix.
    */
  def firstColumn: MatrixI = {
    new MatrixI(Array({
      val result = asArray.transpose; result(0)
    }).transpose)
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
    val buffer = ByteBuffer.allocateDirect((columnCount * rowCount) << 2).order(ByteOrder.nativeOrder).asIntBuffer()
    matrix.foreach(buffer.put)
    buffer.flip
  }

  /**
    * @return clone of this matrix.
    */
  override def replicate(): MatrixI = {
    new MatrixI(asArray)
  }

  /**
    * Matrix hashcode depends on matrix data and will change is matrix data is modified!
    *
    * @return hashcode of this matrix.
    */
  override def hashCode: Int = {
    Data.hashCode(matrix)
  }

  /**
    * @param o other matrix or object instance of type extending matrix.
    * @return true if this matrix is equal to other matrix.
    */
  override def equals(o: Any): Boolean = {
    if (this == o) {
      return true
    }
    if (!o.isInstanceOf[VectorI]) {
      return false
    }
    Data.equals(matrix, o.asInstanceOf[MatrixI].matrix)
  }

  /**
    * Initializes a rotation matrix.
    *
    * @param a     first argument.
    * @param b     second argument.
    * @param angle degrees to rotate in objects multiplied by this rotation matrix.
    * @return plane rotation matrix.
    */
  def initPlaneRotation(a: Int, b: Int, angle: Int): MatrixI = {
    val ai = a - 1
    val bi = b - 1
    for (x <- this.matrix.indices) {
      for (y <- this(0).indices) {
        val value = (y, x) match {
          case (`ai`, `ai`) => FastMath.cos(FastMath.toRadians(angle)).toInt
          case (`bi`, `bi`) => FastMath.cos(FastMath.toRadians(angle)).toInt
          case (`ai`, `bi`) => -FastMath.sin(FastMath.toRadians(angle)).toInt
          case (`bi`, `ai`) => FastMath.sin(FastMath.toRadians(angle)).toInt
          case _ => {
            if (x == y) {
              1
            } else {
              0
            }
          }
        }
        this(y)(x) = value
      }
    }
    this
  }
}

/**
  * Object used to create {@link MatrixI} matrices and ensure their consistency.
  * Use initializer functions from this object only if there is no alternative ones in more specific objects as some
  * of these are very CPU intensive.
  *
  * @author Caellian
  */
object MatrixI {
  type MatF = MatrixI

  /**
    * Creates a new matrix containing given data.
    *
    * @param rows    number of rows matrix should store.
    * @param columns number of columns matrix should store.
    */
  def apply(rows: Int, columns: Int) = {
    new MatrixI(Array.ofDim[Int](rows, columns))
  }

  /**
    * Creates a new matrix containing given data.
    *
    * @param matrix data stored in new matrix.
    */
  def apply(matrix: Array[Array[Int]]): MatrixI = {
    require(matrix.forall(_.length == matrix(0).length), "Matrix rows must be of equal length!")
    new MatrixI(matrix)
  }

  /**
    * Creates a new matrix from given arrays.
    *
    * @param vertical if true, arrays will represent columns; if false, they will represent rows.
    * @param arrays   data stored in new matrix.
    *
    */
  def apply(vertical: Boolean, arrays: Array[Int]*): MatrixI = {
    require(arrays.forall(_.length == arrays(0).length), if (vertical) {
      "Matrix columns must be of equal length!"
    } else {
      "Matrix rows must be of equal length!"
    })
    new MatrixI(if (vertical) {
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
  def apply(buffer: IntBuffer, rowCount: Int, columnCount: Int): MatrixI = {
    new MatrixI((0 until buffer.capacity()).map(buffer.get).toList.grouped(columnCount).map(_.toArray).toArray)
  }

  /**
    * Creates a new square matrix using buffer values.
    *
    * @param buffer buffer to create a new matrix from.
    * @return created matrix.
    */
  def apply(buffer: IntBuffer): MatrixI = {
    val rows = FastMath.sqrt(buffer.capacity())
    // This might happen if buffer has more space allocated than it's being used.
    require(rows.isValidInt, "Acquired buffer can't be used to create a square matrix.")
    new MatrixI((0 until buffer.capacity()).map(buffer.get).toList.grouped(rows.toInt).map(_.toArray).toArray)
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
  def initRotation(rotationData: MatrixI, angle: Int): MatrixI = {
    val n = rotationData.columnCount
    require(n >= 2, s"Can't do rotation in $n-dimensional space!")
    require(rotationData.rowCount == n - 1, s"Insufficient / invalid data! Can't perform rotation.")

    val v = Array.ofDim[MatrixI](n * (n - 1) / 2 + 1)
    val M = Array.ofDim[MatrixI](n * (n - 1) / 2 + 1)

    v(0) = rotationData
    M(0) = MatrixI.initTranslationMatrix((-rotationData.firstRow.toVector).asArray).transpose

    v(1) = (v(0).withColumn(n, Array.fill(n - 1)(1)) * M(0)).submatrix(Array(), Array(n + 1))

    val me = new MatrixI(M(0).matrix)
    var k = 1
    for (r <- 2 until n) {
      for (c <- n to r by -1) {
        k += 1
        M(k - 1) = MatrixI(n + 1).initPlaneRotation(c, c - 1, Math.atan2(v(k - 1)(r - 1)(c - 1), v(k - 1)(r - 1)(c - 2)).toInt)
        v(k) = (v(k - 1).withColumn(n, Array.fill(n - 1)(1)) * M(k - 1)).submatrix(Array(), Array(n + 1))
        me.matrix = (me * M(k - 1)).matrix
      }
    }
    new MatrixI((me * MatrixI(n + 1).initPlaneRotation(n - 1, n, angle) * !me).submatrix(Array(n + 1), Array(n + 1)).matrix)
  }

  /**
    * Creates a new blank square matrix.
    *
    * @param size width & height of new matrix.
    */
  def apply(size: Int) = {
    new MatrixI(Array.ofDim[Int](size, size))
  }

  /**
    * Initializes a new translation matrix.
    *
    * @param location relative location.
    * @return translation matrix.
    */
  def initTranslationMatrix(location: Array[Int]): MatrixI = {
    new MatrixI(Array.tabulate[Int](location.length + 1, location.length + 1)((x,
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
  def initScalingMatrix(scale: Array[Int]): MatrixI = {
    new MatrixI(Array.tabulate[Int](scale.length, scale.length)((x, y) => {
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
  def initIdentityMatrix(n: Int): MatrixI = {
    new MatrixI(Array.tabulate[Int](n, n)((x, y) => {
      if (x == y) {
        1
      } else {
        0
      }
    }))
  }
}

/**
  * Utility object containing initializers for basic 2x2 matrices.
  * These functions should be used instead of any provided by {@link MatrixI}
  * wherever possible as they are supposed to perform faster.
  *
  * @author Caellian
  */
object Matrix2I {
  /**
    * Initializes rotation matrix using degrees.
    *
    * @param degrees Degrees to rotate objects multiplied by this matrix in positive direction.
    * @return rotation matrix
    */
  def initRotation(degrees: Int): MatrixI = {
    if (degrees == 0) {
      return initIdentityMatrix()
    }

    val result = Array.ofDim[Int](2, 2)
    result(0)(0) = FastMath.cos(FastMath.toRadians(degrees)).toInt
    result(0)(1) = FastMath.sin(FastMath.toRadians(degrees)).toInt
    result(1)(0) = -FastMath.sin(FastMath.toRadians(degrees)).toInt
    result(1)(1) = FastMath.cos(FastMath.toRadians(degrees)).toInt

    new MatrixI(result)
  }

  /**
    * Initializes a new 2x2 identity matrix.
    *
    * @return identity matrix.
    */
  def initIdentityMatrix(): MatrixI = {
    new MatrixI(Array.tabulate[Int](2, 2)((x, y) => {
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
    * @return scale matrix.
    */
  def initScalingMatrix(scale: Array[Int]): MatrixI = {
    assert(scale.length == 2, "Translation must have 2 values!")
    new MatrixI(Array.tabulate[Int](2, 2)((x, y) => {
      if (x == y) {
        scale(x)
      } else {
        0
      }
    }))
  }
}

/**
  * Utility object containing initializers for basic 3x3 matrices.
  * These functions should be used instead of any provided by {@link MatrixI}
  * wherever possible as they are supposed to perform faster.
  *
  * @author Caellian
  */
object Matrix3I {
  /**
    * Initializes rotation matrix using forward, up and right vector.
    *
    * @param forward forward vector
    * @param up      up vector
    * @param right   right vector
    * @return rotation matrix
    */
  def initRotationMatrix(forward: VectorI, up: VectorI, right: VectorI): MatrixI = {
    new MatrixI(Array(right.asArray, up.asArray, forward.asArray))
  }

  /**
    * Initializes a rotation matrix.
    *
    * @param a     first argument.
    * @param b     second argument.
    * @param angle degrees to rotate in objects multiplied by this rotation matrix.
    * @return plane rotation matrix.
    */
  def initPlaneRotation(a: Int, b: Int, angle: Int): MatrixI = {
    val ai = a - 1
    val bi = b - 1
    val matrix = Array.ofDim[Int](4, 4)
    for (x <- matrix.indices) {
      for (y <- matrix(0).indices) {
        val value = (y, x) match {
          case (`ai`, `ai`) => FastMath.cos(FastMath.toRadians(angle)).toInt
          case (`bi`, `bi`) => FastMath.cos(FastMath.toRadians(angle)).toInt
          case (`ai`, `bi`) => -FastMath.sin(FastMath.toRadians(angle)).toInt
          case (`bi`, `ai`) => FastMath.sin(FastMath.toRadians(angle)).toInt
          case _ => {
            if (x == y) {
              1
            } else {
              0
            }
          }
        }
        matrix(y)(x) = value
      }
    }
    MatrixI(matrix)
  }

  /**
    * Initializes rotation matrix using degree rotation in x, y & z direction.
    *
    * @param x degree rotation in x direction
    * @param y degree rotation in y direction
    * @param z degree rotation in z direction
    * @return rotation matrix
    */
  def initRotationMatrix(x: Int, y: Int, z: Int): MatrixI = {
    val rx = Matrix3I.initPlaneRotation(2, 3, x)
    val ry = Matrix3I.initPlaneRotation(3, 1, y)
    val rz = Matrix3I.initPlaneRotation(1, 2, z)
    MatrixI((rz * ry * rx).asArray)
  }

  /**
    * Initializes a new 3x3 scaling matrix.
    *
    * @param scale scale.
    * @return scale matrix.
    */
  def initScalingMatrix(scale: Array[Int]): MatrixI = {
    assert(scale.length == 3, "Translation must have 3 values!")
    new MatrixI(Array.tabulate[Int](3, 3)((x, y) => {
      if (x == y) {
        scale(x)
      } else {
        0
      }
    }))
  }

  /**
    * Initializes a new 3x3 translation matrix.
    *
    * @param location relative location.
    * @return translation matrix.
    */
  def initTranslationMatrix(location: Array[Int]): MatrixI = {
    assert(location.length == 2, "Translation must have 2 coordinates!")
    new MatrixI(Array.tabulate[Int](3, 3)((x,
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
    * Initializes a new 3x3 identity matrix.
    *
    * @return identity matrix.
    */
  def initIdentityMatrix(): MatrixI = {
    new MatrixI(Array.tabulate[Int](3, 3)((x, y) => {
      if (x == y) {
        1
      } else {
        0
      }
    }))
  }
}

/**
  * Utility object containing initializers for basic 4x4 matrices.
  * These functions should be used instead of any provided by {@link MatrixI}
  * wherever possible as they are supposed to perform faster.
  *
  * @author Caellian
  */
object Matrix4I {
  /**
    * Initializes perspective transformation matrix.
    *
    * @param fov         field of view.
    * @param aspectRatio aspect ration.
    * @param clipNear    front clipping position.
    * @param clipFar     back clipping position.
    * @return perspective transformation matrix.
    */
  def initPerspectiveMatrix(fov: Int, aspectRatio: Int, clipNear: Int, clipFar: Int): MatrixI = {
    val result = Array.ofDim[Int](4, 4)
    val fowAngle: Int = FastMath.tan(fov / 2).toInt
    val clipRange: Int = clipNear - clipFar
    result(0)(0) = (1.0f / (fowAngle * aspectRatio)).toInt
    result(0)(1) = 0
    result(0)(2) = 0
    result(0)(3) = 0
    result(1)(0) = 0
    result(1)(1) = (1.0f / fowAngle).toInt
    result(1)(2) = 0
    result(1)(3) = 0
    result(2)(0) = 0
    result(2)(1) = 0
    result(2)(2) = (-clipNear - clipFar) / clipRange
    result(2)(3) = 2 * clipFar * clipNear / clipRange
    result(3)(0) = 0
    result(3)(1) = 0
    result(3)(2) = 1
    result(3)(3) = 0
    new MatrixI(result)
  }

  /**
    * Initializes orthographic transformation matrix.
    *
    * @param left     left clipping position.
    * @param right    right clipping position.
    * @param bottom   bottom clipping position.
    * @param top      top clipping position.
    * @param clipNear front clipping position.
    * @param clipFar  back clipping position.
    * @return orthographic transformation matrix
    */
  def initOrthographicMatrix(left: Int,
                             right: Int,
                             bottom: Int,
                             top: Int,
                             clipNear: Int,
                             clipFar: Int): MatrixI = {
    val result = Array.ofDim[Int](4, 4)
    val width: Int = right - left
    val height: Int = top - bottom
    val depth: Int = clipFar - clipNear
    result(0)(0) = 2 / width
    result(0)(1) = 0
    result(0)(2) = 0
    result(0)(3) = -(right + left) / width
    result(1)(0) = 0
    result(1)(1) = 2 / height
    result(1)(2) = 0
    result(1)(3) = -(top + bottom) / height
    result(2)(0) = 0
    result(2)(1) = 0
    result(2)(2) = -2 / depth
    result(2)(3) = -(clipFar + clipNear) / depth
    result(3)(0) = 0
    result(3)(1) = 0
    result(3)(2) = 0
    result(3)(3) = 1
    new MatrixI(result)
  }

  /**
    * Initializes rotation matrix using forward and up vector by calculating
    * right vector.
    *
    * @param forward forward 3f vector.
    * @param up      up 3f vector.
    * @return rotation matrix.
    */
  def initRotationMatrix(forward: VectorI, up: VectorI): MatrixI = {
    val f = forward.normalized
    val r = up.normalized.cross(f)
    val u = f.cross(r)
    Matrix4I.initRotationMatrix(f, u, r)
  }

  /**
    * Initializes rotation matrix using forward, up and right vector.
    *
    * @param forward forward 3f vector.
    * @param up      up 3f vector.
    * @param right   right 3f vector.
    * @return rotation matrix.
    */
  def initRotationMatrix(forward: VectorI, up: VectorI, right: VectorI): MatrixI = {
    val result = Array.ofDim[Int](4, 4)
    result(0)(0) = right(X)
    result(0)(1) = right(Y)
    result(0)(2) = right(Z)
    result(0)(3) = 0
    result(1)(0) = up(X)
    result(1)(1) = up(Y)
    result(1)(2) = up(Y)
    result(1)(3) = 0
    result(2)(0) = forward(X)
    result(2)(1) = forward(Y)
    result(2)(2) = forward(Z)
    result(2)(3) = 0
    result(3)(0) = 0
    result(3)(1) = 0
    result(3)(2) = 0
    result(3)(3) = 1
    new MatrixI(result)
  }

  /**
    * Initializes rotation matrix using a rotation quaternion.
    *
    * @param quaternion quaternion to use for initialization.
    * @return rotation matrix.
    */
  def initRotationMatrix(quaternion: VectorI): MatrixI = {
    val forward = VectorI(2 * (quaternion(0) * quaternion(2) - quaternion(3) * quaternion(1)),
      2 * (quaternion(1) * quaternion(2) + quaternion(3) * quaternion(0)),
      1 - 2 * (quaternion(0) * quaternion(0) + quaternion(1) * quaternion(1)))
    val up = VectorI(2 * (quaternion(0) * quaternion(1) + quaternion(3) * quaternion(2)),
      1 - 2 * (quaternion(0) * quaternion(0) + quaternion(2) * quaternion(2)),
      2 * (quaternion(1) * quaternion(2) - quaternion(3) * quaternion(0)))
    val right = VectorI(1 - 2 * (quaternion(1) * quaternion(1) + quaternion(2) * quaternion(2)),
      2 * (quaternion(0) * quaternion(1) - quaternion(3) * quaternion(2)),
      2 * (quaternion(0) * quaternion(2) + quaternion(3) * quaternion(1)))

    Matrix4I.initRotationMatrix(forward,up,right)
  }

  /**
    * Initializes a rotation matrix.
    *
    * @param a     first argument.
    * @param b     second argument.
    * @param angle degrees to rotate in objects multiplied by this rotation matrix.
    * @return plane rotation matrix.
    */
  def initPlaneRotation(a: Int, b: Int, angle: Int): MatrixI = {
    val ai = a - 1
    val bi = b - 1
    val matrix = Array.ofDim[Int](4, 4)
    for (x <- matrix.indices) {
      for (y <- matrix(0).indices) {
        val value = (y, x) match {
          case (`ai`, `ai`) => FastMath.cos(FastMath.toRadians(angle)).toInt
          case (`bi`, `bi`) => FastMath.cos(FastMath.toRadians(angle)).toInt
          case (`ai`, `bi`) => -FastMath.sin(FastMath.toRadians(angle)).toInt
          case (`bi`, `ai`) => FastMath.sin(FastMath.toRadians(angle)).toInt
          case _ => {
            if (x == y) {
              1
            } else {
              0
            }
          }
        }
        matrix(y)(x) = value
      }
    }
    MatrixI(matrix)
  }

  /**
    * Initializes a new 4x4 scaling matrix.
    *
    * @param scale scale.
    * @return scale matrix.
    */
  def initScalingMatrix(scale: Array[Int]): MatrixI = {
    assert(scale.length == 4, "Translation must have 4 values!")
    new MatrixI(Array.tabulate[Int](4, 4)((x, y) => {
      if (x == y) {
        scale(x)
      } else {
        0
      }
    }))
  }

  /**
    * Initializes a new 4x4 translation matrix.
    *
    * @param location relative location.
    * @return translation matrix.
    */
  def initTranslationMatrix(location: Array[Int]): MatrixI = {
    assert(location.length == 3, "Translation must have 3 coordinates!")
    new MatrixI(Array.tabulate[Int](4, 4)((x,
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
    * Initializes a new 4x4 identity matrix.
    *
    * @return identity matrix.
    */
  def initIdentityMatrix(): MatrixI = {
    new MatrixI(Array.tabulate[Int](4, 4)((x, y) => {
      if (x == y) {
        1
      } else {
        0
      }
    }))
  }
}
