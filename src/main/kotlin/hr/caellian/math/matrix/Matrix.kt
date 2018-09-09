package hr.caellian.math.matrix

import hr.caellian.math.internal.BufferConstructor
import hr.caellian.math.internal.DataWrapper
import hr.caellian.math.internal.Replicable
import hr.caellian.math.vector.Vector
import java.util.*

/**
 * Abstract Matrix class defined for stable code infrastructure and consistency.
 * Most basic Matrix variables and functions can be found here.
 *
 * @author Caellian
 */
abstract class Matrix<T> : Replicable<Matrix<T>>, DataWrapper<Matrix<T>, Array<Array<T>>>, BufferConstructor {
    /**
     * @return number of rows in this matrix.
     */
    val rowCount: Int
        get() = wrapped.size

    /**
     * @return number of columns in this matrix.
     */
    val columnCount: Int
        get() = wrapped[0].size

    /**
     * @return true if this is square matrix.
     */
    val isSquare: Boolean
        get() = rowCount == columnCount

    /**
     * @param row row column entries to return
     *
     * @return all column entries at argument row
     */
    operator fun get(row: Int): Array<T> = wrapped[row]

    /**
     * @return this matrix.
     */
    operator fun unaryPlus() = this

    /**
     * @return new matrix with negated values.
     */
    abstract operator fun unaryMinus(): Matrix<T>

    /**
     * Performs matrix addition and returns resulting matrix.
     * In order to add to matrices together, they must be of same size.
     *
     * @param other matrix to add to this one.
     * @return resulting of matrix addition.
     */
    abstract operator fun plus(other: Matrix<T>): Matrix<T>

    /**
     * Performs matrix subtraction and returns resulting matrix.
     * In order to subtract one matrix from another, matrices must be of same size.
     *
     * @param other matrix to subtract from this one.
     * @return resulting of matrix subtraction.
     */
    abstract operator fun minus(other: Matrix<T>): Matrix<T>

    /**
     * Performs matrix multiplication on this matrix.
     * Returns C from 'C = A×B' where A is this matrix and B is the other / argument matrix.
     *
     * @param other matrix to multiply this matrix with.
     * @return result of matrix multiplication.
     */
    abstract operator fun times(other: Matrix<T>): Matrix<T>

    /**
     * Performs matrix multiplication on this matrix.
     * Returns C from 'C = A×B' where A is this matrix and B is the other / argument vector.
     *
     * @param other vector to multiply this matrix with.
     * @return result of matrix multiplication.
     */
    abstract operator fun times(other: Vector<T>): Vector<T>

    /**
     * Performs scalar multiplication on this matrix and returns resulting matrix.
     *
     * @param scalar scalar to multiply every member of this matrix with.
     * @return result of scalar matrix multiplication.
     */
    abstract operator fun times(scalar: T): Matrix<T>

    /**
     * @return inverse matrix.
     */
    operator fun not(): Matrix<T> = inverse()

    /**
     * @return inverse matrix.
     */
    abstract fun inverse(singularityThreshold: Double = 1e-11): Matrix<T>

    /**
     * This method replaces data of this matrix with LU decomposition data.
     *
     * @return self.
     */
    abstract fun inverseUnsafe(singularityThreshold: Double = 1e-11): Matrix<T>

    /**
     * Switches two rows together.
     *
     * @param rowA row to be switched with rowB.
     * @param rowB row to be switched with rowA.
     * @return resulting matrix.
     */
    abstract fun switchRows(rowA: Int, rowB: Int): Matrix<T>

    /**
     * Switches two columns together.
     *
     * @param columnA column to be switched with columnB.
     * @param columnB column to be switched with columnA.
     * @return resulting matrix.
     *
     * @since 3.0.0
     */
    abstract fun switchColumns(columnA: Int, columnB: Int): Matrix<T>

    /**
     * Multiplies all entries of a row with given scalar.
     *
     * @param row        row to multiply.
     * @param multiplier scalar to multiply rows entries with.
     * @return resulting matrix.
     */
    abstract fun multiplyRow(row: Int, multiplier: T): Matrix<T>

    /**
     * Multiplies all entries of a column with given scalar.
     *
     * @param column        column to multiply.
     * @param multiplier scalar to multiply column entries with.
     * @return resulting matrix.
     */
    abstract fun multiplyColumn(column: Int, multiplier: T): Matrix<T>

    /**
     * Adds one row from matrix to another.
     *
     * @param from       row to add to another row.
     * @param to         row to add another row to; data will be stored on this row.
     * @param multiplier scalar to multiply all members of added row with on addition. It equals to 1 by default.
     * @return new matrix.
     */
    abstract fun addRows(from: Int, to: Int, multiplier: T? = null): Matrix<T>

    /**
     * Adds one column from matrix to another.
     *
     * @param from       column to add to another column.
     * @param to         column to add another column to; data will be stored on this column.
     * @param multiplier scalar to multiply all members of added column with on addition. It equals to 1 by default.
     * @return new matrix.
     */
    abstract fun addColumns(from: Int, to: Int, multiplier: T? = null): Matrix<T>

    /**
     * Inserts given row data at given index shifting rest of the matrix to the next index.
     *
     * @param index index at which added row data will be stored.
     * @param data  row data to store at given index.
     * @return new matrix with extended data.
     */
    abstract fun withRow(index: Int, data: Array<T>): Matrix<T>

    /**
     * Inserts given column data at given index shifting rest of the matrix to the next index.
     *
     * @param index index at which added column data will be stored.
     * @param data  column data to store at given index.
     * @return new matrix with extended data.
     */
    abstract fun withColumn(index: Int, data: Array<T>): Matrix<T>

    /**
     * Creates a new matrix without specified rows & columns.
     *
     * @param deletedRows    rows to exclude from submatrix.
     * @param deletedColumns columns to exclude from submatrix.
     * @return funined submatrix.
     */
    abstract fun submatrix(deletedRows: Array<Int>, deletedColumns: Array<Int>): Matrix<T>

    /**
     * Constructs a new vector out of column / row vector matrix.
     *
     * @return vector containing matrix data.
     */
    abstract fun toVector(): Vector<T>

    /**
     * Constructs a new vector out of any matrix dismissing extra data.
     *
     * @return vector containing only first column of matrix data.
     */
    abstract fun forceToVector(): Vector<T>

    /**
     * @return a new matrix containing only the first row of this matrix.
     */
    abstract fun firstRow(): Matrix<T>

    /**
     * @return a new matrix containing only the first column of this matrix.
     */
    abstract fun firstColumn(): Matrix<T>

    /**
     * @return transposed matrix.
     */
    abstract fun transpose(): Matrix<T>

    /**
     * @return true if matrix is valid
     */
    fun validate(): Boolean = wrapped.all { it.size == wrapped[0].size }

    /**
     * Returns copy of data of this Matrix as a 2D array.
     *
     * Default implementation:
     * <code>
     *     return Array(rowCount) { row -> Array(columnCount) { column -> wrapped[row][column] }}
     * </code>
     *
     * @return 2D array containing data of this matrix.
     */
    abstract fun toArray(): Array<Array<T>>

    /**
     * @return clone of this matrix.
     */
    abstract override fun replicated(): Matrix<T>

    /**
     * @param other other matrix or object instance of type extending matrix.
     * @return true if this matrix is equal to other matrix.
     */
    override fun equals(other: Any?): Boolean {
        if (other == null) {
            return false
        }
        if (this === other) {
            return true
        }
        if (other !is Matrix<*>) {
            return false
        }
        return wrapped contentDeepEquals other.wrapped
    }

    /**
     * Matrix hashcode depends on matrix data and will change if matrix data is modified!
     *
     * @return hashcode of this matrix.
     */
    override fun hashCode(): Int = Arrays.deepHashCode(wrapped)

    /**
     * @return string representation of this matrix.
     */
    override fun toString(): String = "{${wrapped.joinToString(",\n") { "[${it.joinToString(", ")}]" }}}"
}