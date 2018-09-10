package hr.caellian.math.matrix

import hr.caellian.math.internal.BufferConstructor
import hr.caellian.math.internal.DataWrapper
import hr.caellian.math.internal.Replicable
import hr.caellian.math.internal.TypeErasable
import hr.caellian.math.vector.Vector
import java.util.*

/**
 * Abstract Matrix class defined for stable code infrastructure and consistency.
 * Most basic Matrix variables and functions can be found here.
 *
 * @author Caellian
 */
abstract class Matrix<T : Any> : Replicable<Matrix<T>>, DataWrapper<Matrix<T>, Array<Array<T>>>, TypeErasable<T>, Iterable<T>, BufferConstructor {
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
     * @param row row column entries to return
     *
     * @return entry at argument row and column
     */
    operator fun get(row: Int, column: Int): T = wrapped[row][column]

    /**
     * Behaviour depends on implementation.
     *
     * @see MatrixN.not
     * @see MatrixB.not
     */
    abstract fun not(): Matrix<T>

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
     * Returns an iterator over the elements of this object.
     */
    override fun iterator() = MatrixIterator(this)

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

    /**
     * Custom matrix iterator class.
     *
     * @since 3.0.0
     */
    class MatrixIterator<T: Any>(private val parent: Matrix<T>): Iterator<T> {
        var row = 0
        var column = 0

        override fun hasNext() = row < parent.rowCount && column < parent.columnCount

        /**
         * Returns the next element in the iteration.
         */
        override fun next(): T {
            if (row >= parent.rowCount) {
                row = 0
                column++
            }

            return parent.wrapped[row++][column]
        }

        /**
         * Resets the iterator allowing it to be used again to reduce garbage.
         */
        fun reset() {
            row = 0
            column = 0
        }
    }
}