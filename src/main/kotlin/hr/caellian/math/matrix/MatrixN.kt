package hr.caellian.math.matrix

import hr.caellian.math.internal.Inverse
import hr.caellian.math.vector.Vector
import hr.caellian.math.vector.VectorN

/**
 * Abstract Matrix class defining number matrix specific behaviour.
 *
 * @since 3.0.0
 * @author Caellian
 */
abstract class MatrixN<T : Number> : Matrix<T>() {
    /**
     * @return this matrix.
     */
    operator fun unaryPlus() = this

    /**
     * @return new matrix with negated values.
     */
    abstract operator fun unaryMinus(): MatrixN<T>

    /**
     * Performs matrix addition and returns resulting matrix.
     * In order to add to matrices together, they must be of same size.
     *
     * @param other matrix to add to this one.
     * @return resulting of matrix addition.
     */
    abstract operator fun plus(other: MatrixN<T>): MatrixN<T>

    /**
     * Performs matrix subtraction and returns resulting matrix.
     * In order to subtract one matrix from another, matrices must be of same size.
     *
     * @param other matrix to subtract from this one.
     * @return resulting of matrix subtraction.
     */
    abstract operator fun minus(other: MatrixN<T>): MatrixN<T>

    /**
     * Performs matrix multiplication on this matrix.
     * Returns C from 'C = A×B' where A is this matrix and B is the other / argument matrix.
     *
     * @param other matrix to multiply this matrix with.
     * @return result of matrix multiplication.
     */
    abstract operator fun times(other: MatrixN<T>): MatrixN<T>

    /**
     * Performs matrix multiplication on this matrix.
     * Returns C from 'C = A×B' where A is this matrix and B is the other / argument vector.
     *
     * @param other vector to multiply this matrix with.
     * @return result of matrix multiplication.
     */
    abstract operator fun times(other: VectorN<T>): Vector<T>

    /**
     * Performs scalar multiplication on this matrix and returns resulting matrix.
     *
     * @param scalar scalar to multiply every member of this matrix with.
     * @return result of scalar matrix multiplication.
     */
    abstract operator fun times(scalar: T): MatrixN<T>

    /**
     * @return inverse matrix.
     */
    override operator fun not(): Matrix<T> = inverse()

    /**
     * Multiplies all entries of a row with given scalar.
     *
     * @param row        row to multiply.
     * @param multiplier scalar to multiply rows entries with.
     * @return resulting matrix.
     */
    abstract fun multiplyRow(row: Int, multiplier: T): MatrixN<T>

    /**
     * Multiplies all entries of a column with given scalar.
     *
     * @param column        column to multiply.
     * @param multiplier scalar to multiply column entries with.
     * @return resulting matrix.
     */
    abstract fun multiplyColumn(column: Int, multiplier: T): MatrixN<T>

    /**
     * Adds one row from matrix to another.
     *
     * @param from       row to add to another row.
     * @param to         row to add another row to; data will be stored on this row.
     * @param multiplier scalar to multiply all members of added row with on addition. It equals to 1 by default.
     * @return new matrix.
     */
    abstract fun addRows(from: Int, to: Int, multiplier: T? = null): MatrixN<T>

    /**
     * Adds one column from matrix to another.
     *
     * @param from       column to add to another column.
     * @param to         column to add another column to; data will be stored on this column.
     * @param multiplier scalar to multiply all members of added column with on addition. It equals to 1 by default.
     * @return new matrix.
     */
    abstract fun addColumns(from: Int, to: Int, multiplier: T? = null): MatrixN<T>

    /**
     * @return inverse matrix.
     */
    fun inverse(singularityThreshold: Double = 1e-11): MatrixN<T> {
        @Suppress("UNCHECKED_CAST")
        return withData(
                Inverse.inverseMatrix(toArray() as Array<Array<Double>>,
                                      singularityThreshold) as Array<Array<T>>) as MatrixN<T>
    }

    /**
     * This method replaces data of this matrix with LU decomposition data.
     *
     * @return self.
     */
    fun inverseUnsafe(singularityThreshold: Double = 1e-11): MatrixN<T> {
        @Suppress("UNCHECKED_CAST")
        return withData(Inverse.inverseMatrix(wrapped as Array<Array<Double>>,
                                              singularityThreshold) as Array<Array<T>>) as MatrixN<T>
    }
}