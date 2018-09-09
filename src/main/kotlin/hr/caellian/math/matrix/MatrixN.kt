package hr.caellian.math.matrix

import hr.caellian.math.internal.Inverse

/**
 * Abstract Matrix class defining number matrix specific behaviour.
 *
 * @since 3.0.0
 * @author Caellian
 */
abstract class MatrixN<T : Number> : Matrix<T>() {
    /**
     * @return inverse matrix.
     */
    override fun inverse(singularityThreshold: Double): Matrix<T> {
        @Suppress("UNCHECKED_CAST")
        return withData(
                Inverse.inverseMatrix(toArray() as Array<Array<Double>>, singularityThreshold) as Array<Array<T>>)
    }

    /**
     * This method replaces data of this matrix with LU decomposition data.
     *
     * @return self.
     */
    override fun inverseUnsafe(singularityThreshold: Double): Matrix<T> {
        @Suppress("UNCHECKED_CAST")
        return withData(Inverse.inverseMatrix(wrapped as Array<Array<Double>>, singularityThreshold) as Array<Array<T>>)
    }
}