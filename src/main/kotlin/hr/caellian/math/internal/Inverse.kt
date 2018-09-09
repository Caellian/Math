package hr.caellian.math.internal

/**
 * You don't need to interact with this class in most cases as it's handled by Matrix
 * classes automatically.
 *
 * This class is based on Apache Math LUDecomposition class but is optimised to work
 * better with this library as it doesn't calculate data this library doesn't use.
 *
 * @since 2.0.0
 * @author Caellian
 */
object Inverse {
    /**
     * Java implementation of Doolittle LUP matrix decomposition algorithm.
     *
     * @param lu input matrix which will turn into LU data matrix.
     * @param singularityThreshold singularity threshold. This should be a very low number.
     * @return Pivot decomposition data.
     */
    @JvmStatic
    fun doolittleLUP(lu: Array<Array<Double>>, singularityThreshold: Double): Array<Int> {
        if (lu.size != lu[0].size) {
            throw IllegalArgumentException("LU decomposition of non-square matrices not supported!")
        }

        val n = lu.size

        // Pivot
        val p = Array(n) { it }

        // Iterate over columns
        for (col in 0 until n) {

            // Upper matrix construction
            for (row in 0 until col) {
                val luRow = lu[row]
                var sum = luRow[col]
                for (i in 0 until row) {
                    sum -= luRow[i] * lu[i][col]
                }
                luRow[col] = sum
            }

            // Lower matrix construction
            var max = col // Permutation row
            var largest = 0.0
            for (row in col until n) {
                val luRow = lu[row]
                var sum = luRow[col]
                for (i in 0 until col) {
                    sum -= luRow[i] * lu[i][col]
                }
                luRow[col] = sum

                // Maintain best permutation choice
                if (Math.abs(sum) > largest) {
                    largest = Math.abs(sum)
                    max = row
                }
            }

            // Singularity check
            if (Math.abs(lu[max][col]) < singularityThreshold) {
                throw IllegalArgumentException("LUP Decomposition impossible for singular matrices!")
            }

            // Pivot if necessary
            if (max != col) {
                val luMax = lu[max]
                val luCol = lu[col]

                var tmp: Double
                for (i in 0 until n) {
                    tmp = luMax[i]
                    luMax[i] = luCol[i]
                    luCol[i] = tmp
                }

                val temp = p[max]
                p[max] = p[col]
                p[col] = temp
            }

            // Divide the lower elements by the "winning" diagonal elt.
            val luDiagonal = lu[col][col]
            for (row in col + 1 until n) {
                lu[row][col] /= luDiagonal
            }
        }

        return p
    }

    /**
     * Calculates inverse matrix of input matrix. Unwrapped matrix format is used to
     * increase performance.
     *
     * @param lu input matrix which will turn into LU data matrix.
     * @param singularityThreshold singularity threshold. This should be a very low number.
     * @return inverse matrix of given matrix.
     */
    @JvmStatic
    fun inverseMatrix(lu: Array<Array<Double>>, singularityThreshold: Double): Array<Array<Double>> {
        // Decomposition pivot
        val p = doolittleLUP(lu, singularityThreshold)

        // Size of decomposed matrix
        val n = lu.size

        val b = Array(n) { _ -> Array(n) { 0.0 } }

        for (row in 0 until n) {
            val bRow = b[row]
            val pRow = p[row]
            for (col in 0 until n) {
                bRow[col] = (if (pRow == col) 1 else 0).toDouble()
            }
        }

        // Solve LY = b
        for (col in 0 until n) {
            val bpCol = b[col]
            for (i in col + 1 until n) {
                val bpI = b[i]
                val luICol = lu[i][col]
                for (j in 0 until n) {
                    bpI[j] -= bpCol[j] * luICol
                }
            }
        }

        // Solve UX = Y
        for (col in n - 1 downTo 0) {
            val bpCol = b[col]
            val luDiag = lu[col][col]
            for (j in 0 until n) {
                bpCol[j] /= luDiag
            }
            for (i in 0 until col) {
                val bpI = b[i]
                val luICol = lu[i][col]
                for (j in 0 until n) {
                    bpI[j] -= bpCol[j] * luICol
                }
            }
        }

        return b
    }
}
