package hr.caellian.math.matrix

import kotlin.math.cos
import kotlin.math.sin

/**
 * Utility object containing initializers for basic 2x2 matrices.
 * These functions should be used instead of any provided by [MatrixF] wherever possible as they generally perform faster.
 *
 * @author Caellian
 */
object Matrix2F {
    /**
     * Initializes rotation matrix using degrees.
     *
     * @param degrees Degrees to rotate objects multiplied by this matrix in positive direction.
     * @return rotation matrix
     */
    @JvmStatic
    fun initRotation(degrees: Float): MatrixF {
        if (degrees == 0f) {
            return MatrixF.initIdentityMatrix(2)
        }

        return MatrixF(arrayOf(
                arrayOf(cos(Math.toRadians(degrees.toDouble())).toFloat(),
                        sin(Math.toRadians(degrees.toDouble())).toFloat()),
                arrayOf(-sin(Math.toRadians(degrees.toDouble())).toFloat(),
                        cos(Math.toRadians(degrees.toDouble())).toFloat())
        ))
    }
}