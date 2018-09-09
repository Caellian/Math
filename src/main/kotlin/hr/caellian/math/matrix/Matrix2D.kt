package hr.caellian.math.matrix

import kotlin.math.cos
import kotlin.math.sin

/**
 * Utility object containing initializers for basic 2x2 matrices.
 * These functions should be used instead of any provided by [MatrixD] wherever possible as they generally perform faster.
 *
 * @author Caellian
 */
object Matrix2D {
    /**
     * Initializes rotation matrix using degrees.
     *
     * @param degrees Degrees to rotate objects multiplied by this matrix in positive direction.
     * @return rotation matrix
     */
    @JvmStatic
    fun initRotation(degrees: Double): MatrixD {
        if (degrees == 0.0) {
            return MatrixD.initIdentityMatrix(2)
        }

        return MatrixD(arrayOf(
                arrayOf(cos(Math.toRadians(degrees)),
                        sin(Math.toRadians(degrees))),
                arrayOf(-sin(Math.toRadians(degrees)),
                        cos(Math.toRadians(degrees)))
        ))
    }
}