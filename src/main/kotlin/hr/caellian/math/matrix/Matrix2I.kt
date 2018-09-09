package hr.caellian.math.matrix

import kotlin.math.cos
import kotlin.math.roundToInt
import kotlin.math.sin

/**
 * Utility object containing initializers for basic 2x2 matrices.
 * These functions should be used instead of any provided by [MatrixI] wherever possible as they generally perform faster.
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
    @JvmStatic
    fun initRotation(degrees: Int): MatrixI {
        if (degrees == 0) {
            return MatrixI.initIdentityMatrix(2)
        }

        return MatrixI(arrayOf(
                arrayOf(cos(Math.toRadians(degrees.toDouble())).roundToInt(),
                        sin(Math.toRadians(degrees.toDouble())).roundToInt()),
                arrayOf(-sin(Math.toRadians(degrees.toDouble())).roundToInt(),
                        cos(Math.toRadians(degrees.toDouble())).roundToInt())
        ))
    }
}