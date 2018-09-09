package hr.caellian.math.matrix

import hr.caellian.math.vector.VectorF

/**
 * Utility object containing initializers for basic 3x3 matrices.
 * These functions should be used instead of any provided by [MatrixF] wherever possible as they generally perform faster.
 *
 * @author Caellian
 */
object Matrix3F {
    /**
     * Initializes rotation matrix using forward, up and right vector.
     *
     * @param forward forward vector
     * @param up      up vector
     * @param right   right vector
     * @return rotation matrix
     */
    @JvmStatic
    fun initRotationMatrix(forward: VectorF, up: VectorF, right: VectorF): MatrixF {
        return MatrixF(arrayOf(right.toArray(), up.toArray(), forward.toArray()))
    }

    /**
     * Initializes rotation matrix using degree rotation in x, y & z direction.
     *
     * @param x degree rotation in x direction
     * @param y degree rotation in y direction
     * @param z degree rotation in z direction
     * @return rotation matrix
     */
    @JvmStatic
    fun initRotationMatrix(x: Float, y: Float, z: Float): MatrixF {
        val rx = MatrixF.initPlaneRotation(4, 2, 3, x)
        val ry = MatrixF.initPlaneRotation(4, 3, 1, y)
        val rz = MatrixF.initPlaneRotation(4, 1, 2, z)
        return MatrixF((rz * ry * rx).wrapped)
    }
}
