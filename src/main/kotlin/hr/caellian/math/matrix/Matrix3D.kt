package hr.caellian.math.matrix

import hr.caellian.math.vector.VectorD

/**
 * Utility object containing initializers for basic 3x3 matrices.
 * These functions should be used instead of any provided by [MatrixD] wherever possible as they generally perform faster.
 *
 * @author Caellian
 */
object Matrix3D {
    /**
     * Initializes rotation matrix using forward, up and right vector.
     *
     * @param forward forward vector
     * @param up      up vector
     * @param right   right vector
     * @return rotation matrix
     */
    @JvmStatic
    fun initRotationMatrix(forward: VectorD, up: VectorD, right: VectorD): MatrixD {
        return MatrixD(arrayOf(right.toArray(), up.toArray(), forward.toArray()))
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
    fun initRotationMatrix(x: Double, y: Double, z: Double): MatrixD {
        val rx = MatrixD.initPlaneRotation(4, 2, 3, x)
        val ry = MatrixD.initPlaneRotation(4, 3, 1, y)
        val rz = MatrixD.initPlaneRotation(4, 1, 2, z)
        return MatrixD((rz * ry * rx).wrapped)
    }
}
