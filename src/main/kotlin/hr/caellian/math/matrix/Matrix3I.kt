package hr.caellian.math.matrix

import hr.caellian.math.vector.VectorI

/**
 * Utility object containing initializers for basic 3x3 matrices.
 * These functions should be used instead of any provided by [MatrixI] wherever possible as they generally perform faster.
 *
 * @author Caellian
 */
object Matrix3I {
    /**
     * Initializes rotation matrix using forward, up and right vector.
     *
     * @param forward forward vector
     * @param up      up vector
     * @param right   right vector
     * @return rotation matrix
     */
    @JvmStatic
    fun initRotationMatrix(forward: VectorI, up: VectorI, right: VectorI): MatrixI {
        return MatrixI(arrayOf(right.toArray(), up.toArray(), forward.toArray()))
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
    fun initRotationMatrix(x: Int, y: Int, z: Int): MatrixI {
        val rx = MatrixI.initPlaneRotation(4, 2, 3, x)
        val ry = MatrixI.initPlaneRotation(4, 3, 1, y)
        val rz = MatrixI.initPlaneRotation(4, 1, 2, z)
        return MatrixI((rz * ry * rx).wrapped)
    }
}
