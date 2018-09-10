package hr.caellian.math.matrix

import hr.caellian.math.vector.VectorD
import kotlin.math.tan

/**
 * Utility object containing initializers for basic 4x4 matrices.
 * These functions should be used instead of any provided by [MatrixD] wherever possible as they generally perform faster.
 *
 * @author Caellian
 */
object Matrix4D {
    /**
     * Initializes perspective transformation matrix.
     *
     * @param fov         field of view.
     * @param aspectRatio aspect ration.
     * @param clipNear    front clipping position.
     * @param clipFar     back clipping position.
     * @return perspective transformation matrix.
     */
    @JvmStatic
    fun initPerspectiveMatrix(fov: Double, aspectRatio: Double, clipNear: Double, clipFar: Double): MatrixD {
        val fowAngle = tan(fov / 2)
        val clipRange = clipNear - clipFar

        return MatrixD(Array(4) { row ->
            Array(4) { column ->
                when {
                    row == 0 && column == 0 -> 1.0 / (fowAngle * aspectRatio)
                    row == 1 && column == 1 -> 1.0 / fowAngle
                    row == 2 && column == 2 -> (-clipNear - clipFar) / clipRange
                    row == 2 && column == 3 -> 2 * clipFar * clipNear / clipRange
                    row == 3 && column == 2 -> 1.0
                    else -> 0.0
                }
            }
        })
    }

    /**
     * Initializes orthographic transformation matrix.
     *
     * @param left     left clipping position.
     * @param right    right clipping position.
     * @param bottom   bottom clipping position.
     * @param top      top clipping position.
     * @param clipNear front clipping position.
     * @param clipFar  back clipping position.
     * @return orthographic transformation matrix
     */
    @JvmStatic
    fun initOrthographicMatrix(left: Double, right: Double, bottom: Double, top: Double, clipNear: Double, clipFar: Double): MatrixD {
        val width = right - left
        val height = top - bottom
        val depth = clipFar - clipNear

        return MatrixD(Array(4) { row ->
            Array(4) { column ->
                when {
                    row == 0 && column == 0 -> 2 / width
                    row == 0 && column == 3 -> -(right + left) / width
                    row == 1 && column == 1 -> 2 / height
                    row == 1 && column == 3 -> -(top + bottom) / height
                    row == 2 && column == 2 -> -2 / depth
                    row == 2 && column == 3 -> -(clipFar + clipNear) / depth
                    row == 3 && column == 3 -> 1.0
                    else -> 0.0
                }
            }
        })
    }

    /**
     * Initializes rotation matrix using forward and up vector by calculating
     * right vector.
     *
     * @param forward forward 3d vector.
     * @param up      up 3d vector.
     * @return rotation matrix.
     */
    @JvmStatic
    fun initRotationMatrix(forward: VectorD, up: VectorD): MatrixD {
        require(forward.size == 3) { "Invalid forward vector size (${forward.size}), expected size of 3!" }
        require(up.size == 3) { "Invalid up vector size (${up.size}), expected size of 3!" }

        val f = forward.normalized()
        val r = up.normalized().cross(f)
        val u = f.cross(r)
        return Matrix4D.initRotationMatrix(f, u, r)
    }

    /**
     * Initializes rotation matrix using a rotation quaternion.
     *
     * @param quaternion quaternion to use for initialization.
     * @return rotation matrix.
     */
    @JvmStatic
    fun initRotationMatrix(quaternion: VectorD): MatrixD {
        val forward = VectorD(2.0 * (quaternion[0] * quaternion[2] - quaternion[3] * quaternion[1]),
                              2.0 * (quaternion[1] * quaternion[2] + quaternion[3] * quaternion[0]),
                              1.0 - 2.0 * (quaternion[0] * quaternion[0] + quaternion[1] * quaternion[1]))
        val up = VectorD(2.0 * (quaternion[0] * quaternion[1] + quaternion[3] * quaternion[2]),
                         1.0 - 2.0 * (quaternion[0] * quaternion[0] + quaternion[2] * quaternion[2]),
                         2.0 * (quaternion[1] * quaternion[2] - quaternion[3] * quaternion[0]))
        val right = VectorD(1.0 - 2.0 * (quaternion[1] * quaternion[1] + quaternion[2] * quaternion[2]),
                            2.0 * (quaternion[0] * quaternion[1] - quaternion[3] * quaternion[2]),
                            2.0 * (quaternion[0] * quaternion[2] + quaternion[3] * quaternion[1]))

        return Matrix4D.initRotationMatrix(forward, up, right)
    }

    /**
     * Initializes rotation matrix using forward, up and right vector.
     *
     * @param forward forward 3d vector.
     * @param up      up 3d vector.
     * @param right   right 3d vector.
     * @return rotation matrix.
     */
    @JvmStatic
    fun initRotationMatrix(forward: VectorD, up: VectorD, right: VectorD): MatrixD {
        require(forward.size == 3) { "Invalid forward vector size (${forward.size}), expected size of 3!" }
        require(up.size == 3) { "Invalid up vector size (${up.size}), expected size of 3!" }
        require(right.size == 3) { "Invalid right vector size (${right.size}), expected size of 3!" }
        return MatrixD(Array(4) { row ->
            Array(4) { column ->
                when {
                    row == 0 && column != 3 -> right[column]
                    row == 1 && column != 3 -> up[column]
                    row == 2 && column != 3 -> forward[column]
                    row == 3 && column == 3 -> 1.0
                    else -> 0.0
                }
            }
        })
    }

    /**
     * Utility method that combines translation and rotation directly and returns world transformation matrix.
     *
     * @since 3.0.0
     *
     * @param eye camera position 3d vector.
     * @param center position to look at.
     * @param up up 3d vector.
     * @return world transformation matrix.
     */
    @JvmStatic
    fun lookAt(eye: VectorD, center: VectorD, up: VectorD): MatrixD {
        require(eye.size == 3) { "Invalid eye position vector size (${eye.size}), expected size of 3!" }
        require(center.size == 3) { "Invalid center position vector size (${center.size}), expected size of 3!" }
        require(up.size == 3) { "Invalid up vector size (${up.size}), expected size of 3!" }

        val forward = (eye - center).normalized()

        return MatrixD.initTranslationMatrix(eye - center) * initRotationMatrix(forward, up)
    }
}
