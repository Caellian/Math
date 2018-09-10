package hr.caellian.math.matrix

import hr.caellian.math.vector.VectorI
import kotlin.math.roundToInt
import kotlin.math.tan

/**
 * Utility object containing initializers for basic 4x4 matrices.
 * These functions should be used instead of any provided by [MatrixI] wherever possible as they generally perform faster.
 *
 * @author Caellian
 */
object Matrix4I {
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
    fun initPerspectiveMatrix(fov: Float, aspectRatio: Float, clipNear: Float, clipFar: Float): MatrixI {
        val fowAngle: Float = tan(fov / 2)
        val clipRange = clipNear - clipFar

        return MatrixI(Array(4) { row ->
            Array(4) { column ->
                when {
                    row == 0 && column == 0 -> (1f / (fowAngle * aspectRatio)).roundToInt()
                    row == 1 && column == 1 -> (1 / fowAngle).roundToInt()
                    row == 2 && column == 2 -> ((-clipNear - clipFar) / clipRange).roundToInt()
                    row == 2 && column == 3 -> (2 * clipFar * clipNear / clipRange).roundToInt()
                    row == 3 && column == 2 -> 1
                    else -> 0
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
    fun initOrthographicMatrix(left: Float, right: Float, bottom: Float, top: Float, clipNear: Float, clipFar: Float): MatrixI {
        val width = right - left
        val height = top - bottom
        val depth = clipFar - clipNear

        return MatrixI(Array(4) { row ->
            Array(4) { column ->
                when {
                    row == 0 && column == 0 -> (2 / width).roundToInt()
                    row == 0 && column == 3 -> (-(right + left) / width).roundToInt()
                    row == 1 && column == 1 -> (2 / height).roundToInt()
                    row == 1 && column == 3 -> (-(top + bottom) / height).roundToInt()
                    row == 2 && column == 2 -> (-2 / depth).roundToInt()
                    row == 2 && column == 3 -> (-(clipFar + clipNear) / depth).roundToInt()
                    row == 3 && column == 3 -> 1
                    else -> 0
                }
            }
        })
    }

    /**
     * Initializes rotation matrix using forward and up vector by calculating
     * right vector.
     *
     * @param forward forward 3i vector.
     * @param up      up 3i vector.
     * @return rotation matrix.
     */
    @JvmStatic
    fun initRotationMatrix(forward: VectorI, up: VectorI): MatrixI {
        require(forward.size == 3) { "Invalid forward vector size (${forward.size}), expected size of 3!" }
        require(up.size == 3) { "Invalid up vector size (${up.size}), expected size of 3!" }

        val f = forward.normalized()
        val r = up.normalized().cross(f)
        val u = f.cross(r)
        return Matrix4I.initRotationMatrix(f, u, r)
    }

    /**
     * Initializes rotation matrix using a rotation quaternion.
     *
     * @param quaternion quaternion to use for initialization.
     * @return rotation matrix.
     */
    @JvmStatic
    fun initRotationMatrix(quaternion: VectorI): MatrixI {
        val forward = VectorI(2 * (quaternion[0] * quaternion[2] - quaternion[3] * quaternion[1]),
                              2 * (quaternion[1] * quaternion[2] + quaternion[3] * quaternion[0]),
                              1 - 2 * (quaternion[0] * quaternion[0] + quaternion[1] * quaternion[1]))
        val up = VectorI(2 * (quaternion[0] * quaternion[1] + quaternion[3] * quaternion[2]),
                         1 - 2 * (quaternion[0] * quaternion[0] + quaternion[2] * quaternion[2]),
                         2 * (quaternion[1] * quaternion[2] - quaternion[3] * quaternion[0]))
        val right = VectorI(1 - 2 * (quaternion[1] * quaternion[1] + quaternion[2] * quaternion[2]),
                            2 * (quaternion[0] * quaternion[1] - quaternion[3] * quaternion[2]),
                            2 * (quaternion[0] * quaternion[2] + quaternion[3] * quaternion[1]))

        return Matrix4I.initRotationMatrix(forward, up, right)
    }

    /**
     * Initializes rotation matrix using forward, up and right vector.
     *
     * @param forward forward 3i vector.
     * @param up      up 3i vector.
     * @param right   right 3i vector.
     * @return rotation matrix.
     */
    @JvmStatic
    fun initRotationMatrix(forward: VectorI, up: VectorI, right: VectorI): MatrixI {
        require(forward.size == 3) { "Invalid forward vector size (${forward.size}), expected size of 3!" }
        require(up.size == 3) { "Invalid up vector size (${up.size}), expected size of 3!" }
        require(right.size == 3) { "Invalid right vector size (${right.size}), expected size of 3!" }
        return MatrixI(Array(4) { row ->
            Array(4) { column ->
                when {
                    row == 0 && column != 3 -> right[column]
                    row == 1 && column != 3 -> up[column]
                    row == 2 && column != 3 -> forward[column]
                    row == 3 && column == 3 -> 1
                    else -> 0
                }
            }
        })
    }

    /**
     * Utility method that combines translation and rotation directly and returns world transformation matrix.
     *
     * @since 3.0.0
     *
     * @param eye camera position 3i vector.
     * @param center position to look at.
     * @param up up 3i vector.
     * @return world transformation matrix.
     */
    @JvmStatic
    fun lookAt(eye: VectorI, center: VectorI, up: VectorI): MatrixI {
        require(eye.size == 3) { "Invalid eye position vector size (${eye.size}), expected size of 3!" }
        require(center.size == 3) { "Invalid center position vector size (${center.size}), expected size of 3!" }
        require(up.size == 3) { "Invalid up vector size (${up.size}), expected size of 3!" }

        val forward = (eye - center).normalized()

        return MatrixI.initTranslationMatrix(eye - center) * initRotationMatrix(forward, up)
    }
}
