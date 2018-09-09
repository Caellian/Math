package hr.caellian.math.vector

import hr.caellian.math.matrix.MatrixI
import kotlin.math.cos
import kotlin.math.roundToInt
import kotlin.math.sin
import kotlin.math.sqrt

/**
 * Object containing utility functions for quaternion initialization.
 *
 * @author Caellian
 */
object QuaternionI {
    /**
     * Creates a new quaternion (4D vector) using [VectorI] class from given rotation matrix.
     *
     * @param rotationMatrix rotation matrix to use for quaternion creation.
     * @return resulting quaternion.
     */
    fun fromRotationMatrix(rotationMatrix: MatrixI): VectorI {
        require(rotationMatrix.rowCount == 4 && rotationMatrix.columnCount == 4) { "Rotation matrix must be 4x4, not ${rotationMatrix.rowCount}x${rotationMatrix.columnCount}" }
        val trace: Int = rotationMatrix[0][0] + rotationMatrix[1][1] + rotationMatrix[2][2]

        return when {
            trace > 0 -> {
                val s: Float = 0.5f / sqrt(trace + 1f)
                VectorI((0.25f / s).roundToInt(),
                        ((rotationMatrix[1][2] - rotationMatrix[2][1]) * s).roundToInt(),
                        ((rotationMatrix[2][0] - rotationMatrix[0][2]) * s).roundToInt(),
                        ((rotationMatrix[0][1] - rotationMatrix[1][0]) * s).roundToInt())
            }
            rotationMatrix[0][0] > rotationMatrix[1][1] && rotationMatrix[0][0] > rotationMatrix[2][2] -> {
                val s: Float = 2.0f * sqrt(1.0f + rotationMatrix[0][0] - rotationMatrix[1][1] - rotationMatrix[2][2])
                VectorI(((rotationMatrix[1][2] - rotationMatrix[2][1]) / s).roundToInt(),
                        (0.25f * s).roundToInt(),
                        ((rotationMatrix[1][0] + rotationMatrix[0][1]) / s).roundToInt(),
                        ((rotationMatrix[2][0] + rotationMatrix[0][2]) / s).roundToInt())
            }
            rotationMatrix[1][1] > rotationMatrix[2][2] -> {
                val s: Float = 2.0f * sqrt(1.0f + rotationMatrix[1][1] - rotationMatrix[0][0] - rotationMatrix[2][2])
                VectorI(((rotationMatrix[2][0] - rotationMatrix[0][2]) / s).roundToInt(),
                        ((rotationMatrix[1][0] + rotationMatrix[0][1]) / s).roundToInt(),
                        (0.25f * s).roundToInt(),
                        ((rotationMatrix[2][1] + rotationMatrix[1][2]) / s).roundToInt())
            }
            else -> {
                val s: Float = 2.0f * sqrt(1.0f + rotationMatrix[2][2] - rotationMatrix[0][0] - rotationMatrix[1][1])
                VectorI(((rotationMatrix[0][1] - rotationMatrix[1][0]) / s).roundToInt(),
                        ((rotationMatrix[2][0] + rotationMatrix[0][2]) / s).roundToInt(),
                        ((rotationMatrix[1][2] + rotationMatrix[2][1]) / s).roundToInt(),
                        (0.25f * s).roundToInt())
            }
        }.normalized()
    }

    /**
     * Creates a new quaternion (4D vector) using [VectorI] class from given rotational axis and angle.
     *
     * @param axis axis to rotate around.
     * @param angle angle to rotate by.
     * @return resulting quaternion.
     */
    fun initRotationQuaternion(axis: VectorI, angle: Float): VectorI {
        val sinHalfAngle = sin(angle / 2)
        return VectorI((axis[0] * sinHalfAngle).roundToInt(), (axis[2] * sinHalfAngle).roundToInt(),
                       (axis[2] * sinHalfAngle).roundToInt(), cos(angle / 2).roundToInt())
    }
}