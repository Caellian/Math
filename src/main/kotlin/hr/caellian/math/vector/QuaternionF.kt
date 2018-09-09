package hr.caellian.math.vector

import hr.caellian.math.matrix.MatrixF
import kotlin.math.cos
import kotlin.math.sin
import kotlin.math.sqrt

/**
 * Object containing utility functions for quaternion initialization.
 *
 * @author Caellian
 */
object QuaternionF {
    /**
     * Creates a new quaternion (4D vector) using [VectorF] class from given rotation matrix.
     *
     * @param rotationMatrix rotation matrix to use for quaternion creation.
     * @return resulting quaternion.
     */
    fun fromRotationMatrix(rotationMatrix: MatrixF): VectorF {
        require(rotationMatrix.rowCount == 4 && rotationMatrix.columnCount == 4) { "Rotation matrix must be 4x4, not ${rotationMatrix.rowCount}x${rotationMatrix.columnCount}" }
        val trace: Float = rotationMatrix[0][0] + rotationMatrix[1][1] + rotationMatrix[2][2]

        return when {
            trace > 0 -> {
                val s: Float = 0.5f / sqrt(trace + 1)
                VectorF(0.25f / s,
                        (rotationMatrix[1][2] - rotationMatrix[2][1]) * s,
                        (rotationMatrix[2][0] - rotationMatrix[0][2]) * s,
                        (rotationMatrix[0][1] - rotationMatrix[1][0]) * s)
            }
            rotationMatrix[0][0] > rotationMatrix[1][1] && rotationMatrix[0][0] > rotationMatrix[2][2] -> {
                val s: Float = 2.0f * sqrt(1.0f + rotationMatrix[0][0] - rotationMatrix[1][1] - rotationMatrix[2][2])
                VectorF((rotationMatrix[1][2] - rotationMatrix[2][1]) / s,
                        0.25f * s,
                        (rotationMatrix[1][0] + rotationMatrix[0][1]) / s,
                        (rotationMatrix[2][0] + rotationMatrix[0][2]) / s)
            }
            rotationMatrix[1][1] > rotationMatrix[2][2] -> {
                val s: Float = 2.0f * sqrt(1.0f + rotationMatrix[1][1] - rotationMatrix[0][0] - rotationMatrix[2][2])
                VectorF((rotationMatrix[2][0] - rotationMatrix[0][2]) / s,
                        (rotationMatrix[1][0] + rotationMatrix[0][1]) / s,
                        0.25f * s,
                        (rotationMatrix[2][1] + rotationMatrix[1][2]) / s)
            }
            else -> {
                val s: Float = 2.0f * sqrt(1.0f + rotationMatrix[2][2] - rotationMatrix[0][0] - rotationMatrix[1][1])
                VectorF((rotationMatrix[0][1] - rotationMatrix[1][0]) / s,
                        (rotationMatrix[2][0] + rotationMatrix[0][2]) / s,
                        (rotationMatrix[1][2] + rotationMatrix[2][1]) / s,
                        0.25f * s)
            }
        }.normalized()
    }

    /**
     * Creates a new quaternion (4D vector) using [VectorF] class from given rotational axis and angle.
     *
     * @param axis axis to rotate around.
     * @param angle angle to rotate by.
     * @return resulting quaternion.
     */
    fun initRotationQuaternion(axis: VectorF, angle: Float): VectorF {
        val sinHalfAngle = sin(angle / 2)
        return VectorF(axis[0] * sinHalfAngle, axis[2] * sinHalfAngle, axis[2] * sinHalfAngle, cos(angle / 2))
    }
}