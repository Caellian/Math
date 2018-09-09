package hr.caellian.math.vector

import hr.caellian.math.matrix.Matrix
import hr.caellian.math.matrix.MatrixD
import java.nio.Buffer
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.DoubleBuffer
import kotlin.math.abs
import kotlin.math.sqrt

/**
 * Vector class for double N-dimensional vectors.
 *
 * @param wrapped value array to create a new vector from.
 *
 * @author Caellian
 */
class VectorD(override var wrapped: Array<Double> = emptyArray()) : VectorN<Double>() {

    /**
     * Creates a new vector using given values.
     *
     * @param values values to create a new vector from.
     * @return created vector.
     */
    constructor(vararg values: Double) : this(values.toTypedArray())

    /**
     * Creates a new vector using given collection values.
     *
     * @param values collection values to create a new vector from.
     * @return created vector.
     */
    constructor(values: Collection<Double>) : this(values.toTypedArray())

    /**
     * Creates a new vector using buffer values.
     *
     * @param buffer buffer to create a new vector from.
     * @return created vector.
     */
    constructor(buffer: DoubleBuffer) : this((0 until buffer.capacity()).map { buffer.get() })

    /**
     * @return new vector with negated values.
     */
    override operator fun unaryMinus(): VectorD = VectorD(Array(size) { -this[it] })

    /**
     * Adds two vectors together and returns resulting vector.
     * In order to add to matrices together, they must be of same size.
     *
     * @return result of vector addition.
     */
    override operator fun plus(other: Vector<Double>): VectorD {
        require(size == other.size) { "Invalid argument vector size: ${other.size}!" }
        return VectorD(Array(size) { this[it] + other[it] })
    }

    /**
     * Subtracts other vector from this one and returns resulting vector.
     * In order to subtract one vector from another, both vectors must be of same size.
     *
     * @return result of vector subtraction.
     */
    override operator fun minus(other: Vector<Double>): VectorD {
        require(size == other.size) { "Invalid argument vector size: ${other.size}!" }
        return VectorD(Array(size) { this[it] - other[it] })
    }

    /**
     * Multiplies two vectors together and returns resulting vector.
     * In order to add to multiply vectors together, they must be of same size.
     *
     * @return result of vector multiplication.
     */
    override operator fun times(other: Vector<Double>): VectorD {
        require(size == other.size) { "Invalid argument vector size: ${other.size}!" }
        return VectorD(Array(size) { this[it] * other[it] })
    }

    /**
     * Divides this vector with other and returns resulting vector.
     * In order to divide one vector with another, both vectors must be of same size.
     *
     * @return result of vector division.
     */
    override operator fun div(other: Vector<Double>): VectorD {
        require(size == other.size) { "Invalid argument vector size: ${other.size}!" }
        return VectorD(Array(size) { this[it] / other[it] })
    }

    /**
     * Performs scalar addition on this vector and returns resulting vector.
     *
     * @return result of vector scalar addition.
     */
    override operator fun plus(value: Double): VectorD = VectorD(Array(size) { this[it] + value })

    /**
     * Performs scalar subtraction on this vector and returns resulting vector.
     *
     * @return result of scalar vector subtraction.
     */
    override operator fun minus(value: Double): VectorD = VectorD(Array(size) { this[it] - value })

    /**
     * Performs scalar multiplication on this vector and returns resulting vector.
     *
     * @return result of scalar vector multiplication.
     */
    override operator fun times(value: Double): VectorD = VectorD(Array(size) { this[it] * value })

    /**
     * Performs scalar division on this vector and returns resulting vector.
     *
     * @return result of scalar vector division.
     */
    override operator fun div(value: Double): VectorD = VectorD(Array(size) { this[it] / value })

    /**
     * @return biggest value of a member of this vector.
     */
    override fun max(): Double = wrapped.max() ?: 0.0

    /**
     * @return smalled value of a member of this vector.
     */
    override fun min(): Double = wrapped.min() ?: 0.0

    /**
     * @return new vector containing absolute values of this vector.
     */
    override fun absolute(): VectorD = VectorD(Array(size) { abs(this[it]) })

    /**
     * @return new vector with normalized values of this one.
     */
    override fun normalized(): VectorD = this / magnitude()

    /**
     * @return magnitude of this vector.
     */
    override fun magnitude(): Double = distanceTo(this)

    /**
     * Calculates distance from this to other vector.
     *
     * @return distance between this and other vector.
     */
    override fun distanceTo(other: Vector<Double>): Double = sqrt(this dot other)

    /**
     *
     * @return dot product of two vectors.
     */
    override fun dot(other: Vector<Double>): Double {
        require(size == other.size) { "Invalid argument vector size: ${other.size}!" }
        return wrapped.zip(other.wrapped).sumByDouble { (a, b) -> a * b }
    }

    /**
     * Returns cross product of this and other vector.
     *
     * @return cross product.
     */
    override fun cross(other: Vector<Double>): VectorD {
        require(size == other.size) { "Invalid argument vector size: ${other.size}!" }

        return when (size) {
            3 -> VectorD(arrayOf(
                    this[1] * other[2] - this[2] * other[1],
                    this[2] * other[0] - this[0] * other[2],
                    this[0] * other[1] - this[1] * other[0]
            ))
            7 -> VectorD(arrayOf(
                    this[1] * other[3] - this[3] * other[1] + this[2] * other[6] - this[6] * other[2] + this[4] * other[5] - this[5] * other[4],
                    this[2] * other[4] - this[4] * other[2] + this[3] * other[0] - this[0] * other[3] + this[5] * other[6] - this[6] * other[5],
                    this[3] * other[5] - this[5] * other[3] + this[4] * other[1] - this[1] * other[4] + this[6] * other[0] - this[0] * other[6],
                    this[4] * other[6] - this[6] * other[4] + this[5] * other[2] - this[2] * other[5] + this[0] * other[1] - this[1] * other[0],
                    this[5] * other[0] - this[0] * other[5] + this[6] * other[3] - this[3] * other[6] + this[1] * other[2] - this[2] * other[1],
                    this[6] * other[1] - this[1] * other[6] + this[0] * other[4] - this[4] * other[0] + this[2] * other[3] - this[3] * other[2],
                    this[0] * other[2] - this[2] * other[0] + this[1] * other[5] - this[5] * other[1] + this[3] * other[4] - this[4] * other[3]
            ))
            else -> throw NotImplementedError("Cross product does not exist in $size-dimensional space!")
        }
    }

    /**
     * Rotates this vector using rotation matrix.
     *
     * @return rotated vector.
     */
    override fun rotated(rotationMatrix: Matrix<Double>): VectorD = (rotationMatrix * verticalMatrix).toVector() as VectorD

    /**
     * Linearly interpolates between two vectors.
     *
     * @return linear interpolation.
     */
    override fun lerp(destination: Vector<Double>, percent: Double): VectorD = this + (destination - this) * percent

    /**
     * Vertical matrix containing data of this vector.
     */
    override val verticalMatrix: MatrixD by lazy { MatrixD(arrayOf(toArray()), true) }
    /**
     * Horizontal matrix containing data of this vector.
     */
    override val horizontalMatrix: MatrixD by lazy { MatrixD(arrayOf(toArray())) }

    /**
     * Returns array containing vector data.
     *
     * @return array containing data of this vector.
     */
    override fun toArray(): Array<Double> = Array(size) { wrapped[it] }

    /**
     * @return clone of this vector.
     */
    override fun replicated(): VectorD = VectorD(toArray())

    /**
     * Creates a new instance of wrapper containing given data.
     *
     * @param data data of new wrapper.
     * @return new instance of wrapper containing argument data.
     */
    override fun withData(wrapped: Array<Double>): VectorD = VectorD(wrapped)

    /**
     * @return [Buffer] containing data of represented object.
     */
    override fun toBuffer(): Buffer {
        val result = ByteBuffer.allocateDirect(wrapped.size shl 2).order(
                ByteOrder.nativeOrder()).asDoubleBuffer()
        wrapped.forEach { result.put(it) }
        return result.flip()
    }
}
