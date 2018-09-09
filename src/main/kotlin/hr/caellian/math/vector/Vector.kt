package hr.caellian.math.vector

import hr.caellian.math.matrix.Matrix
import hr.caellian.math.internal.BufferConstructor
import hr.caellian.math.internal.DataWrapper
import hr.caellian.math.internal.Replicable
import java.util.*

/**
 * Generic Vector trait defined for stable code infrastructure and consistency.
 * Most basic Vector variables and functions can be found here.
 *
 * @tparam T vector data type.
 * @author Caellian
 */
abstract class Vector<T> : Replicable<Vector<T>>, DataWrapper<Vector<T>, Array<T>>, BufferConstructor {
    /**
     * Size of this vector.
     */
    val size: Int
        get() = wrapped.size

    /**
     * @param index element return
     *
     * @return vector element at [index] position.
     */
    operator fun get(index: Int): T = wrapped[index]

    /**
     * @return this vector.
     */
    operator fun unaryPlus() = this

    /**
     * @return new vector with negated values.
     */
    abstract operator fun unaryMinus(): Vector<T>

    /**
     * Adds two vectors together and returns resulting vector.
     * In order to add to matrices together, they must be of same size.
     *
     * @return result of vector addition.
     */
    abstract operator fun plus(other: Vector<T>): Vector<T>

    /**
     * Subtracts other vector from this one and returns resulting vector.
     * In order to subtract one vector from another, both vectors must be of same size.
     *
     * @return result of vector subtraction.
     */
    abstract operator fun minus(other: Vector<T>): Vector<T>

    /**
     * Multiplies two vectors together and returns resulting vector.
     * In order to add to multiply vectors together, they must be of same size.
     *
     * @return result of vector multiplication.
     */
    abstract operator fun times(other: Vector<T>): Vector<T>

    /**
     * Divides this vector with other and returns resulting vector.
     * In order to divide one vector with another, both vectors must be of same size.
     *
     * @return result of vector division.
     */
    abstract operator fun div(other: Vector<T>): Vector<T>

    /**
     * Performs scalar addition on this vector and returns resulting vector.
     *
     * @return result of vector scalar addition.
     */
    abstract operator fun plus(value: T): Vector<T>

    /**
     * Performs scalar subtraction on this vector and returns resulting vector.
     *
     * @return result of scalar vector subtraction.
     */
    abstract operator fun minus(value: T): Vector<T>

    /**
     * Performs scalar multiplication on this vector and returns resulting vector.
     *
     * @return result of scalar vector multiplication.
     */
    abstract operator fun times(value: T): Vector<T>

    /**
     * Performs scalar division on this vector and returns resulting vector.
     *
     * @return result of scalar vector division.
     */
    abstract operator fun div(value: T): Vector<T>

    /**
     * @return biggest value of a member of this vector.
     */
    abstract fun max(): T

    /**
     * @return smalled value of a member of this vector.
     */
    abstract fun min(): T

    /**
     * @return new vector containing absolute values of this vector.
     */
    abstract fun absolute(): Vector<T>

    /**
     * @return new vector with normalized values of this one.
     */
    abstract fun normalized(): Vector<T>

    /**
     * @return magnitude of this vector.
     */
    abstract fun magnitude(): T

    /**
     * Calculates distance from this to other vector.
     *
     * @return distance between this and other vector.
     */
    abstract fun distanceTo(other: Vector<T>): T

    /**
     *
     * @return dot product of two vectors.
     */
    abstract infix fun dot(other: Vector<T>): T

    /**
     * Returns cross product of this and other vector.
     *
     * @return cross product.
     */
    abstract fun cross(other: Vector<T>): Vector<T>

    /**
     * Rotates this vector using rotation matrix.
     *
     * @return rotated vector.
     */
    abstract fun rotated(rotationMatrix: Matrix<T>): Vector<T>

    /**
     * Linearly interpolates between two vectors.
     *
     * @return linear interpolation.
     */
    abstract fun lerp(destination: Vector<T>, percent: T): Vector<T>

    /**
     * Vertical matrix containing data of this vector.
     */
    abstract val verticalMatrix: Matrix<T>

    /**
     * Horizontal matrix containing data of this vector.
     */
    abstract val horizontalMatrix: Matrix<T>

    /**
     * Returns array containing vector data.
     * Default implementation:
     * <code>
     *     Array(size) { wrapped[it] }
     * </code>
     *
     * @return array containing data of this vector.
     */
    abstract fun toArray(): Array<T>

    /**
     * @return clone of this vector.
     */
    abstract override fun replicated(): Vector<T>

    /**
     * @return true if this vector is equal to other vector.
     */
    override fun equals(other: Any?): Boolean {
        if (other == null) {
            return false
        }
        if (this === other) {
            return true
        }
        if (other !is Vector<*>) {
            return false
        }
        return wrapped contentDeepEquals other.wrapped
    }

    /**
     * Vector hashcode depends on vector data and will change if vector data is modified!
     *
     * @return hashcode of this vector.
     */
    override fun hashCode(): Int = Arrays.hashCode(wrapped)

    /**
     * @return string representation of this vector.
     */
    override fun toString(): String = "(${wrapped.joinToString(", ")})"
}