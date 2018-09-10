package hr.caellian.math.vector

import hr.caellian.math.matrix.MatrixN

/**
 * Abstract Vector class defining number vector specific behaviour.
 *
 * This class doesn't represent 'uvecn' GLSL type. Use [VectorI] instead as JVM has no concept of unsigned types.
 *
 * @author Caellian
 */
abstract class VectorN<T : Number> : Vector<T>() {
    /**
     * @return this vector.
     */
    operator fun unaryPlus() = this

    /**
     * @return new vector with negated values.
     */
    abstract operator fun unaryMinus(): VectorN<T>

    /**
     * Adds two vectors together and returns resulting vector.
     * In order to add to matrices together, they must be of same size.
     *
     * @return result of vector addition.
     */
    abstract operator fun plus(other: VectorN<T>): VectorN<T>

    /**
     * Subtracts other vector from this one and returns resulting vector.
     * In order to subtract one vector from another, both vectors must be of same size.
     *
     * @return result of vector subtraction.
     */
    abstract operator fun minus(other: VectorN<T>): VectorN<T>

    /**
     * Multiplies two vectors together and returns resulting vector.
     * In order to add to multiply vectors together, they must be of same size.
     *
     * @return result of vector multiplication.
     */
    abstract operator fun times(other: VectorN<T>): VectorN<T>

    /**
     * Divides this vector with other and returns resulting vector.
     * In order to divide one vector with another, both vectors must be of same size.
     *
     * @return result of vector division.
     */
    abstract operator fun div(other: VectorN<T>): VectorN<T>

    /**
     * Performs scalar addition on this vector and returns resulting vector.
     *
     * @return result of vector scalar addition.
     */
    abstract operator fun plus(value: T): VectorN<T>

    /**
     * Performs scalar subtraction on this vector and returns resulting vector.
     *
     * @return result of scalar vector subtraction.
     */
    abstract operator fun minus(value: T): VectorN<T>

    /**
     * Performs scalar multiplication on this vector and returns resulting vector.
     *
     * @return result of scalar vector multiplication.
     */
    abstract operator fun times(value: T): VectorN<T>

    /**
     * Performs scalar division on this vector and returns resulting vector.
     *
     * @return result of scalar vector division.
     */
    abstract operator fun div(value: T): VectorN<T>

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
    abstract fun absolute(): VectorN<T>

    /**
     * @return new vector with normalized values of this one.
     */
    abstract fun normalized(): VectorN<T>

    /**
     * @return magnitude of this vector.
     */
    abstract fun magnitude(): T

    /**
     * Calculates distance from this to other vector.
     *
     * @return distance between this and other vector.
     */
    abstract fun distanceTo(other: VectorN<T>): T

    /**
     *
     * @return dot product of two vectors.
     */
    abstract infix fun dot(other: VectorN<T>): T

    /**
     * Returns cross product of this and other vector.
     *
     * @return cross product.
     */
    abstract fun cross(other: VectorN<T>): Vector<T>

    /**
     * Rotates this vector using rotation matrix.
     *
     * @return rotated vector.
     */
    abstract fun rotated(rotationMatrix: MatrixN<T>): Vector<T>

    /**
     * Linearly interpolates between two vectors.
     *
     * @return linear interpolation.
     */
    abstract fun lerp(destination: VectorN<T>, percent: T): Vector<T>
}
