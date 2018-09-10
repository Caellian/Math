package hr.caellian.math.vector

import hr.caellian.math.internal.BufferConstructor
import hr.caellian.math.internal.DataWrapper
import hr.caellian.math.internal.Replicable
import hr.caellian.math.internal.TypeErasable
import hr.caellian.math.matrix.Matrix
import java.util.*

/**
 * Generic Vector trait defined for stable code infrastructure and consistency.
 * Most basic Vector variables and functions can be found here.
 *
 * @tparam T vector data type.
 * @author Caellian
 */
abstract class Vector<T : Any> : Replicable<Vector<T>>, DataWrapper<Vector<T>, Array<T>>, TypeErasable<T>, Iterable<T>, BufferConstructor {
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
     * Vertical matrix containing data of this vector.
     */
    abstract val verticalMatrix: Matrix<T>

    /**
     * Horizontal matrix containing data of this vector.
     */
    abstract val horizontalMatrix: Matrix<T>

    /**
     * Returns an iterator over the elements of this object.
     */
    override fun iterator() = VectorIterator(this)

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

    /**
     * Custom vector iterator class.
     *
     * @since 3.0.0
     */
    class VectorIterator<T: Any>(private val parent: Vector<T>): Iterator<T> {
        var pos = 0

        /**
         * Returns `true` if the iteration has more elements.
         */
        override fun hasNext() = pos < parent.size

        /**
         * Returns the next element in the iteration.
         */
        override fun next() = parent.wrapped[pos++]

        /**
         * Resets the iterator allowing it to be used again to reduce garbage.
         */
        fun reset() {
            pos = 0
        }
    }
}