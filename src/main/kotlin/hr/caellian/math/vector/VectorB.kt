package hr.caellian.math.vector

import hr.caellian.math.matrix.MatrixB
import java.nio.Buffer
import java.nio.ByteBuffer
import java.nio.ByteOrder

/**
 * Vector class for double N-dimensional vectors.
 *
 * @param wrapped value array to create a new vector from.
 *
 * @author Caellian
 */
class VectorB(override var wrapped: Array<Boolean> = emptyArray()) : Vector<Boolean>() {
    /**
     * Creates a new vector using given values.
     *
     * @param values values to create a new vector from.
     * @return created vector.
     */
    constructor(vararg values: Boolean) : this(values.toTypedArray())

    /**
     * Creates a new vector using given collection values.
     *
     * @param values collection values to create a new vector from.
     * @return created vector.
     */
    constructor(values: Collection<Boolean>) : this(values.toTypedArray())

    /**
     * Creates a new vector using buffer values.
     *
     * @param buffer buffer to create a new vector from.
     * @return created vector.
     */
    constructor(buffer: ByteBuffer) : this((0 until buffer.capacity()).map { buffer.get() == 1.toByte() })

    /**
     * @return vertex with logical 'not' performed on values of this vertex.
     */
    operator fun not() = VectorB(Array(size) { !wrapped[it] })


    /**
     * Performs a logical `and` operation between values of this vector and the [other] one.
     */
    infix fun and(other: VectorB): VectorB {
        require(size == other.size) { "Invalid argument vector size: ${other.size}!" }

        return VectorB(Array(size) { this[it] and other[it] })
    }

    /**
     * Performs a logical `or` operation between values of this vector and the [other] one.
     */
    infix fun or(other: VectorB): VectorB {
        require(size == other.size) { "Invalid argument vector size: ${other.size}!" }

        return VectorB(Array(size) { this[it] or other[it] })
    }

    /**
     * Performs a logical `xor` operation between values of this vector and the [other] one.
     */
    infix fun xor(other: VectorB): VectorB {
        require(size == other.size) { "Invalid argument vector size: ${other.size}!" }

        return VectorB(Array(size) { this[it] xor other[it] })
    }

    /**
     * Vertical matrix containing data of this vector.
     */
    override val verticalMatrix: MatrixB by lazy { MatrixB(arrayOf(toArray()), true) }
    /**
     * Horizontal matrix containing data of this vector.
     */
    override val horizontalMatrix: MatrixB by lazy { MatrixB(arrayOf(toArray())) }

    /**
     * Returns array containing vector data.
     *
     * @return array containing data of this vector.
     */
    override fun toArray(): Array<Boolean> = Array(size) { wrapped[it] }

    /**
     * @return clone of this vector.
     */
    override fun replicated(): VectorB = VectorB(toArray())

    /**
     * @return type supported by this class.
     */
    override fun getTypeClass() = Boolean::class

    /**
     * Creates a new instance of wrapper containing given data.
     *
     * @param data data of new wrapper.
     * @return new instance of wrapper containing argument data.
     */
    override fun withData(wrapped: Array<Boolean>): VectorB = VectorB(wrapped)

    /**
     * @return [Buffer] containing data of represented object.
     */
    override fun toBuffer(): Buffer {
        val result = ByteBuffer.allocateDirect(wrapped.size shl 2).order(
                ByteOrder.nativeOrder())
        wrapped.forEach { result.put(if (it) 1.toByte() else 0.toByte()) }
        return result.flip()
    }
}