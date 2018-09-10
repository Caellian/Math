package hr.caellian.math.internal

import java.nio.Buffer

/**
 * Object implementing this interface can be directly transferred to [Buffer] objects using [toBuffer] method.
 *
 * @since 3.0.0
 */
interface BufferConstructor {
    /**
     * @return [Buffer] containing data of represented object.
     */
    fun toBuffer(): Buffer
}