package hr.caellian.math.internal

import java.nio.Buffer

/**
 * Object implementing this interface can be directly transferred to [Buffer] objects using [toBuffer] method.
 */
interface BufferConstructor {
    /**
     * @return [Buffer] containing data of represented object.
     */
    fun toBuffer(): Buffer
}