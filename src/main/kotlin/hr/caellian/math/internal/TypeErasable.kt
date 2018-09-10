package hr.caellian.math.internal

import kotlin.reflect.KClass

/**
 * Objects implementing this interface allow access to their implementation type's [KClass] object instance.
 *
 * @tparam T type implemented by child objects.
 *
 * @since 3.0.0
 */
interface TypeErasable<T : Any> {
    /**
     * @return type supported by this class.
     */
    fun getTypeClass(): KClass<T>
}