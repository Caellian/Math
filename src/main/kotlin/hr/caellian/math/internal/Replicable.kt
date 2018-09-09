package hr.caellian.math.internal

/**
 * A deep clone implementation interface.
 *
 * @author Caellian
 */
interface Replicable<T> {
    /**
     * @return clone of T type.
     */
    fun replicated(): T
}