package hr.caellian.math.internal

/**
 * Objects implementing this interface act as utility wrappers for data provided by data variable.
 *
 * @tparam C type of wrapper.
 * @tparam T type contained in wrapper.
 */
interface DataWrapper<C : DataWrapper<C, T>, T> {
    /**
     * Wrapped data.
     *
     * This value shouldn't be accessed directly.
     */
    var wrapped: T

    /**
     * Creates a new instance of wrapper containing given data.
     *
     * @param data data of new wrapper.
     * @return new instance of wrapper containing argument data.
     */
    fun withData(wrapped: T): C
}