package hr.caellian.math.internal

/**
 * Utility object containing functions for data comparison and hash code generation.
 *
 * @author Caellian
 */
object DataUtil {
    /**
     * Creates a new 2D array that is transposed (from this).
     *
     * @return transposed version of this array.
     */
    inline fun <reified T> Array<Array<T>>.transpose(): Array<Array<T>> {
        return Array(this[0].size) { i -> Array(this.size) { j -> this[j][i] } }
    }

    /**
     * Returns the sum of all [Float] values produced by [selector] function applied to each element in the collection.
     */
    inline fun <T> Iterable<T>.sumByFloat(selector: (T) -> Float): Float {
        var sum = 0f
        for (element in this) {
            sum += selector(element)
        }
        return sum
    }
}
