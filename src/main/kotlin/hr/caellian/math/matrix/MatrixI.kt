package hr.caellian.math.matrix

import hr.caellian.math.internal.DataUtil.transpose
import hr.caellian.math.vector.VectorI
import hr.caellian.math.vector.VectorN
import java.nio.Buffer
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.IntBuffer
import kotlin.math.cos
import kotlin.math.roundToInt
import kotlin.math.sin

/**
 * Matrix class for float N-dimensional matrices.
 *
 * @author Caellian
 */
class MatrixI(override var wrapped: Array<Array<Int>> = emptyArray(), vertical: Boolean = false) : MatrixN<Int>() {

    init {
        require(rowCount > 1 && columnCount > 1) { "Invalid matrix size!" }
        require(validate()) {
            if (vertical)
                "Matrix columns must be of equal length!"
            else
                "Matrix rows must be of equal length!"
        }

        if (vertical) {
            wrapped = transpose().wrapped
        }
    }

    /**
     * Creates a new blank square matrix.
     *
     * @param size width & height of new matrix.
     */
    constructor(size: Int, default: Int = 0) : this(Array(size) { _ -> Array(size) { default } })

    /**
     * Creates a new matrix containing given data.
     *
     * @param rows    number of rows matrix should store.
     * @param columns number of columns matrix should store.
     */
    constructor(rows: Int, columns: Int, default: Int = 0) : this(Array(rows) { _ -> Array(columns) { default } })

    /**
     * Creates a new matrix using collection values.
     *
     * @param values      values to create a new matrix from.
     * @return created matrix.
     */
    constructor(values: Collection<Collection<Int>>) : this(values.map { it.toTypedArray() }.toTypedArray())

    /**
     * Creates a new matrix using buffer values.
     *
     * @param buffer      buffer to create a new matrix from.
     * @param rowCount    number of rows new matrix will have.
     * @param columnCount number of columns new matrix will have.
     * @return created matrix.
     */
    constructor(buffer: IntBuffer, rowCount: Int, columnCount: Int) : this(
            Array(rowCount) { _ -> Array(columnCount) { buffer.get() } })

    /**
     * Creates a new square matrix using buffer values.
     *
     * This constructor depends on buffer capacity. Make sure square root of your buffer's capacity is equal to row and
     * column count.
     *
     * @param buffer buffer to create a new matrix from.
     * @return created matrix.
     */
    constructor(buffer: IntBuffer) : this(
            buffer.let { _ ->
                val rows = Math.sqrt(buffer.capacity().toDouble())
                require(rows - rows.roundToInt() == 0.0) { "Acquired buffer can't be used to create a square matrix." }
                val rowCount = rows.roundToInt()
                Array(rowCount) { _ -> Array(rowCount) { buffer.get() } }
            }
    )

    /**
     * @return new matrix with negated values.
     */
    override fun unaryMinus() = MatrixI(
            Array(rowCount) { row -> Array(columnCount) { column -> -wrapped[row][column] } })

    /**
     * Performs matrix addition and returns resulting matrix.
     * In order to add to matrices together, they must be of same size.
     *
     * @param other matrix to add to this one.
     * @return resulting of matrix addition.
     */
    override fun plus(other: MatrixN<Int>): MatrixI {
        require(rowCount == other.rowCount && columnCount == other.columnCount) { "Invalid argument matrix size: ${other.rowCount}x${other.columnCount}!" }
        return MatrixI(
                Array(rowCount) { row -> Array(columnCount) { column -> wrapped[row][column] + other[row][column] } })
    }

    /**
     * Performs matrix subtraction and returns resulting matrix.
     * In order to subtract one matrix from another, matrices must be of same size.
     *
     * @param other matrix to subtract from this one.
     * @return resulting of matrix subtraction.
     */
    override fun minus(other: MatrixN<Int>): MatrixI {
        require(rowCount == other.rowCount && columnCount == other.columnCount) { "Invalid argument matrix size: ${other.rowCount}x${other.columnCount}!" }
        return MatrixI(
                Array(rowCount) { row -> Array(columnCount) { column -> wrapped[row][column] - other[row][column] } })
    }

    /**
     * Performs matrix multiplication on this matrix.
     * Returns C from 'C = A×B' where A is this matrix and B is the other / argument matrix.
     *
     * @param other matrix to multiply this matrix with.
     * @return result of matrix multiplication.
     */
    override operator fun times(other: MatrixN<Int>): MatrixI {
        require(columnCount == other.rowCount) { "Invalid multiplication (mat${rowCount}x$columnCount) * (mat${other.rowCount}x${other.columnCount})!" }
        return MatrixI(Array(rowCount) { row ->
            Array(other.columnCount) { column ->
                (0 until columnCount).sumBy { wrapped[row][it] * other.wrapped[it][column] }
            }
        })
    }

    /**
     * Performs matrix multiplication on this matrix.
     * Returns C from 'C = A×B' where A is this matrix and B is the other / argument vector.
     *
     * @param other vector to multiply this matrix with.
     * @return result of matrix multiplication.
     */
    override fun times(other: VectorN<Int>): VectorI = (this * other.verticalMatrix as MatrixN<Int>).toVector()

    /**
     * Performs scalar multiplication on this matrix and returns resulting matrix.
     *
     * @param scalar scalar to multiply every member of this matrix with.
     * @return result of scalar matrix multiplication.
     */
    override operator fun times(scalar: Int): MatrixI {
        return MatrixI(Array(rowCount) { row -> Array(columnCount) { column -> wrapped[row][column] * scalar } })
    }

    /**
     * Switches two rows together.
     *
     * @param rowA row to be switched with rowB.
     * @param rowB row to be switched with rowA.
     * @return resulting matrix.
     */
    override fun switchRows(rowA: Int, rowB: Int): MatrixI {
        require(rowA in 0..rowCount && rowB in 0..rowCount && rowA != rowB) { "Illegal row argument(s)!" }
        return MatrixI(Array(rowCount) { row ->
            Array(columnCount) { column ->
                when (row) {
                    rowA -> wrapped[rowB][column]
                    rowB -> wrapped[rowA][column]
                    else -> wrapped[row][column]
                }
            }
        })
    }

    /**
     * Switches two columns together.
     *
     * @param columnA column to be switched with columnB.
     * @param columnB column to be switched with columnA.
     * @return resulting matrix.
     */
    override fun switchColumns(columnA: Int, columnB: Int): MatrixI {
        require(columnA in 0..columnCount && columnB in 0..columnCount && columnA != columnB) { "Illegal column argument(s)!" }
        return MatrixI(Array(rowCount) { row ->
            Array(columnCount) { column ->
                when (column) {
                    columnA -> wrapped[row][columnB]
                    columnB -> wrapped[row][columnA]
                    else -> wrapped[row][column]
                }
            }
        })
    }

    /**
     * Multiplies all entries of a row with given scalar.
     *
     * @param row        row to multiply.
     * @param multiplier scalar to multiply rows entries with.
     * @return resulting matrix.
     */
    override fun multiplyRow(row: Int, multiplier: Int): MatrixI {
        require(row in 0..rowCount) { "Illegal row argument!" }
        return MatrixI(Array(rowCount) { rowI ->
            Array(columnCount) { column ->
                when (rowI) {
                    row -> wrapped[rowI][column] * multiplier
                    else -> wrapped[rowI][column]
                }
            }
        })
    }

    /**
     * Multiplies all entries of a column with given scalar.
     *
     * @param column        column to multiply.
     * @param multiplier scalar to multiply column entries with.
     * @return resulting matrix.
     */
    override fun multiplyColumn(column: Int, multiplier: Int): MatrixI {
        require(column in 0..columnCount) { "Illegal row argument!" }
        return MatrixI(Array(rowCount) { row ->
            Array(columnCount) { columnI ->
                when (columnI) {
                    column -> wrapped[row][columnI] * multiplier
                    else -> wrapped[row][columnI]
                }
            }
        })
    }

    /**
     * Adds one row from matrix to another.
     *
     * @param from       row to add to another row.
     * @param to         row to add another row to; data will be stored on this row.
     * @param multiplier scalar to multiply all members of added row with on addition. It equals to 1 by default.
     * @return new matrix.
     */
    override fun addRows(from: Int, to: Int, multiplier: Int?): MatrixI {
        require(from in 0..rowCount && to in 0..rowCount) { "Illegal row argument(s)!" }
        val multiplierVal = multiplier ?: 1
        return MatrixI(Array(rowCount) { row ->
            Array(columnCount) { column ->
                when (row) {
                    to -> wrapped[to][column] + wrapped[from][column] * multiplierVal
                    else -> wrapped[row][column]
                }
            }
        })
    }

    /**
     * Adds one column from matrix to another.
     *
     * @param from       column to add to another column.
     * @param to         column to add another column to; data will be stored on this column.
     * @param multiplier scalar to multiply all members of added column with on addition. It equals to 1 by default.
     * @return new matrix.
     */
    override fun addColumns(from: Int, to: Int, multiplier: Int?): MatrixI {
        require(from in 0..columnCount && to in 0..columnCount) { "Illegal column argument(s)!" }
        val multiplierVal = multiplier ?: 1
        return MatrixI(Array(rowCount) { row ->
            Array(columnCount) { column ->
                when (column) {
                    to -> wrapped[row][to] + wrapped[row][from] * multiplierVal
                    else -> wrapped[row][column]
                }
            }
        })
    }

    /**
     * Inserts given row data at given index shifting rest of the matrix to the next index.
     *
     * @param index index at which added row data will be stored.
     * @param data  row data to store at given index.
     * @return new matrix with extended data.
     */
    override fun withRow(index: Int, data: Array<Int>): MatrixI {
        require(data.size == columnCount) { "Illegal row array size! Should be $columnCount." }
        return MatrixI(Array(rowCount + 1) { row ->
            Array(columnCount) { column ->
                when {
                    row == index -> data[column]
                    row > index -> wrapped[row + 1][column]
                    else -> wrapped[row][column]
                }
            }
        })
    }

    /**
     * Inserts given column data at given index shifting rest of the matrix to the next index.
     *
     * @param index index at which added column data will be stored.
     * @param data  column data to store at given index.
     * @return new matrix with extended data.
     */
    override fun withColumn(index: Int, data: Array<Int>): MatrixI {
        require(data.size == rowCount) { "Illegal column array size! Should be $rowCount." }
        return MatrixI(Array(rowCount) { row ->
            Array(columnCount + 1) { column ->
                when {
                    column == index -> data[row]
                    column > index -> wrapped[row][column + 1]
                    else -> wrapped[row][column]
                }
            }
        })
    }

    /**
     * Creates a new matrix without specified rows & columns.
     *
     * @param deletedRows    rows to exclude from submatrix.
     * @param deletedColumns columns to exclude from submatrix.
     * @return defined submatrix.
     */
    override fun submatrix(deletedRows: Array<Int>, deletedColumns: Array<Int>): MatrixI {
        require(deletedRows.all { it in 0..rowCount } && deletedColumns.all { it in 0..columnCount }) { "Tried to delete rows which don't exist." }

        val keptRows = (0..rowCount).toList().filterNot { deletedRows.contains(it) }
        val keptColumns = (0..columnCount).toList().filterNot { deletedColumns.contains(it) }

        return MatrixI(Array(keptRows.size) { row ->
            Array(keptColumns.size) { column ->
                wrapped[keptRows[row]][keptColumns[column]]
            }
        })
    }

    /**
     * Constructs a new vector out of column / row vector matrix.
     *
     * @return vector containing matrix data.
     */
    override fun toVector(): VectorI {
        require(columnCount == 1 || rowCount == 1 && !(columnCount > 1 && rowCount > 1)) { "Matrix cannot be turned into a vector!" }
        return if (columnCount > rowCount) {
            VectorI(firstRow()[0])
        } else {
            VectorI(firstColumn().transpose()[0])
        }
    }

    /**
     * Constructs a new vector out of any matrix dismissing extra data.
     *
     * @return vector containing only first column of matrix data.
     */
    override fun forceToVector(): VectorI = VectorI(firstColumn().transpose()[0])

    /**
     * @return a new matrix containing only the first row of this matrix.
     */
    override fun firstRow() = MatrixI(
            Array(1) { row -> Array(columnCount) { column -> wrapped[row][column] } })

    /**
     * @return a new matrix containing only the first column of this matrix.
     */
    override fun firstColumn() = MatrixI(
            Array(rowCount) { row -> Array(1) { column -> wrapped[row][column] } })

    /**
     * @return transposed matrix.
     */
    override fun transpose() = MatrixI(wrapped.transpose())

    /**
     * @return buffer containing data of this matrix.
     */
    override fun toBuffer(): Buffer {
        return ByteBuffer.allocateDirect((columnCount * rowCount) shl 2).order(ByteOrder.nativeOrder()).asIntBuffer()
                .also { buffer ->
                    wrapped.forEach { it.forEach { inner -> buffer.put(inner) } }
                    buffer.flip()
                }
    }

    /**
     * Returns copy of data of this Matrix as a 2D array.
     *
     * @return 2D array containing data of this matrix.
     */
    override fun toArray(): Array<Array<Int>> = Array(rowCount) { row ->
        Array(columnCount) { column -> wrapped[row][column] }
    }

    /**
     * @return clone of this matrix.
     */
    override fun replicated() = MatrixI(toArray())

    /**
     * @return type supported by this class.
     */
    override fun getTypeClass() = Int::class

    /**
     * Creates a new instance of [MatrixI] containing given data.
     *
     * @param data data of new wrapper.
     * @return new instance of wrapper containing argument data.
     */
    override fun withData(wrapped: Array<Array<Int>>) = MatrixI(wrapped)

    companion object {
        /**
         * Initializes a new n-dimensional rotation matrix.
         *
         * Unless you're working with 4+ dimensional space, it's recommended you use simpler, dimensionality specific
         * functions defined in specialised classes as this one is not the fastest for those simpler requirements.
         *
         * For details see: <a href="http://wscg.zcu.cz/wscg2004/Papers_2004_Short/N29.pdf">Aguilera - Perez Algorithm</a>
         *
         * @param rotationSimplex defining data. Rows of this matrix represent points defining
         *                     simplex to perform this rotation around. Points must have their
         *                     position in all 'n' dimensions defined and there must be 'n-1'
         *                     points to define rotation simplex.
         * @param angle        degrees to rotate by objects multiplied by this rotation matrix.
         * @return rotation matrix.
         */
        @JvmStatic
        fun initRotation(rotationSimplex: MatrixI, angle: Int): MatrixI {
            val n = rotationSimplex.columnCount
            require(n >= 2) { "Can't do rotation in $n-dimensional space!" }
            require(rotationSimplex.rowCount == n - 1) { "Insufficient / invalid data! Can't perform rotation." }

            @Suppress("LocalVariableName")
            val M = arrayOfNulls<MatrixI>(n * (n - 1) / 2 + 1)
            val v = arrayOfNulls<MatrixI>(n * (n - 1) / 2 + 1)

            M[0] = MatrixI.initTranslationMatrix(((-rotationSimplex).firstRow().toVector()).toArray()).transpose()
            v[0] = rotationSimplex

            v[1] = (v[0]?.withColumn(n, Array(n - 1) { 1 })!! * M[0]!!).submatrix(emptyArray(), arrayOf(n + 1))

            var result = MatrixI(M[0]!!.wrapped)
            var k = 1
            for (r in 2 until n) {
                for (c in n downTo r) {
                    k += 1
                    M[k - 1] = MatrixI.initPlaneRotation(n + 1, c, c - 1,
                                                         Math.atan2(v[k - 1]!![r - 1][c - 1].toDouble(),
                                                                    v[k - 1]!![r - 1][c - 2].toDouble()).toInt())
                    v[k] = (v[k - 1]?.withColumn(n, Array(n - 1) { 1 })!! * M[k - 1]!!).submatrix(emptyArray(),
                                                                                                  arrayOf(n + 1))
                    result *= M[k - 1]!!
                }
            }
            return (result * MatrixI.initPlaneRotation(n + 1, n - 1, n, angle) * result.inverseUnsafe()).submatrix(
                    arrayOf(n + 1), arrayOf(n + 1))
        }

        /**
         * Initializes a plane rotation matrix.
         *
         * @param size  size/dimensionality of plane rotation matrix.
         * @param a     rotated axis index.
         * @param b     destination axis index.
         * @param angle degrees to rotate in objects multiplied by this rotation matrix.
         * @return plane rotation matrix.
         */
        @JvmStatic
        fun initPlaneRotation(size: Int, a: Int, b: Int, angle: Int): MatrixI {
            return MatrixI(Array(size) { row ->
                Array(size) { column ->
                    when {
                        row == a - 1 && column == a - 1 -> cos(Math.toRadians(angle.toDouble())).roundToInt()
                        row == b - 1 && column == b - 1 -> cos(Math.toRadians(angle.toDouble())).roundToInt()
                        row == a - 1 && column == b - 1 -> -sin(Math.toRadians(angle.toDouble())).roundToInt()
                        row == b - 1 && column == a - 1 -> sin(Math.toRadians(angle.toDouble())).roundToInt()
                        row == column -> 1
                        else -> 0
                    }
                }
            })
        }

        /**
         * Initializes a new translation matrix using array of axial translations.
         *
         * @param location relative location.
         * @return translation matrix.
         */
        @JvmStatic
        fun initTranslationMatrix(location: Array<Int>): MatrixI {
            return MatrixI(Array(location.size + 1) { row ->
                Array(location.size + 1) { column ->
                    when {
                        row == column -> 1
                        column == location.size && row < location.size -> location[row]
                        else -> 0
                    }
                }
            })
        }

        /**
         * Initializes a new translation matrix using [VectorI].
         *
         * @since 3.0.0
         *
         * @param location relative location.
         * @return translation matrix.
         */
        @JvmStatic
        fun initTranslationMatrix(location: VectorI): MatrixI {
            return MatrixI(Array(location.size + 1) { row ->
                Array(location.size + 1) { column ->
                    when {
                        row == column -> 1
                        column == location.size && row < location.size -> location[row]
                        else -> 0
                    }
                }
            })
        }

        /**
         * Initializes a new scaling matrix.
         *
         * @param scale scale.
         * @return scale matrix.
         */
        @JvmStatic
        fun initScalingMatrix(scale: Array<Int>): MatrixI {
            return MatrixI(Array(scale.size) { row ->
                Array(scale.size) { column ->
                    if (row == column) scale[row] else 0
                }
            })
        }

        /**
         * Initializes a new identity matrix.
         *
         * @param n matrix size.
         * @return identity matrix.
         */
        @JvmStatic
        fun initIdentityMatrix(n: Int): MatrixI {
            return MatrixI(Array(n) { row ->
                Array(n) { column ->
                    if (row == column) 1 else 0
                }
            })
        }
    }
}