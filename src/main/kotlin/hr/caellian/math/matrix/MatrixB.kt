package hr.caellian.math.matrix

import hr.caellian.math.internal.DataUtil.transpose
import hr.caellian.math.vector.Vector
import hr.caellian.math.vector.VectorB
import java.nio.Buffer
import java.nio.ByteBuffer
import java.nio.ByteOrder
import kotlin.math.roundToInt

class MatrixB @JvmOverloads constructor(override var wrapped: Array<Array<Boolean>> = emptyArray(), vertical: Boolean = false) : Matrix<Boolean>() {

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
    @JvmOverloads
    constructor(size: Int, default: Boolean = false) : this(Array(size) { _ -> Array(size) { default } })

    /**
     * Creates a new matrix containing given data.
     *
     * @param rows    number of rows matrix should store.
     * @param columns number of columns matrix should store.
     */
    @JvmOverloads
    constructor(rows: Int, columns: Int, default: Boolean = false) : this(
            Array(rows) { _ -> Array(columns) { default } })

    /**
     * Creates a new matrix using collection values.
     *
     * @param values      values to create a new matrix from.
     * @return created matrix.
     */
    constructor(values: Collection<Collection<Boolean>>) : this(values.map { it.toTypedArray() }.toTypedArray())

    /**
     * Creates a new matrix using buffer values.
     *
     * @param buffer      buffer to create a new matrix from.
     * @param rowCount    number of rows new matrix will have.
     * @param columnCount number of columns new matrix will have.
     * @return created matrix.
     */
    constructor(buffer: ByteBuffer, rowCount: Int, columnCount: Int) : this(
            Array(rowCount) { _ -> Array(columnCount) { buffer.get() == 1.toByte() } })

    /**
     * Creates a new square matrix using buffer values.
     *
     * This constructor depends on buffer capacity. Make sure square root of your buffer's capacity is equal to row and
     * column count.
     *
     * @param buffer buffer to create a new matrix from.
     * @return created matrix.
     */
    constructor(buffer: ByteBuffer) : this(
            buffer.let { _ ->
                val rows = Math.sqrt(buffer.capacity().toDouble())
                require(rows - rows.roundToInt() == 0.0) { "Acquired buffer can't be used to create a square matrix." }
                val rowCount = rows.roundToInt()
                Array(rowCount) { _ -> Array(rowCount) { buffer.get() == 1.toByte() } }
            }
    )

    /**
     * @return matrix with logical 'not' performed on values of this matrix.
     */
    override fun not() = MatrixB(Array(rowCount) { row -> Array(columnCount) { column -> !wrapped[row][column] } })

    /**
     * Performs a logical `and` operation between values of this matrix and the [other] one.
     */
    infix fun and(other: MatrixB): MatrixB {
        require(columnCount == other.columnCount && rowCount == other.rowCount) { "Invalid argument matrix size: ${other.rowCount}x${other.columnCount}!" }

        return MatrixB(Array(rowCount) { row ->
            Array(columnCount) { column -> wrapped[row][column] && other.wrapped[row][column] }
        })
    }

    /**
     * Performs a logical `or` operation between values of this matrix and the [other] one.
     */
    infix fun or(other: MatrixB): MatrixB {
        require(columnCount == other.columnCount && rowCount == other.rowCount) { "Invalid argument matrix size: ${other.rowCount}x${other.columnCount}!" }

        return MatrixB(Array(rowCount) { row ->
            Array(columnCount) { column -> wrapped[row][column] || other.wrapped[row][column] }
        })
    }

    /**
     * Performs a logical `xor` operation between values of this matrix and the [other] one.
     */
    infix fun xor(other: MatrixB): MatrixB {
        require(columnCount == other.columnCount && rowCount == other.rowCount) { "Invalid argument matrix size: ${other.rowCount}x${other.columnCount}!" }

        return MatrixB(Array(rowCount) { row ->
            Array(columnCount) { column -> wrapped[row][column] xor other.wrapped[row][column] }
        })
    }

    /**
     * @return true if any field in matrix is true.
     */
    fun any(): Boolean {
        wrapped.forEach { outer ->
            outer.forEach {
                if (it) return true
            }
        }
        return false
    }

    /**
     * @return true if any field in matrix is false.
     */
    fun anyFalse(): Boolean {
        wrapped.forEach { outer ->
            outer.forEach {
                if (!it) return true
            }
        }
        return false
    }

    /**
     * Switches two rows together.
     *
     * @param rowA row to be switched with rowB.
     * @param rowB row to be switched with rowA.
     * @return resulting matrix.
     */
    override fun switchRows(rowA: Int, rowB: Int): Matrix<Boolean> {
        require(rowA in 0..rowCount && rowB in 0..rowCount && rowA != rowB) { "Illegal row argument(s)!" }
        return MatrixB(Array(rowCount) { row ->
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
     *
     * @since 3.0.0
     */
    override fun switchColumns(columnA: Int, columnB: Int): Matrix<Boolean> {
        require(columnA in 0..columnCount && columnB in 0..columnCount && columnA != columnB) { "Illegal column argument(s)!" }
        return MatrixB(Array(rowCount) { row ->
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
     * Inserts given row data at given index shifting rest of the matrix to the next index.
     *
     * @param index index at which added row data will be stored.
     * @param data  row data to store at given index.
     * @return new matrix with extended data.
     */
    override fun withRow(index: Int, data: Array<Boolean>): Matrix<Boolean> {
        require(data.size == columnCount) { "Illegal row array size! Should be $columnCount." }
        return MatrixB(Array(rowCount + 1) { row ->
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
    override fun withColumn(index: Int, data: Array<Boolean>): Matrix<Boolean> {
        require(data.size == rowCount) { "Illegal column array size! Should be $rowCount." }
        return MatrixB(Array(rowCount) { row ->
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
     * @return funined submatrix.
     */
    override fun submatrix(deletedRows: Array<Int>, deletedColumns: Array<Int>): Matrix<Boolean> {
        require(deletedRows.all { it in 0..rowCount } && deletedColumns.all { it in 0..columnCount }) { "Tried to delete rows which don't exist." }

        val keptRows = (0..rowCount).toList().filterNot { deletedRows.contains(it) }
        val keptColumns = (0..columnCount).toList().filterNot { deletedColumns.contains(it) }

        return MatrixB(Array(keptRows.size) { row ->
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
    override fun toVector(): Vector<Boolean> {
        require(columnCount == 1 || rowCount == 1 && !(columnCount > 1 && rowCount > 1)) { "Matrix cannot be turned into a vector!" }
        return if (columnCount > rowCount) {
            VectorB(firstRow()[0])
        } else {
            VectorB(firstColumn().transpose()[0])
        }
    }

    /**
     * Constructs a new vector out of any matrix dismissing extra data.
     *
     * @return vector containing only first column of matrix data.
     */
    override fun forceToVector(): VectorB = VectorB(firstColumn().transpose()[0])

    /**
     * @return a new matrix containing only the first row of this matrix.
     */
    override fun firstRow(): MatrixB = MatrixB(
            Array(1) { row -> Array(columnCount) { column -> wrapped[row][column] } })

    /**
     * @return a new matrix containing only the first column of this matrix.
     */
    override fun firstColumn(): MatrixB = MatrixB(
            Array(rowCount) { row -> Array(1) { column -> wrapped[row][column] } })

    /**
     * @return transposed matrix.
     */
    override fun transpose(): MatrixB = MatrixB(wrapped.transpose())

    /**
     * Returns copy of data of this Matrix as a 2D array.
     *
     * @return 2D array containing data of this matrix.
     */
    override fun toArray(): Array<Array<Boolean>> = Array(rowCount) { row ->
        Array(columnCount) { column -> wrapped[row][column] }
    }

    /**
     * @return clone of T type.
     */
    override fun replicated(): MatrixB = MatrixB(toArray())

    /**
     * Creates a new instance of wrapper containing given data.
     *
     * @param data data of new wrapper.
     * @return new instance of wrapper containing argument data.
     */
    override fun withData(wrapped: Array<Array<Boolean>>) = MatrixB(wrapped)

    /**
     * @return type supported by this class.
     */
    override fun getTypeClass() = Boolean::class

    /**
     * @return [Buffer] containing data of represented object.
     */
    override fun toBuffer(): Buffer {
        return ByteBuffer.allocateDirect((columnCount * rowCount) shl 2).order(ByteOrder.nativeOrder())
                .also { buffer ->
                    wrapped.forEach { it.forEach { inner -> buffer.put(if (inner) 1.toByte() else 0.toByte()) } }
                    buffer.flip()
                }
    }
}