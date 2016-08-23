/*
 * The MIT License (MIT)
 * Copyright (c) 2016 Tin Švagelj <tin.svagelj.email@gmail.com> a.k.a. Caellian
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */

package hr.caellian.math.util;

import hr.caellian.math.matrix.Matrix;
import hr.caellian.math.matrix.MatrixD;
import scala.*;

import java.lang.Double;

/**
 * Java class used for inverse matrix calculation.
 * This code is implemented in Java because Java for loops perform much faster than Scala
 * ones.
 *
 * You don't need to interact with this class in most cases as it's handled by Matrix
 * trait automatically.
 *
 * This class is based on Apache Math LUDecomposition class but is optimised to work
 * better with this library as it doesn't calculate needless data
 *
 * @since 2.0.0
 * @author Caellian
 */
public class Inverse {

    /**
     * Java implementation of Doolittle LUP matrix decomposition algorithm.
     *
     * @param lu input matrix which will turn into LU data matrix.
     * @param singularityThreshold singularity threshold. This should be a very low number.
     * @return Pivot decomposition data.
     */
    public static int[] doolittleLUP(double[][] lu, double singularityThreshold) {
        if (lu.length != lu[0].length) {
            throw new IllegalArgumentException("requirement failed: LU decomposition of non-square matrices not supported!");
        }

        final int n = lu.length;

        // Pivot
        final int[] p = new int[n];
        for (int i = 0; i < n; i++) {
            p[i] = i;
        }

        // Iterate over columns
        for (int col = 0; col < n; col++) {

            // Upper matrix construction
            for (int row = 0; row < col; row++) {
                final double[] luRow = lu[row];
                double sum = luRow[col];
                for (int i = 0; i < row; i++) {
                    sum -= luRow[i] * lu[i][col];
                }
                luRow[col] = sum;
            }

            // Lower matrix construction
            int max = col; // Permutation row
            double largest = 0;
            for (int row = col; row < n; row++) {
                final double[] luRow = lu[row];
                double sum = luRow[col];
                for (int i = 0; i < col; i++) {
                    sum -= luRow[i] * lu[i][col];
                }
                luRow[col] = sum;

                // maintain best permutation choice
                if (Math.abs(sum) > largest) {
                    largest = Math.abs(sum);
                    max = row;
                }
            }

            // Singularity check
            if (Math.abs(lu[max][col]) < singularityThreshold) {
                throw new IllegalArgumentException("requirement failed: LUP Decomposition impossible for singular matrices!");
            }

            // Pivot if necessary
            if (max != col) {
                final double[] luMax = lu[max];
                final double[] luCol = lu[col];

                double tmp;
                for (int i = 0; i < n; i++) {
                    tmp = luMax[i];
                    luMax[i] = luCol[i];
                    luCol[i] = tmp;
                }

                int temp = p[max];
                p[max] = p[col];
                p[col] = temp;
            }

            // Divide the lower elements by the "winning" diagonal elt.
            final double luDiagonal = lu[col][col];
            for (int row = col + 1; row < n; row++) {
                lu[row][col] /= luDiagonal;
            }
        }

        return p;
    }

    /**
     * Calculates inverse matrix of input matrix. Unwrapped matrix format is used to
     * increase performance.
     * 
     * @param lu input matrix which will turn into LU data matrix.
     * @param singularityThreshold singularity threshold. This should be a very low number.
     * @return inverse matrix of given matrix.
     */
    public static double[][] inverseMatrix(double[][] lu, double singularityThreshold) {
        // Decomposition pivot
        final int[] p = doolittleLUP(lu, singularityThreshold);

        // Size of decomposed matrix
        final int n = lu.length;

        final double[][] b = new double[n][n];

        for (int row = 0; row < n; row++) {
            final double[] bRow = b[row];
            final int pRow = p[row];
            for (int col = 0; col < n; col++) {
                bRow[col] = pRow == col ? 1 : 0;
            }
        }

        // Solve LY = b
        for (int col = 0; col < n; col++) {
            final double[] bpCol = b[col];
            for (int i = col + 1; i < n; i++) {
                final double[] bpI = b[i];
                final double luICol = lu[i][col];
                for (int j = 0; j < n; j++) {
                    bpI[j] -= bpCol[j] * luICol;
                }
            }
        }

        // Solve UX = Y
        for (int col = n - 1; col >= 0; col--) {
            final double[] bpCol = b[col];
            final double luDiag = lu[col][col];
            for (int j = 0; j < n; j++) {
                bpCol[j] /= luDiag;
            }
            for (int i = 0; i < col; i++) {
                final double[] bpI = b[i];
                final double luICol = lu[i][col];
                for (int j = 0; j < n; j++) {
                    bpI[j] -= bpCol[j] * luICol;
                }
            }
        }

        return b;
    }
}
