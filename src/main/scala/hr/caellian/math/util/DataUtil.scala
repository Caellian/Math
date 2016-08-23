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

package hr.caellian.math.util

/**
  * Utility object containing functions for data comparison and hash code generation.
  *
  * @author Caellian
  */
object DataUtil {
  /**
    * @param array object to calculate hash code of.
    * @return hash code of given array.
    */
  def hashCode(array: Array[Double]): Int = {
    var res = 1
    for (elem <- array) {
      res = 37 * res + elem.##
    }
    res
  }

  /**
    * @param array object to calculate hash code of.
    * @return hash code of given 2D array.
    */
  def hashCode(array: Array[Array[Double]]): Int = {
    var res = 1
    for (elem <- array) {
      res = 37 * res + hashCode(elem)
    }
    res
  }

  /**
    * @param first  first array.
    * @param second second array.
    * @return true if 2D arrays are equal.
    */
  def equals(first: Array[Array[Double]], second: Array[Array[Double]]): Boolean = {
    if (first.length != second.length) {
      false
    } else {
      for (i <- first.indices) {
        if (first(i) sameElements second(i)) {
          return false
        }
      }
      true
    }
  }

  /**
    * @param array object to calculate hash code of.
    * @return hash code of given array.
    */
  def hashCode(array: Array[Float]): Int = {
    var res = 1
    for (elem <- array) {
      res = 37 * res + elem.##
    }
    res
  }

  /**
    * @param array object to calculate hash code of.
    * @return hash code of given 2D array.
    */
  def hashCode(array: Array[Array[Float]]): Int = {
    var res = 1
    for (elem <- array) {
      res = 37 * res + hashCode(elem)
    }
    res
  }

  /**
    * @param first  first array.
    * @param second second array.
    * @return true if 2D arrays are equal.
    */
  def equals(first: Array[Array[Float]], second: Array[Array[Float]]): Boolean = {
    if (first.length != second.length) {
      false
    } else {
      for (i <- first.indices) {
        if (first(i) sameElements second(i)) {
          return false
        }
      }
      true
    }
  }

  /**
    * @param array object to calculate hash code of.
    * @return hash code of given array.
    */
  def hashCode(array: Array[Int]): Int = {
    var res = 1
    for (elem <- array) {
      res = 37 * res + elem.##
    }
    res
  }

  /**
    * @param array object to calculate hash code of.
    * @return hash code of given 2D array.
    */
  def hashCode(array: Array[Array[Int]]): Int = {
    var res = 1
    for (elem <- array) {
      res = 37 * res + hashCode(elem)
    }
    res
  }

  /**
    * @param first  first array.
    * @param second second array.
    * @return true if 2D arrays are equal.
    */
  def equals(first: Array[Array[Int]], second: Array[Array[Int]]): Boolean = {
    if (first.length != second.length) {
      false
    } else {
      for (i <- first.indices) {
        if (first(i) sameElements second(i)) {
          return false
        }
      }
      true
    }
  }

  /**
    * Switches values of two arrays in specified range.
    *
    * @param range range to switch values of arrays in.
    * @param a     first array.
    * @param b     second array.
    * @tparam T type of arrays.
    */
  def rangeSwitch[T](range: Range, a: Array[T], b: Array[T]): Unit = {
    for (i <- range) {
      val tmp = a(i)
      a(i) = b(i)
      b(i) = tmp
    }
  }
}
