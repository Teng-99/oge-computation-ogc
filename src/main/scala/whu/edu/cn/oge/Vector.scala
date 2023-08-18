package whu.edu.cn.oge

import scala.math.Numeric.Implicits._
import scala.reflect.ClassTag
import scala.math.Ordered.orderingToOrdered
/**
 * Represents a one/two-dimensional Vector with various operations.
 *
 * @param values Arrays containing the vector data
 * @tparam T Type of the elements in the vector
 */
class Vector[T: Numeric : ClassTag](values: Array[T]*) {
  private val data: Array[Array[T]] = values.toArray
  /**
   * Accesses an element in the vector by its indices.
   *
   * @param i Row index
   * @param j Column index
   * @return Element at the specified indices
   */
  def apply(i: Int, j: Int): T = data(i)(j) // For two-dimensional array access
  /**
   * Returns the number of rows in the vector.
   *
   * @return Number of rows
   */
  def numRows: Int = data.length
  /**
   * Returns the number of columns in the vector.
   *
   * @return Number of columns
   */
  def numCols: Int = if (numRows > 0) data(0).length else 0
  /**
   * Returns the length (number of rows) of the 1d vector.
   *
   * @return Vector length
   */
  def length: Int = data.length
  /**
   * Returns a string representation of the vector.
   *
   * @return String representation of the vector
   */
  override def toString: String = {
    if (data.length == 1) {
      data(0).mkString("[", ", ", "]\n")
    } else {
      "[" + data.map(row => row.mkString("[", ", ", "]")).mkString(",") + "]\n"
    }
  }
  /**
   * Returns a new vector with all elements converted to their absolute values.
   *
   * @return A new vector with absolute values of elements
   */
  // Convert all elements to their absolute values
  def abs: Vector[T] = {
    val newData = data.map(row => row.map(_.abs))
    new Vector[T](newData: _*)
  }
  /**
   * Concatenates two vectors together.
   *
   * @param other Another vector to concatenate
   * @return A new vector resulting from concatenation
   */
  // Concatenate two vectors
  def cat(other: Vector[T]): Vector[T] = {
    val concatenatedData = data ++ other.data
    new Vector[T](concatenatedData: _*)
  }
  /**
   * Creates a new vector by cutting based on specified indices.
   *
   * @param indices Indices to cut the vector at
   * @return A new vector containing the specified elements
   * @throws IllegalArgumentException if the number of indices is greater than the max number of dimensions
   *                                  or if an invalid index is provided for a dimension
   */
  // Cut the vector based on specified indices
  def cut(indices: Int*): Vector[T] = {
    if (indices.length > data.length) {
      throw new IllegalArgumentException("Number of indices must less than or equal the max number of dimensions")
    }

    var newData: Array[Array[T]] = data
    if (data.length == 1){
      val idx = indices(0)
      if (idx != -1) {
        if (idx < -1 || idx >= newData(0).length) {
          throw new IllegalArgumentException(s"Invalid index for dimension 0: $idx")
        }
        else{
          newData = newData.map(row => Array(row(idx)))
        }
      }
    }
    else{
      for (dim <- indices.indices) {
        val idx = indices(dim)
        if (idx != -1) {
          if (idx < -1 || idx >= newData(dim).length) {
            throw new IllegalArgumentException(s"Invalid index for dimension $dim: $idx")
          }
          else if(dim == 0){
            newData = Array(newData(idx))
          }
          else if(dim == 1){
            newData = newData.map(row => Array(row(idx)))
          }
        }
      }
    }
    new Vector[T](newData: _*)
  }
  /**
   * Returns a new vector with values sorted in ascending order based on a specified dimension.
   *
   * @param ordering Implicit ordering to compare elements
   * @return A new vector with sorted values in the specified dimension
   */
  // Sort the vector based on values in the specified dimension
  def sort()(implicit ordering: Ordering[T]): Vector[T] = {
    var sortedData = data.map(row => row.sorted)
    for (i <- 0 until data(0).length){
      val tmp = sortedData.sortWith((arr1, arr2) => arr1(i) < arr2(i))
      if (!(tmp sameElements sortedData)){
        sortedData = tmp
      }
    }
    new Vector[T](sortedData: _*)
  }
  /**
   * Adds another vector element-wise to this vector.
   *
   * @param other Another vector to be added
   * @return A new vector resulting from element-wise addition
   * @throws IllegalArgumentException if the dimensions of the vectors do not match
   */
  // Add two vectors element-wise
  def add(other: Vector[T]): Vector[T] = {
    if (numRows != other.numRows || numCols != other.numCols) {
      throw new IllegalArgumentException("Vectors must have the same dimensions for addition")
    }

    val newData = data.zip(other.data).map { case (row1, row2) =>
      row1.zip(row2).map { case (elem1, elem2) =>
        implicitly[Numeric[T]].plus(elem1, elem2)
      }
    }

    new Vector[T](newData: _*)
  }
  /**
   * Adds a scalar value to each element of the vector.
   *
   * @param scalar Scalar value to be added
   * @return A new vector with scalar added to each element
   */
  // Add a scalar value to each element
  def add(scalar: T): Vector[T] = {
    val newData = data.map { row =>
      row.map { elem =>
        implicitly[Numeric[T]].plus(elem, scalar)
      }
    }

    new Vector[T](newData: _*)
  }
  /**
   * Subtracts another vector element-wise from this vector.
   *
   * @param other Another vector to be subtracted
   * @return A new vector resulting from element-wise subtraction
   * @throws IllegalArgumentException if the dimensions of the vectors do not match
   */
  // subtract two vectors element-wise
  def subtract(other: Vector[T]): Vector[T] = {
    if (numRows != other.numRows || numCols != other.numCols) {
      throw new IllegalArgumentException("Vectors must have the same dimensions for addition")
    }

    val newData = data.zip(other.data).map { case (row1, row2) =>
      row1.zip(row2).map { case (elem1, elem2) =>
        implicitly[Numeric[T]].minus(elem1, elem2)
      }
    }
    new Vector[T](newData: _*)
  }
  /**
   * Subtracts a scalar value from each element of the vector.
   *
   * @param scalar Scalar value to be subtracted
   * @return A new vector with scalar subtracted from each element
   */
  // subtract a scalar value to each element
  def subtract(scalar: T): Vector[T] = {
    val newData = data.map { row =>
      row.map { elem =>
        implicitly[Numeric[T]].minus(elem, scalar)
      }
    }
    new Vector[T](newData: _*)
  }
  /**
   * Multiplies this vector element-wise with another vector.
   *
   * @param other Another vector for element-wise multiplication
   * @return A new vector resulting from element-wise multiplication
   * @throws IllegalArgumentException if the dimensions of the vectors do not match
   */
  // multiply two vectors element-wise
  def multiply(other: Vector[T]): Vector[T] = {
    if (numRows != other.numRows || numCols != other.numCols) {
      throw new IllegalArgumentException("Vectors must have the same dimensions for addition")
    }

    val newData = data.zip(other.data).map { case (row1, row2) =>
      row1.zip(row2).map { case (elem1, elem2) =>
        implicitly[Numeric[T]].times(elem1, elem2)
      }
    }
    new Vector[T](newData: _*)
  }
  /**
   * Multiplies each element of the vector by a scalar value.
   *
   * @param scalar Scalar value for element-wise multiplication
   * @return A new vector with elements multiplied by the scalar
   */
  // multiply a scalar value to each element
  def multiply(scalar: T): Vector[T] = {
    val newData = data.map { row =>
      row.map { elem =>
        implicitly[Numeric[T]].times(elem, scalar)
      }
    }
    new Vector[T](newData: _*)
  }
  /**
   * Divides this vector element-wise by another vector.
   *
   * @param other Another vector for element-wise division
   * @return A new vector resulting from element-wise division
   * @throws IllegalArgumentException if the dimensions of the vectors do not match
   */
  // divide two vectors element-wise
  def divide(other: Vector[T]): Vector[T] = {
    if (numRows != other.numRows || numCols != other.numCols) {
      throw new IllegalArgumentException("Vectors must have the same dimensions for division")
    }

    val newData = data.zip(other.data).map { case (row1, row2) =>
      row1.zip(row2).map { case (elem1, elem2) =>
        if (implicitly[Numeric[T]].toDouble(elem2) != 0.0) {
          val bdElem1 = BigDecimal(elem1.toString)
          val bdElem2 = BigDecimal(elem2.toString)
          val result = bdElem1 / bdElem2
          result.toDouble.asInstanceOf[T]
        } else {
          implicitly[Numeric[T]].zero
        }
      }
    }
    new Vector[T](newData: _*)
  }
  /**
   * Divides each element of the vector by a scalar value.
   *
   * @param scalar Scalar value for element-wise division
   * @return A new vector with elements divided by the scalar
   */
  // divide a scalar value to each element,if scalar == zero, result = 0
  def divide(scalar: T): Vector[T] = {
    val newData = data.map { row =>
      row.map { elem =>
        if (implicitly[Numeric[T]].toDouble(scalar) != 0.0) {
          val bdElem1 = BigDecimal(elem.toString)
          val bdElem2 = BigDecimal(scalar.toString)
          val result = bdElem1 / bdElem2
          result.toDouble.asInstanceOf[T]
        } else {
          implicitly[Numeric[T]].zero
        }
      }
    }
    new Vector[T](newData: _*)
  }


}

object Vector {
  /**
   * Creates a new Vector instance from the provided arrays.
   *
   * @param values Arrays containing the vector data
   * @tparam T Type of the elements in the vector
   * @return A new Vector instance
   */
  def apply[T: Numeric : ClassTag](values: Array[T]*): Vector[T] = new Vector[T](values: _*)
  /**
   * Combines two vectors into a new vector with the specified link type.
   *
   * @param v1 First vector
   * @param v2 Second vector
   * @param linkType Type of linking (0: stack, 1: concatenate)
   * @tparam T Type of the elements in the vectors
   * @return A new vector resulting from the combination
   * @throws IllegalArgumentException if the number of columns in the vectors does not match
   *                                  or an invalid link type is provided
   */
  // Combine two vectors into a new vector with specified link type
  def cat[T: Numeric : ClassTag](v1: Vector[T], v2: Vector[T], linkType: Int = 0): Vector[T] = {
    if (v1.numCols != v2.numCols) {
      throw new IllegalArgumentException("Number of columns must be the same for linking")
    }

    val combinedData = linkType match {
      case 0 => v1.data ++ v2.data
      case 1 => v1.data.zip(v2.data).map { case (row1, row2) => row1 ++ row2 }
      case _ => throw new IllegalArgumentException("Invalid link type")
    }

    new Vector[T](combinedData: _*)
  }
}



object TestObject extends App {
  // Create vectors
  val intVector1 = Vector[Int](Array(1),Array(2),Array(3))
  val intVector2 = Vector[Int](Array(4),Array(5),Array(6))

  // Print the original vectors
  println("Original Vectors:")
  println(intVector1)
  println(intVector2)

  // Combine vectors using different link types and print the results
  val catVector1 = Vector.cat(intVector1, intVector2, 0)
  println("Cat 1 Vector (Link Type 0):")
  println(catVector1)

  val catVector2 = Vector.cat(intVector1, intVector2, 1)
  println("Cat 2 Vector (Link Type 1):")
  println(catVector2)

  // Create vectors
  val intVector3 = Vector[Double](Array(1.1,2.2,3.3))
  val intVector4 = Vector[Double](Array(4.4,5.5,6.6))

  // Print the original vectors
  println("Original Vectors:")
  println(intVector3)
  println(intVector4)

  // Combine vectors using different link types and print the results
  val catVector3 = Vector.cat(intVector3, intVector4, 0)
  println("Cat 3 Vector (Link Type 0):")
  println(catVector3)

  val catVector4 = Vector.cat(intVector3, intVector4, 1)
  println("Cat 4 Vector (Link Type 1):")
  println(catVector4)


  // Create a 2D int vector using Vector class
  val intVectorOri = Vector[Int](Array(0, 1, 2), Array(3, 4, 5))

  // Print the original vector
  println("Original Vector:")
  println(intVectorOri)

  // Cut the vector based on specified indices and print the result
  val cutVector1 = intVectorOri.cut(-1, -1)
  println("Cut Vector ([-1, -1]):")
  println(cutVector1)

  val cutVector2 = intVectorOri.cut(-1, 0)
  println("Cut Vector ([-1, 0]):")
  println(cutVector2)

  val cutVector3 = intVectorOri.cut(1, -1)
  println("Cut Vector ([1, -1]):")
  println(cutVector3)

  val floatVector = Vector[Double](Array(0.0, 1.0, 2.0))
  val cutVector4 = floatVector.cut(-1)
  println("Cut Vector ([-1]):")
  println(cutVector4)

  val cutVector5 = floatVector.cut(0)
  println("Cut Vector ([0]):")
  println(cutVector5)

  val cutVector6 = floatVector.cut(2)
  println("Cut Vector ([2]):")
  println(cutVector6)

  val intVectorForLen = Vector[Int](Array(0, 1, 2), Array(3, 4, 5))
  println(intVectorForLen.length)

  val intVectorForSort = Vector[Int](Array(0, 5, 2), Array(0, 3, 2))
  println(intVectorForSort.sort())


  // Create two 2D int vectors using Vector class
  val vector1ForAdd = Vector[Int](
    Array(5, 6),
    Array(5, 8)
  )

  val vector2ForAdd = Vector[Int](
    Array(7, 8),
    Array(5, 6)
  )

  // Add the vectors and print the result
  var resultForAdd = vector1ForAdd.add(vector2ForAdd)
  println("Vector Addition Result:")
  println(resultForAdd)

  // Create a 2D vector using Vector class
  val vector3ForAdd = Vector[Double](
    Array(0, 1, 2),
    Array(3, 4, 5)
  )

  // Add a scalar value and print the result
  val result2ForAdd = vector3ForAdd.add(1)
  println("Vector Addition with Scalar Result:")
  println(result2ForAdd)



  // Create two 2D int vectors using Vector class
  val vector1ForSub = Vector[Double](
    Array(10, 6),
    Array(5, 8)
  )

  val vector2ForSub = Vector[Double](
    Array(7, 8),
    Array(5, 6)
  )

  // subtract the vectors and print the result
  var resultForSub = vector1ForSub.subtract(vector2ForSub)
  println("Vector subtraction Result:")
  println(resultForSub)

  // Create a 2D vector using Vector class
  val vector3ForSub = Vector[Double](
    Array(0, 1, 2),
    Array(3, 4, 5)
  )

  // subtract a scalar value and print the result
  val result2ForSub = vector3ForSub.subtract(1)
  println("Vector subtraction with Scalar Result:")
  println(result2ForSub)



  // Create two 2D int vectors using Vector class
  val vector1ForMul = Vector[Double](
    Array(10, 6),
    Array(5, 8)
  )

  val vector2ForMul = Vector[Double](
    Array(7, 8),
    Array(5, 6)
  )

  // multiply the vectors and print the result
  var resultForMul = vector1ForMul.multiply(vector2ForMul)
  println("Vector multiplication Result:")
  println(resultForMul)

  // Create a 2D vector using Vector class
  val vector3ForMul = Vector[Double](
    Array(0, 1, 2),
    Array(3, 4, 5)
  )

  // multiply a scalar value and print the result
  val result2ForMul = vector3ForMul.multiply(3)
  println("Vector multiplication with Scalar Result:")
  println(result2ForMul)



  // Create two 2D int vectors using Vector class
  val vector1ForDiv = Vector[Double](
    Array(10, 6),
    Array(2.5, 3)
  )

  val vector2ForDiv = Vector[Double](
    Array(2, 3),
    Array(5, 8)
  )

  // divide the vectors and print the result
  var resultForDiv = vector1ForDiv.divide(vector2ForDiv)
  println("Vector division Result:")
  println(resultForDiv)

  // Create a 2D vector using Vector class
  val vector3ForDiv = Vector[Double](
    Array(9, 1, 15),
    Array(3, 6, 0)
  )

  // divide a scalar value and print the result
  val result2ForDiv = vector3ForDiv.divide(4)
  println("Vector division with Scalar Result:")
  println(result2ForDiv)

  // divide a scalar value and print the result
  val result3ForDiv = vector3ForDiv.divide(0)
  println("Vector division with Scalar Result(0 divisor):")
  println(result3ForDiv)
}