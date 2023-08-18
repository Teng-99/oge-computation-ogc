import Array._

//  目前考虑到的可以处理浮点数的方法是将代码中复制一遍并将其中所有Int类型都替换成float类型在构建一遍函数，由于是比较机械化的重复这里就没有进行实现
//  所以这里的输入限为整数，实现形式多为二维数组。
object Vector {

  //  对该数组的所有值绝对值
  def abs_get(x: Array[Array[Int]], lin: Int, col: Int) = {
    for (i <- 1 to lin; j <- 1 to col) {
      if (x(i-1)(j-1) > 0) {
        x(i-1)(j-1) = x(i-1)(j-1)
      }
      else {
        x(i-1)(j-1) = 0-x(i-1)(j-1)
      }
    }
  }

  //  对该数组的选定维度进行合并
  def cat(x: Array[Array[Int]], y: Array[Array[Int]], lin: Int, col: Int, pix: Int) = {
    var buffer1 = x.toBuffer
    //  等于0直接合并
    if (pix == 0) {
      for (i <- 1 to lin) {
        buffer1.append(y(i-1))
      }
      buffer1.foreach { a => a.foreach(b => println(b)) }
    }
    //  等于1分别取出对应的维度进行合并
    else if (pix == 1) {
      for (i <- 1 to lin) {
        var layer = buffer1(i - 1)
        var layer1 = layer.toBuffer
        for (j <- 1 to col) {
          layer1.append(y(i - 1)(j - 1))
        }
        println(layer1)
      }
    }
    //  类似上个
    else if (pix == 2) {
      for (i <- 1 to lin) {
        var layer = buffer1(i - 1)
        var layer1 = layer.toBuffer
        for (j <- 1 to col) {
          layer1.append(y(i - 1)(j - 1))
        }
        println(layer1)
      }
    }
  }

  //  求数组长度
  def len(lin: Int, col: Int) = {
    println("该二维数组的长度为：")
    println(lin*col)
  }

  //  对数组对应的维度进行切割，其中-1代表整个维度
  def cut(x: Array[Array[Int]], lin: Int, col: Int, pix1: Int, pix2: Int) = {
    if (pix1 > lin | pix1 < -1 | pix2 > col | pix2 < -1 ) println("输入的维度数据有误。")
    if (pix1 == -1) {
      if (pix2 == -1) {
        x.foreach { a => a.foreach(b => println(b)) }
      }
      else {
        //  利用for循环切割
        for (i <- 1 to lin) {
          println(x(i-1)(pix2))
        }
      }
    }
    else {
      if (pix2 == -1) {
        for (i <- 1 to col) {
          println(x(pix1)(i - 1))
        }
      }
      else {
        println(x(pix1)(pix2))
      }
    }
  }

  //  将数组的左右部分分割
  def divide(x: Array[Array[Int]], lin: Int, col: Int, loc: Int) = {
    if (loc > col | loc < 1) print("输入的数据错误。")
    var arr1 = ofDim[Int](0)
    var arr2 = ofDim[Int](0)
    var layer1 = arr1.toBuffer
    var layer2 = arr2.toBuffer
    for (i <- 1 to lin; j <- 1 to loc){
      layer1.append(x(i-1)(j-1))
    }
    println("分割后左边的数列：")
    println(layer1)
    for (i <- 1 to lin; j <- loc+1 to col) {
      layer2.append(x(i - 1)(j - 1))
    }
    println("分割后右边的数列：")
    println(layer2)
  }

  //  对数组每个数字添加一个值
  def add_num(x: Array[Array[Int]], lin: Int, col: Int, num: Int) = {
    for (i <- 1 to lin; j <- 1 to col) {
      x(i - 1)(j - 1) += num
    }
    println("增加后的结果为：")
    x.foreach { a => a.foreach(b => println(b)) }
  }

  //  对数组添加另一个相同形状数组的值
  def add_arr(x: Array[Array[Int]], y: Array[Array[Int]], lin: Int, col: Int) = {
    for (i <- 1 to lin; j <- 1 to col) {
      x(i - 1)(j - 1) += y(i - 1)(j - 1)
    }
    println("增加后的结果为：")
    x.foreach { a => a.foreach(b => println(b)) }
  }

  //  对数组每个数字乘以一个值
  def multiple_num(x: Array[Array[Int]], lin: Int, col: Int, num: Int) = {
    for (i <- 1 to lin; j <- 1 to col) {
      x(i - 1)(j - 1) *= num
    }
    println("相乘后的结果为：")
    x.foreach { a => a.foreach(b => println(b)) }
  }

  //  对数组乘以另一个相同形状数组的值
  def multiple_arr(x: Array[Array[Int]], y: Array[Array[Int]], lin: Int, col: Int) = {
    for (i <- 1 to lin; j <- 1 to col) {
      x(i - 1)(j - 1) *= y(i - 1)(j - 1)
    }
    println("相乘后的结果为：")
    x.foreach { a => a.foreach(b => println(b)) }
  }

  //  对数组每个数字减去一个值
  def substract_num(x: Array[Array[Int]], lin: Int, col: Int, num: Int) = {
    for (i <- 1 to lin; j <- 1 to col) {
      x(i - 1)(j - 1) -= num
    }
    println("相减后的结果为：")
    x.foreach { a => a.foreach(b => println(b)) }
  }

  //  对数组减去另一个相同形状数组的值
  def substract_arr(x: Array[Array[Int]], y: Array[Array[Int]], lin: Int, col: Int) = {
    for (i <- 1 to lin; j <- 1 to col) {
      x(i - 1)(j - 1) -= y(i - 1)(j - 1)
    }
    println("相减后的结果为：")
    x.foreach { a => a.foreach(b => println(b)) }
  }

  //  对数组进行冒泡排序
  def popSort(arr: Array[Int]) = {
    var temp = 0
    var count = 0
    for (i <- 0 to arr.length - 1) {
      for (j <- 0 to arr.length - 2 - i) {
        if (arr(j) > arr(j + 1)) {
          var temp = arr(j)
          arr(j) = arr(j + 1)
          arr(j + 1) = temp

        }
        count += 1
      }
    }
    arr.foreach(println)
  }

  def main(args: Array[String]): Unit = {
    println("请输入您所要二维数组的列数：")
    val col = scala.io.StdIn.readInt()
    println("请输入您所要二维数组的行数：")
    val lin = scala.io.StdIn.readInt()
    var matrix = ofDim[Int](lin, col)
    println("请按行依次输入二维数组的每个位置的数据：(整数)")
    //  利用双重for循环对数组每个位置的值进行输入
    for (i <- 1 to col; j <- 1 to lin) {
      matrix(i-1)(j-1) = scala.io.StdIn.readInt()
    }
    println("得到的二维数组为：(整数)")
    matrix.foreach { x => x.foreach(y => println(y)) }
    len(lin, col)
    println("接下来对其进行求绝对值操作：")
    abs_get(matrix, lin, col)
    matrix.foreach { x => x.foreach(y => println(y)) }
    var matrix1 = ofDim[Int](lin, col)
    println("接下来对其进行求cat操作，新创建一个相同形状的二维数组，请输入数据：")
    for (i <- 1 to col; j <- 1 to lin) {
      matrix1(i - 1)(j - 1) = scala.io.StdIn.readInt()
    }
    println("得到的二维数组为：(整数)")
    matrix1.foreach { x => x.foreach(y => println(y)) }
    println("请输入您需要用哪个维度合并两个二维数组：")
    val pix1 = scala.io.StdIn.readInt()
    if (pix1 != 0 & pix1 != 1 & pix1 != 2) println("输入数据错误")
    println("合并结果如下所示")
    cat(matrix, matrix1, lin, col, pix1)
    println("接下来对第一个二维数组进行切割操作，请输入想要进行切割的维度：")
    println("第一个维度：")
    val pix_1 = scala.io.StdIn.readInt()
    println("第二个维度：")
    val pix_2 = scala.io.StdIn.readInt()
    cut(matrix, lin, col, pix_1, pix_2)
    println("接下来对第一个二维数组进行分割操作，请输入想要进行分割的位置：")
    val loc = scala.io.StdIn.readInt()
    divide(matrix, lin, col, loc)
    println("接下来对第一个二维数组进行add单个数字的操作，请输入想要增加的数字：")
    val num = scala.io.StdIn.readInt()
    add_num(matrix, lin, col, num)
    var matrix2 = ofDim[Int](lin, col)
    println("接下来对其进行add另一个二维数组的操作，新创建一个相同形状的二维数组，请输入数据：")
    for (i <- 1 to col; j <- 1 to lin) {
      matrix2(i - 1)(j - 1) = scala.io.StdIn.readInt()
    }
    println("得到的二维数组为：(整数)")
    matrix2.foreach { x => x.foreach(y => println(y)) }
    add_arr(matrix, matrix2, lin, col)
    println("接下来对第一个二维数组进行multiple单个数字的操作，请输入想要乘以的数字：")
    val num1 = scala.io.StdIn.readInt()
    multiple_num(matrix, lin, col, num1)
    var matrix3 = ofDim[Int](lin, col)
    println("接下来对其进行multiple另一个二维数组的操作，新创建一个相同形状的二维数组，请输入数据：")
    for (i <- 1 to col; j <- 1 to lin) {
      matrix3(i - 1)(j - 1) = scala.io.StdIn.readInt()
    }
    println("得到的二维数组为：(整数)")
    matrix3.foreach { x => x.foreach(y => println(y)) }
    multiple_arr(matrix, matrix3, lin, col)
    println("接下来对第一个二维数组进行substract单个数字的操作，请输入想要减去的数字：")
    val num2 = scala.io.StdIn.readInt()
    substract_num(matrix, lin, col, num2)
    var matrix4 = ofDim[Int](lin, col)
    println("接下来对其进行substract另一个二维数组的操作，新创建一个相同形状的二维数组，请输入数据：")
    for (i <- 1 to col; j <- 1 to lin) {
      matrix4(i - 1)(j - 1) = scala.io.StdIn.readInt()
    }
    println("得到的二维数组为：(整数)")
    matrix4.foreach { x => x.foreach(y => println(y)) }
    substract_arr(matrix, matrix4, lin, col)
    println("接下来对一维数组进行排序操作，请输入数组的长度：")
    val len1 = scala.io.StdIn.readInt()
    println("接下来依次输入数组的值：")
    var arr1 = ofDim[Int](0)
    var layer1 = arr1.toBuffer
    for (i <- 1 to len1) {
      val num1 = scala.io.StdIn.readInt()
      layer1.append(num1)
    }
    var layer2 = layer1.toArray
    println("得到的数组为:")
    println(layer1)
    println("排序后的结果为:")
    popSort(layer2)

  }
}
