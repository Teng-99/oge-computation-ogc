import Array._
import scala.collection.mutable.Map

//  目前考虑到的可以处理其他类型的关键字和值的方式同上，改变函数中对应的[String, Int]的值后重新构建一遍函数，由于是比较机械化的重复这里就没有进行实现
//  所以这里的输入的字典限为[String, Int]的形式。
object Main {
  //  创建字典
  def Dictionary(): Map[String, Int] = {
    val map1 = Map[String, Int]()
    var arr1 = ofDim[String](0)
    var arr2 = ofDim[Int](0)
    var layer1 = arr1.toBuffer
    var layer2 = arr2.toBuffer
    //  利用while循环不断输入关键字和值直到结束
    println("输入你想要生成的字典的关键字,结束时请输入finish：（建议：Jack;Rose;Andy;John）")
    var count1 = 0
    while (count1 == 0) {
      val keys = scala.io.StdIn.readLine()
      if (keys == "finish") {
        count1 += 1
      }
      else {
        layer1.append(keys)
      }
    }
    println("得到的关键字数组为：")
    println(layer1)
    println("输入你想要生成的字典的值,结束时请输入0：（建议：18;21;16;28）")
    var count2 = 0
    while (count2 == 0) {
      val keys = scala.io.StdIn.readInt()
      if (keys == 0) {
        count2 += 1
      }
      else {
        layer2.append(keys)
      }
    }
    println("得到的值数组为：")
    println(layer2)
    //  直接利用+=对空字典中添加元组内容
    if (layer1.length != layer2.length) println("输入数据有误，关键字与值数量不符。")
    for (i <- 1 to layer1.length) {
      val tuple1 = new Tuple2[String, Int](layer1(i - 1), layer2(i - 1))
      map1 += tuple1
    }
    return(map1)
  }

  //  求字典的长度
  def len(dic1: Map[String, Int]): Int = {
    var len = 0
    for (k <- dic1.keys) len += 1
    return len
  }

  //  对两个字典进行合并操作，利用get推断是否在字典中，利用+=加入字典，对于overwrite不同的以不同的字典为基础进行添加
  def combine(dic1: Map[String, Int], dic2: Map[String, Int], len1: Int, len2: Int, overwrite: Boolean) = {
    if (overwrite == true) {
      val keys1 = dic2.keys.toList
      for (i <- 1 to len2) {
        if (dic1.get(keys1(i-1)) == None) {
          val tuple1 = new Tuple2[String, Int](keys1(i-1), dic2.get(keys1(i-1)).get)
          dic1 += tuple1
        }
      }
      println("合并后得到的字典为：")
      println(dic1)
    }
    else {
      val keys2 = dic1.keys.toList
      for (i <- 1 to len1) {
        if (dic2.get(keys2(i - 1)) == None) {
          val tuple2 = new Tuple2[String, Int](keys2(i - 1), dic1.get(keys2(i - 1)).get)
          dic2 += tuple2
        }
      }
      println("合并后得到的字典为：")
      println(dic2)
    }
  }

  //  查看字典中是否包括此关键字
  def contains(dic1: Map[String, Int], key: String): Boolean = {
    if (dic1.get(key) == None) return false
    else return true
  }

  //  利用keys和values函数提取对应关键字的值
  def get(dic1: Map[String, Int], key: String) = {
    val keys1 = dic1.keys.toList
    val values1 = dic1.values.toList
    var len1 = len(dic1)
    if (contains(dic1, key)) {
      for (i <- 1 to len1) {
        if (keys1(i-1) == key) println(values1(i-1))
      }
    }
    else println("该关键字不在字典中。")
  }

  //  提取字典的关键字
  def keys(dic1: Map[String, Int]): List[String] = {
    val keys1 = dic1.keys.toList
    return keys1
  }

  //  对字典添加关键字和值
  def set(dic1: Map[String, Int]) = {
    var arr1 = ofDim[String](0)
    var layer1 = arr1.toBuffer
    println("输入你想要set的关键字。")
    var key1 = scala.io.StdIn.readLine()
    println("输入你想要set的值。")
    var values1 = scala.io.StdIn.readInt()
    val tuple1 = new Tuple2[String, Int](key1, values1)
    dic1 += tuple1
    println("得到的字典为：")
    println(dic1)
  }

  //  以数组的形式对关键字和值进行提取
  def toArray(dic1: Map[String, Int]) = {
    println("依次输入你想要的数组的关键字的值,结束时请输入finish：")
    var arr1 = ofDim[String](0)
    var arr2 = ofDim[Int](0)
    var layer1 = arr1.toBuffer
    var layer2 = arr2.toBuffer
    var count1 = 0
    while (count1 == 0) {
      val keys = scala.io.StdIn.readLine()
      if (keys == "finish") {
        count1 += 1
      }
      else {
        layer1.append(keys)
        layer2.append(dic1.get(keys).get)
      }
    }
    println("得到的对应的值的数组如下：")
    println(layer2)
  }

  //  对字典减少关键字和值
  def remove(dic1: Map[String, Int]) = {
    val keys1 = dic1.keys.toList
    println("依次输入你想要减去的数组的关键字,结束时请输入finish：")
    var count1 = 0
    while (count1 == 0) {
      val keys = scala.io.StdIn.readLine()
      if (keys == "finish") {
        count1 += 1
      }
      else {
        val flag1 = keys1.exists(x => x.equals(keys))
        if (flag1) dic1 -= keys
        else println("给出的关键字不在字典中。")
      }
    }
    println("减去后的字典如下：")
    println(dic1)
  }

  def main(args: Array[String]): Unit = {
    println("接下来将展示生成一个字典：")
    val map1 = Dictionary()
    var len1 = len(map1)
    println("接下来对这个字典进行combine操作，需生成一个新的字典：")
    val map2 = Dictionary()
    var len2 = len(map2)
    println("请选择overwrite的值是false还是true。")
    var overwrite = scala.io.StdIn.readBoolean()
    combine(map1, map2, len1, len2, overwrite)
    println("接下来对这个字典进行contains操作，请输入一个关键字：")
    var key = scala.io.StdIn.readLine()
    if (contains(map1, key)) println("true")
    else println("false")
    println("接下来对这个字典进行get操作，请输入一个关键字：")
    var key1 = scala.io.StdIn.readLine()
    get(map1, key1)
    println("接下来对这个字典进行keys操作，得到该字典的关键字：")
    println(keys(map1))
    println("接下来对这个字典进行set操作。")
    set(map1)
    println("接下来对这个字典进行toArray操作。")
    toArray(map1)
    println("接下来对这个字典进行remove操作。")
    remove(map1)
  }
}
