package whu.edu.cn.oge
/**
 * A simple dictionary-like data structure that stores key-value pairs.
 */
class Dictionary {
  // Private data storage using a Map
  private var data: Map[Any, Any] = Map.empty
  /**
   * Sets the value associated with the given key.
   *
   * @param key   The key for the value
   * @param value The value to be associated with the key
   */
  def set(key: Any, value: Any): Unit = {
    data += (key -> value)
  }
  /**
   * Checks if the dictionary contains a specific key.
   *
   * @param key The key to check for
   * @return True if the key is present, false otherwise
   */
  def contains(key: Any): Boolean = {
    data.contains(key)
  }
  /**
   * Retrieves the value associated with the given key. If the key is not found,
   * sets the key with the provided default value and returns it.
   *
   * @param key          The key to retrieve
   * @param defaultValue The default value to set and return if the key is not found
   * @return The value associated with the key or the default value if not found
   */
  def get(key: Any, defaultValue: Any = null): Any = {
    data.getOrElse(key, {
      set(key, defaultValue)
      defaultValue
    })
  }
  /**
   * Retrieves the value associated with the given key as a String.
   *
   * @param key The key to retrieve
   * @return The value associated with the key as a String, or null if not found or not a String
   */
  def getString(key: Any): String = {
    data.get(key) match {
      case Some(value: String) => value
      case _ => null
    }
  }
  /**
   * Retrieves a sorted list of keys present in the dictionary.
   *
   * @return A sorted list of keys
   */
  def keys: List[Any] = {
    implicit val ordering: Ordering[Any] = new Ordering[Any] {
      def compare(x: Any, y: Any): Int = {
        x.toString.compareTo(y.toString)
      }
    }
    data.keys.toList.sorted
  }
  /**
   * Applies a given algorithm to each key-value pair in the dictionary and returns a new Dictionary.
   *
   * @param algorithm A function that takes a key and value, and returns a new value
   * @return A new Dictionary with modified key-value pairs
   */
  def map(algorithm: (Any, Any) => Any): Dictionary = {
    val newData = data.flatMap {
      case (key, value) =>
        val newValue = algorithm(key, value)
        if (newValue != null) Some(key -> newValue)
        else None
    }
    val newDictionary = new Dictionary
    newDictionary.data = newData
    newDictionary
  }
  /**
   * Retrieves an array of values associated with the given keys.
   *
   * @param keys The keys to retrieve values for
   * @return An array of values associated with the keys
   */
  def toArray(keys: Any*): Array[Any] = {
    val selectedKeys = if (keys.nonEmpty) keys else this.keys
    selectedKeys.flatMap(key => data.get(key)).toArray
  }
  /**
   * Removes specified keys from the dictionary.
   *
   * @param keys          The keys to remove
   * @param ignoreMissing If true, missing keys will not raise an exception
   */
  def remove(keys: List[Any], ignoreMissing: Boolean = false): Unit = {
    for (key <- keys) {
      if (!ignoreMissing && !data.contains(key)) {
        throw new NoSuchElementException(s"Key '$key' not found in the dictionary.")
      }
      data -= key
    }
  }
  /**
   * Executes a provided function on the dictionary itself and returns the modified dictionary.
   *
   * @param func A function that takes the dictionary as a parameter and returns Unit
   * @return The modified dictionary
   */
  def aside(func: Any => Unit): Dictionary = {
    func(this)
    this
  }
  /**
   * Combines the dictionary with another dictionary, optionally overwriting existing values.
   *
   * @param other     The other dictionary to combine with
   * @param overwrite If true, existing keys will be overwritten by keys from the other dictionary
   * @return A new Dictionary containing combined key-value pairs
   */
  def combine(other: Dictionary, overwrite: Boolean = false): Dictionary = {
    val combinedDict = new Dictionary
    for ((key, value) <- data) {
      combinedDict.set(key, value)
    }
    for ((key, value) <- other.data) {
      if (overwrite || !combinedDict.data.contains(key)) {
        combinedDict.set(key, value)
      }
    }
    combinedDict
  }
  /**
   * Converts the dictionary to a human-readable string.
   *
   * @return A string representation of the dictionary
   */
  override def toString: String = {
    data.map {
      case (key: String, value: String) => s"'$key' -> '$value'"
      case (key: String, value) => s"'$key' -> $value"
      case (key, value: String) => s"$key -> '$value'"
      case (key, value) => s"$key -> $value"
    }.mkString("Dictionary(", ", ", ")")
  }
}
/**
 * Companion object for the Dictionary class, providing a convenient way to create
 * a new Dictionary instance with initial entries.
 */
object Dictionary {
  /**
   * Creates a new Dictionary instance with initial entries provided as pairs of key-value tuples.
   *
   * @param entries Initial key-value pairs to populate the dictionary with
   * @return A new Dictionary instance with the provided initial entries
   */
  def apply(entries: (Any, Any)*): Dictionary = {
    val dict = new Dictionary
    for ((key, value) <- entries) {
      dict.set(key, value)
    }
    dict
  }
}
/**
 * This is the main entry point for testing the Dictionary class and its functionalities.
 */
object Main extends App {
  // Create a dictionary with initial entries using key-value pairs
  val dict = Dictionary(
    "B1" -> 182,
    "B2" -> 219,
    "B3" -> 443,
    1.0 -> "hello"
  )
  println(dict)
  println("aside:")
  // Execute a provided function on the dictionary and print a debug message
  dict.aside(obj => println(s"Debug: $obj"))
  println("getString:")
  // Retrieve and print a string value associated with the key "B2"
  println(dict.getString("B2"))

  // Retrieve and print a string value associated with the key 1.0
  println(dict.getString(1.0))

  // Print the dictionary
  println(dict)
  println("get:")
  // Retrieve and print the value associated with the key "B2"
  println(dict.get("B2"))

  // Print the dictionary
  println(dict)

  // Retrieve and print the value associated with the key "B5", using "test" as the default value
  println(dict.get("B5", "test"))

  // Print the dictionary
  println(dict)

  // Create two dictionaries with initial entries
  val dict1 = Dictionary(
    "B1" -> 182,
    "B2" -> 219,
    "B3" -> 443
  )

  val dict2 = Dictionary(
    "B2" -> 999,
    "B4" -> 123
  )

  // Combine two dictionaries, overwriting conflicting keys from dict1 with dict2
  val combinedDict = dict1.combine(dict2, overwrite = true)
  println("Combine overwrite = true:")
  println(combinedDict)

  // Combine two dictionaries, keeping values from dict1 for conflicting keys
  val combinedDict2 = dict1.combine(dict2)
  println("Combine overwrite = false:")
  println(combinedDict2)

  // Print the keys present in the dictionary
  println("keys:")
  println(dict.keys)

  // Create a new dictionary by applying a function to select key-value pairs from the original dictionary
  val mappedDict = dict.map((key, value) => if (key.toString.startsWith("B")) value else null)
  println("map:")
  println(mappedDict)

  // Define an algorithm to modify values in the dictionary
  val algorithm: (Any, Any) => Any = (key, value) => {
    value match {
      case num: Int => num + 10
      case _ => null
    }
  }

  // Apply the algorithm to modify values in the dictionary and print the result
  val mappedDict2 = dict.map(algorithm)
  println(mappedDict2) // Should print Dictionary('B1' -> 192, 'B2' -> '229', 'B3' -> 453)

  // Retrieve and print selected values from the dictionary as an array
  val values = dict.toArray("B1", "B3")
  println("toArray:")
  println(values.mkString(", ")) // Should print 182, 443

  // Retrieve and print all values from the dictionary as an array
  val allValues = dict.toArray()
  println(allValues.mkString(", ")) // Should print Hello, 182, 443

  // Remove specified keys from the dictionary, ignoring missing keys
  dict.remove(List("B2", "B4"), ignoreMissing = true)
  println("remove:")
  println(dict)
}
