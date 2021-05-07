package exercise1

object TaskOneOperations {
  def max(arr: Array[Int]): Int = arr.reduce((a, b) => if (a > b) a else b)

  def min(arr: Array[Int]): Int = arr.reduce((a, b) => if (a < b) a else b)

  def sum(arr: Array[Int]): Int = arr.reduce((a, b) => a + b)
}
