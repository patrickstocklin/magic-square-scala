object Solution {

  def main(args: Array[String]) : Unit = {
    println("Hello Magic Squares")

    val square =
      List(
        List(4,3,8),
        List(9,5,1),
        List(2,7,6)
      )

    println(calculateMagicConstant(square))
    println(validateMagicSquare(square))
  }

  def calculateMagicConstant(square: List[List[Int]]) : Int = {
    square.size * ( (square.size * square.size) + 1) / 2;
  }

  def validateMagicSquare(square: List[List[Int]]) : Boolean = {
    val n = square.size
    val m = calculateMagicConstant(square)
    validateSetOfElements(square) &&
      validateRows(square, m) &&
      validateCols(square, m) &&
      validateDiags(square, n, m)
  }

  def validateSetOfElements(square: List[List[Int]]) : Boolean = {
    square.flatten.toSet.diff((1 to square.size * square.size).toSet).isEmpty
  }

  def validateArray(array: List[Int], m: Int, arrayType: String) : Boolean = {
    println("Calculating Sum of " + arrayType + ": " + array.toString())
    array.sum.equals(m)
  }

  def validateRows(square: List[List[Int]], m: Int) : Boolean = {
    square.forall(validateArray(_, m, "Row"))
  }

  def validateCols(square: List[List[Int]], m: Int) : Boolean = {
    val indices = List.range(0, square.size)
    indices.map(j => indices.map(i => square(i)(j))).forall(validateArray(_, m, "Col"))
  }

  def validateDiags(square: List[List[Int]], n: Int, m: Int) : Boolean = {
    val indices = List.range(0, square.size)
    validateArray(indices.map(i => square(i)(i)), m, "Diag") &&
    validateArray(indices.map(j => square(n - 1 - j)(j)), m, "Diag")
  }

}
