package chap3

object MatrixMultApp {
  def main(args: Array[String]): Unit = {
    val myMatrix1: Array[Array[Double]] = Array(Array(2.5, 1.5, 0.5), Array(1, 2, 4))
    val myMatrix2: Array[Array[Double]] = Array(Array(-1, -1.5, 1, -1), Array(0.5, -2, -2.5, 1), Array(1, 2, 1, 1))

    val myResultMatrix: Array[Array[Double]] = multiplyMatrices(myMatrix1, myMatrix2)

    for(i <- myResultMatrix.indices) {
      for(j <- myResultMatrix(0).indices) {
        println(myResultMatrix(i)(j) + " ")
      }
      println()
    }
  }

  def multiplyMatrices(mat1: Array[Array[Double]], mat2: Array[Array[Double]]): Array[Array[Double]] = {
    val resultMat = Array.ofDim[Double](mat1.length, mat2(0).length)

    val mat1Rows = mat1.length
    val mat2Rows = mat2.length
    val mat1Columns = mat1(0).length
    val mat2Columns = mat2(0).length

    if (mat1Columns != mat2Rows) {
      println("Matrix 1 Columns: " + mat1Columns + " did not match with Matrix 2 rows: " + mat2Rows)
    } else {
      for(i <- 0 until mat1Rows) {
        for(j <- 0 until mat2Columns) {
          for(k <- 0 until mat1Columns) {
            resultMat(i)(j) += mat1(i)(k) * mat2(k)(j)
          }
        }
      }
    }

    resultMat
  }
}
