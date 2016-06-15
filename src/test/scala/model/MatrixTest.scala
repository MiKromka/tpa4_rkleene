package model

import org.scalatest.{Matchers, FlatSpec}

class MatrixTest extends FlatSpec with Matchers  {

  "Matrix" should "return correct submatrices" in {
    //given
    val m = Matrix(Array(Array(1,2), Array(3,4)))

    //when
    val results: Seq[Matrix] = Seq(m.getA, m.getB, m.getC, m.getD)

    //then
    results.zipWithIndex.foreach{
      case (matrix, idx) =>
        matrix.size shouldEqual 1
        matrix.m(0)(0) shouldEqual idx + 1
    }
  }

  it should "return true for power of two" in {
    //given
    val powersOfTwo = Seq(1,2,4,16,32,64,128,256,512,1024)

    powersOfTwo.foreach{ n =>
      //when-then
      Matrix.isPowerOfTwo(n) shouldEqual true
    }
  }

  it should "return false for numbers that are not power of two" in {
    //given
    val powersOfTwo = Seq(3,10,11,12,13,24,77,66,513,258)

    powersOfTwo.foreach{ n =>
      //when-then
      Matrix.isPowerOfTwo(n) shouldEqual false
    }
  }
  it should "create matrix from four submatrices" in {
    //given
    val a = Matrix(Array(Array(1,2),Array(5,6)))
    val b = Matrix(Array(Array(3,4),Array(7,8)))
    val c = Matrix(Array(Array(9,10),Array(13,14)))
    val d = Matrix(Array(Array(11,12),Array(15,16)))
    val expectedMatrix = Matrix(Array(Array(1,2,3,4), Array(5,6,7,8), Array(9,10,11,12), Array(13,14,15,16)))

    //when
    val resultMatrix = Matrix(a,b,c,d)

    //then
    resultMatrix shouldEqual expectedMatrix
  }

  it should "sum matrices using min operator instead of +" in {
    //given
    val m1 = Matrix(Array(Array(10, 1), Array(3, 3)))
    val m2 = Matrix(Array(Array(2, 22), Array(3, 1)))

    val expectedMatrix = Matrix(Array(Array(2, 1), Array(3,1)))

    //when
    val result = m1 + m2

    //then
    result shouldEqual expectedMatrix
  }

  it should "multiply matrices using min instead of + and + operator instead of *" in {
    //given
    val m1 = Matrix(Array(Array(10, 1), Array(3, 3)))
    val m2 = Matrix(Array(Array(2, 22), Array(3, 1)))

    val expectedMatrix = Matrix(Array(Array(4, 2), Array(5, 4)))

    //when
    val result = m1 * m2

    //then
    result shouldEqual expectedMatrix
  }



  it should "throw IllegalArgumentException if non-square matrix is created" in {
    a [IllegalArgumentException] should be thrownBy {
      Matrix(Array(Array(1,2,3,4), Array(4,5,6,7)))
    }

    a [IllegalArgumentException] should be thrownBy {
      Matrix(Array(Array(1,2)))
    }
  }

  it should "throw IllegalArgumentException if matrix has size that is not a power of 2" in {
    a [IllegalArgumentException] should be thrownBy {
      Matrix(Array(Array(1,2,3), Array(4,5,6), Array(7,8,9)))
    }
  }

  it should "return true when comparing two exact matrices" in {
    //given
    val m1 = Matrix(Array(Array(1,2), Array(3,4)))
    val m2 = Matrix(Array(Array(1,2), Array(3,4)))

    //when-then
    m1 shouldEqual m2
  }

  it should "return false when comparing two different matrices" in {
    //given
    val m1 = Matrix(Array(Array(1,2), Array(3,4)))
    val m2 = Matrix(Array(Array(3,4), Array(1,2)))

    //when-then
    m1 shouldNot be (m2)
  }
}
