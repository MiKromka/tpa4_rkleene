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
