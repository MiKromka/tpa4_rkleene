package model

case class Indices(start: Int, end: Int)

/**
  * matrix is a list of rows of columns
  *
  * M = | A  B |
  *     | C  D |
  */
case class Matrix(m: Array[Array[Int]]) {

  import Matrix._

  require(isPowerOfTwo(m.length), "Number of rows must be power of two!")
  require(m.forall(row => isPowerOfTwo(row.length)), "Number of columns in a row must be power of two!")
  require(m.forall(row => row.length == m.length ), "Matrix must be square!")

  def size: Int = m.length

  def getA: Matrix = {
    getByIndices(rows = Indices(0, size / 2 - 1), columns = Indices(0, size / 2 - 1))
  }
  def getB: Matrix = {
    getByIndices(rows = Indices(0, size / 2 - 1), columns = Indices(size/2, size+1))
  }
  def getC: Matrix = {
    getByIndices(rows = Indices(size/2, size+1), columns = Indices(0, size / 2 - 1))
  }
  def getD: Matrix = {
    getByIndices(rows = Indices(size/2, size+1), columns = Indices(size/2, size+1))
  }

  private[model] def getByIndices(rows: Indices, columns: Indices): Matrix = {
    Matrix(m.slice(rows.start, rows.end + 1).map(_.slice(columns.start, columns.end + 1)))
  }

  override def toString: String = {
    m.deep.mkString("\n")
  }

  override def equals(o: Any) = {
    o match {
      case that: Matrix => that.m.deep == this.m.deep
      case _ => false
    }
  }

  override def hashCode = this.m.hashCode()

  /**
    * + is meant as a Matrix with elements that are min of corresponding elements of two matrices:
    * | 5 4 | + | 1 5 | = | 1 4 |
    * | 3 3 |   | 2 2 |   | 2 2 |
    */
  def +(other: Matrix): Matrix = {
    val newM = m.zip(other.m).map {
      case (thisRows, otherRows) =>
        thisRows.zip(otherRows).map{case (a1,a2) => Math.min(a1,a2)}
    }

    Matrix(newM)
  }

  /**
    * Fancy way of multiplying matrices in Scala with + changed to Math.min and * changed to +
    */
  def *(other: Matrix): Matrix = {
    val summed = for (row <- m)
      yield for (col <- other.m.transpose)
        yield {
          row.zip(col).map {
            case (a1,a2) if a1 == Int.MaxValue || a2 == Int.MaxValue =>
              Int.MaxValue
            case (a1,a2) =>
              a1 + a2
          }.reduceLeft[Int]{
            case (a:Int,b:Int) =>
              Math.min(a,b)
          }
        }

    Matrix(summed)
  }
}

object Matrix {
  private[model] def isPowerOfTwo(n: Int): Boolean = {
    (n & (n - 1)) == 0
  }

  def apply(a: Matrix, b: Matrix, c: Matrix, d: Matrix): Matrix = {
    val ab = a.m.zip(b.m).map {
      case (la,lb) => la ++ lb
    }

    val cd = c.m.zip(d.m).map {
      case (lc, ld) => lc ++ ld
    }

    val abcd = Matrix(ab ++ cd)

    abcd
  }
}

