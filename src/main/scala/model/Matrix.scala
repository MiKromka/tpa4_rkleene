package model

case class Indices(start: Int, end: Int)

/**
  * matrix is a list of rows of columns
  *
  * M = | A  B |
  *     | C  D |
  */
case class Matrix(m: Array[Array[Double]]) {

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
}

object Matrix {
  private[model] def isPowerOfTwo(n: Int): Boolean = {
    (n & (n - 1)) == 0
  }
}

