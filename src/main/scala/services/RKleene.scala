package services

import model.Matrix

object RKleene extends ShortestPathServiceLike {

  /**
    * Computes shortest paths between all node pairs using r-Kleene Algorithm
    * [[http://link.springer.com/article/10.1007%2Fs00453-006-1224-z Article on Springer site]]
    * @param m adjacency matrix of a graph
    * @return shortest paths matrix
    */
  override def computeShortestPaths(m: Matrix): Matrix = {
    if (m.size == 1) {
      Matrix(Array(Array(0)))
    } else if (m.size == 2){
      m
    } else {
      val a = computeShortestPaths(m.getA)
      val b = m.getB + a * m.getB
      val c = m.getC + m.getC * a
      val d = m.getD + c * b
      val d2 = computeShortestPaths(d)
      val b2 = b + b * d2
      val c2 = c + d2 * c
      val a2 = a + b2 * c2

      Matrix(a=a2, b=b2, c=c2, d=d2)
    }
  }

}
