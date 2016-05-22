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
    ??? // TODO TPA
  }

}
