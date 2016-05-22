package services

import model.Matrix

object FloydWarshall extends ShortestPathServiceLike {

  /**
    * Computes shortest paths between all node pairs using Floyd-Warshall Algorithm
    * [[https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm Wikipedia entry for Floyd-Warshall algorithm]]
    * @param m adjacency matrix of a graph
    * @return shortest paths matrix
    */
  override protected def computeShortestPaths(m: Matrix): Matrix = {
    ??? // TODO MKROMKA
  }
}
