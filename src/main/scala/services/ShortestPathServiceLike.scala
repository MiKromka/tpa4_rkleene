package services

import model.Matrix

trait ShortestPathServiceLike {
  /**
    * Wrapper for computing shortest paths between all two nodes
    * @param m adjacency matrix of a graph
    * @return shortest paths matrix
    */
  def compute(m: Matrix): Matrix = {
    println(s"Computing shortest paths for matrix:")
    println(m)

    val shortestPaths = computeShortestPaths(m)

    println(s"Computed shortest paths:")
    println(shortestPaths)

    shortestPaths
  }

  /**
    * Method returning matrix of shortest paths between all two nodes in the graph
    * @param m adjacency matrix of a graph
    * @return shortest paths matrix
    */
  protected def computeShortestPaths(m: Matrix): Matrix
}
