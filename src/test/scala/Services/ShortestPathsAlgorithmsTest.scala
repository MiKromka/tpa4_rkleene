package Services

import model.Matrix
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Matchers, FlatSpec}
import services.{FloydWarshall, RKleene}

class ShortestPathsAlgorithmsTest extends FlatSpec with Matchers with TableDrivenPropertyChecks {

  it should "compute shortest paths" in {
    forAll(testDataTable) {
      (adjMatrix, shortestPathMatrix, algorithm) =>
        algorithm.compute(adjMatrix) shouldEqual shortestPathMatrix
    }
  }

  //test cases
  lazy val trivialCaseMatrix = Matrix(Array(
    Array(1)
  ))
  lazy val simpleCaseShortestPaths = Matrix(Array(
    Array(0)
  ))

  lazy val twoVerticesMatrix = Matrix(Array(
    Array(0, 1),
    Array(1, 0)
  ))
  lazy val twoVerticesShortestPaths = twoVerticesMatrix

  lazy val fourVerticesMatrix = Matrix(Array(
    Array(0, 7, 8, 2),
    Array(7, 0, Int.MaxValue, Int.MaxValue),
    Array(8, Int.MaxValue, 0, 1),
    Array(2, Int.MaxValue, 1, 0)
  ))
  lazy val fourVerticesShortestPaths = Matrix(Array(
    Array(0,  7,  3, 2),
    Array(7,  0, 10, 9),
    Array(3, 10,  0, 1),
    Array(2,  9,  1, 0)
  ))

  lazy val testDataTable = Table(
    ("AdjacencyMatrix", "ShortestPathMatrix", "Algorithm"),
    testData:_*
  )

  lazy val dataSets = Seq(
//    (trivialCaseMatrix, simpleCaseShortestPaths),
//    (twoVerticesMatrix, twoVerticesShortestPaths),
    (fourVerticesMatrix, fourVerticesShortestPaths)
  )

  lazy val shortestPathAlgorithms = Seq(/*FloydWarshall, */RKleene)

  lazy val testData = shortestPathAlgorithms.flatMap(
    algorithm =>
      dataSets.map{
        case (m, sp) => (m, sp, algorithm)
      }
  )

}
