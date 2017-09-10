package com.bromberger.scalagraphs

import com.bromberger.scalagraphs.Util.time
import org.scalatest.FlatSpec

class TimingExperiments extends FlatSpec {

  def fixture =
    new {
      lazy val g_n100_p20 = Util.loadCsvAsEdgeList("src/test/resources/testGraphCSV/ER.n100.p20.csv")
      lazy val g_n100_p70 = Util.loadCsvAsEdgeList("src/test/resources/testGraphCSV/ER.n100.p70.csv")
      lazy val g_n1000_p20 = Util.loadCsvAsEdgeList("src/test/resources/testGraphCSV/ER.n1000.p20.csv")
      lazy val g_n1000_p70 = Util.loadCsvAsEdgeList("src/test/resources/testGraphCSV/ER.n1000.p70.csv")
      lazy val g_n10000_p20 = Util.loadCsvAsEdgeList("src/test/resources/testGraphCSV/ER.n10000.p20.csv")
      lazy val g_n10000_p70 = Util.loadCsvAsEdgeList("src/test/resources/testGraphCSV/ER.n10000.p70.csv")
    }

  def speedTest(g: Array[(Int, Int)]): Unit = {
    val g3 = time { Graph(g) }
    println("created graph")
    println("g3 = " + g3)
    println("g3(2) outdegree= " + g3.outDegree(2))
    println("g3(2) indegree = " + g3.inDegree(2))
    val x1 = time { g3.dijkstra(0) }
    val x2 = time { g3.dijkstra(0, allpaths=true) }

    val vs = 0.to(6)
    val dijkm1 = time { vs.par.map(v => g3.dijkstra(v)) }
    val x3 = time { vs.par.map(v => g3.dijkstra(v, allpaths=true)) }
    println("dijkm = " + dijkm1)
  }

  "Testing g_n100_p20" should "be quick" in {
    speedTest(fixture.g_n100_p20)
  }

  "Testing g_n100_p70" should "be quick" in {
    speedTest(fixture.g_n100_p70)
  }

  "Testing g_n1000_p20" should "be quick" in {
    speedTest(fixture.g_n1000_p20)
  }

  "Testing g_n1000_p70" should "be quick" in {
    speedTest(fixture.g_n1000_p70)
  }

  "Testing g_n10000_p20" should "be quick" in {
    speedTest(fixture.g_n10000_p20)
  }

  //takes too long to load the 342 MB CSV
//  "Testing g_n10000_p70" should "be quick" in {
//    speedTest(fixture.g_n10000_p70)
//  }

}
