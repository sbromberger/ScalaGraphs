package com.bromberger.scalagraphs

import com.bromberger.sparsematrices.SparseMatrixCSR
import de.ummels.prioritymap.PriorityMap

case class DijkstraState(dists:Map[Int, Double], parents:Map[Int, Int], predecessors:Map[Int, List[Int]]) {
  override def toString:String = "DijkstraState with " + dists.size + " dists"
}

case class Graph(private val fMat:SparseMatrixCSR, private val bMat:SparseMatrixCSR, private val _props:Array[Array[Any]]) {
  val ne:Int = fMat.nnz
  val nv:Int = fMat.size._1

  def edges: Iterator[(Int, Int)] = {
    fMat.rows.zipWithIndex.flatMap(r => r._1.map(v => (r._2, v)))
  }

  def hasEdge(u:Int, v:Int):Boolean = fMat(u, v) >= 0

  def vertices:Iterator[Int] = 0.until(nv).toIterator
  def outNeighbors(v:Int):Array[Int] = fMat.getRow(v)
  def outNeighbors: Iterator[Array[Int]] = fMat.rows
  def inNeighbors(v:Int):Array[Int] = bMat.getRow(v)
  def inNeighbors: Iterator[Array[Int]] = bMat.rows

  def toUndirected:Graph = {
    val combined = fMat + bMat
    Graph(combined, combined, _props)
  }
  override def toString:String = "{" + nv + ", " + ne + "} Graph"


  // Sugary methods
  def outDegree(v:Int):Int =  outNeighbors(v).length
  def outDegree:Iterator[Int] = outNeighbors.map(_.length)
  def inDegree(v:Int):Int = inNeighbors(v).length
  def inDegree:Iterator[Int] = inNeighbors.map(_.length)
  def hasSelfLoops:Boolean = vertices.map(v => hasEdge(v, v)).reduce(_ || _)

  private def _degreeCentrality(normalize:Boolean, it:Iterator[Int]):Iterator[Double] = {
    val s = if (normalize) 1.0 / (nv - 1.0) else 1.0
    it.map(_ * s)
  }
  def inDegreeCentrality(normalize:Boolean=false):Iterator[Double] = _degreeCentrality(normalize, inDegree)
  def outDegreeCentrality(normalize:Boolean=false):Iterator[Double] = _degreeCentrality(normalize, outDegree)

  def dijkstra(source: Int, distfn:(Int, Int) => Double = (_, _) => 1, allpaths:Boolean=false):
  DijkstraState = {
    def go(PQ: PriorityMap[Int, Double], res: Map[Int, Double], parents: Map[Int, Int], preds:scala.collection.mutable.Map[Int, List[Int]]):
    (Map[Int, Double], Map[Int, Int], Map[Int, List[Int]]) =
      if (PQ.isEmpty) (res, parents, preds.toMap)
      else {
        val (node, cost) = PQ.head
        val neighbors = outNeighbors(node)
            .map(v => (v, distfn(node, v)))
            .filter(nc => (cost + nc._2) < PQ.getOrElse(nc._1, Double.PositiveInfinity)).map(nc =>
            (nc._1, cost + nc._2))

        val neighborMap = neighbors.filterNot(v => res.contains(v._1)).toMap
        val predMap = neighbors.map(_._1)
        if (allpaths)
          for (p <- predMap) {
            preds(p) = node :: preds(p)
          }

        val parent = neighborMap.mapValues(_ => node)

        go(PQ.tail ++ neighborMap, res + (node -> cost), parents ++ parent, preds)
      }

    val predMap = scala.collection.mutable.Map[Int, List[Int]]()
    vertices.foreach(v => predMap += (v -> List.empty[Int]))
    val (dists, parents, preds) = go(PriorityMap(source -> 0), Map.empty, Map.empty, predMap)
    DijkstraState(dists, parents, preds)
  }
}


object Graph {
  def apply(m:SparseMatrixCSR):Graph = Graph(m, m.transpose, Array.empty)
  def apply(m:Array[Array[Int]]):Graph = Graph(SparseMatrixCSR(m), SparseMatrixCSR(m).transpose, Array.empty)
  def apply(edgeList:Seq[(Int, Int)]):Graph = {
    val el = edgeList.sorted.distinct
    val ss = el.map(_._1)
    val ds = el.map(_._2)
    val nv = List(ss.max, ds.max).max + 1

    val byrow = el.groupBy(_._1).map{case (k, v) => (k, v.map(_._2).sorted.toArray) }

    val elemsPerRow = 0.until(nv).map(i=>byrow.getOrElse(i, Array()).length)

    val rp = Array(0) ++ elemsPerRow.scanLeft(0)(_ + _).tail
    val ci = ds.toArray
    Graph(SparseMatrixCSR(rp, ci))
  }
}
object TestSparseMatrixCSR{

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  def main(args: Array[String]) {
    // (1) use the primary constructor

    // (2) use a secondary constructor
    println("Starting")
    val matrix = Array.ofDim[Int](4,4)
    matrix(1)(0) = 5
    matrix(1)(1) = 8
    matrix(2)(2) = 3
    matrix(3)(1) = 6
    val B = SparseMatrixCSR(matrix)
    println("Done with B")
    val C = SparseMatrixCSR(matrix.transpose)
    println("Done with C")
    val D = B.transpose
    println("Done with D")
    println("B = " + B)
    println("C = " + C)
    println("D = " + D)

    val m2 = Array.ofDim[Int](3, 4)
    m2(0)(0) = 1
    m2(0)(3) = 2
    m2(1)(2) = 3
    m2(2)(2) = 4

    val E = SparseMatrixCSR(m2)
    val F = SparseMatrixCSR(m2.transpose)
    val G = E.transpose

    println("E = " + E)
    println("F = " + F)
    println("G = " + G)

    val m3 = Array.ofDim[Int](3, 4)
    m3(0)(2) = 7
    m3(1)(0) = 5
    m3(2)(3) = 6

    val H = SparseMatrixCSR(m3)
    val I = SparseMatrixCSR(m3.transpose)
    val J = H.transpose

    println("H = " + H)
    println("I = " + I)
    println("J = " + J)


    val m4 = Array.ofDim[Int](3, 4)
    m4(0)(0) = 1
    m4(0)(3) = 2
    m4(1)(2) = 3
    m4(2)(2) = 4
    m4(0)(2) = 7
    m4(1)(0) = 5
    m4(2)(3) = 6


    val K = SparseMatrixCSR(m4)
    val L = SparseMatrixCSR(m4.transpose)
    val M = H.transpose

    println("K = " + K)
    println("L = " + L)
    println("M = " + M)

    val N = E + H
    println("N = " + N)
    println("N == K = " + (N == K))

    val g = Graph(B)
    println("g = " + g)
    g.edges.foreach(println)
    g.vertices.foreach(v => println("g.outNeighbors(" + v + ") = " + g.outNeighbors(v)))
    println("---")
    g.outNeighbors.foreach(r => println("out r = " + r))
    g.inNeighbors.foreach(r => println("in r = " + r))

    val z = List((0, 1), (1, 2), (2,3), (3,4), (0,3))
    val g2 = Graph(z)
    println("g2 = " + g2)
    g2.edges.foreach(println)
    val d1 = g2.dijkstra(0, allpaths=true)
    println("d1 = " + d1)

    println("LOADING edges.csv")
    val bufferedSource = io.Source.fromFile("/Users/seth/dev/scala/ScalaGraphs/edges.csv")
    val edgeList = bufferedSource.getLines.map(line => {
      val cols = line.split(",").map(_.trim)
      (cols(0).toInt, cols(1).toInt)
    }).toArray
    bufferedSource.close
    println("Loaded edges.csv")
    val g3 = time { Graph(edgeList) }
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

}