package com.bromberger.scalagraphs

import scala.collection.parallel.immutable.ParVector
import scala.collection.mutable.ArrayBuffer
import com.bromberger.sparsematrices.SparseMatrixCSR

case class DijkstraState(parents:Vector[Int], dists:Vector[Double], predecessors:Vector[Vector[Int]])

//case class PQ(els:ArrayBuffer[(Int, Double)]) {
//  private val pris = els.map(_._2)
//
//  def enqueue(x:(Int,Double)) = {
//    els.map(_._2)
//    els += x
//  }
//  def dequeue = ???
//
//}

case class Graph(private val fMat:SparseMatrixCSR, private val bMat:SparseMatrixCSR, private val _props:Vector[Vector[Any]]) {
  val ne:Int = fMat.nnz
  val nv:Int = fMat.size._1

  def edges: Iterator[(Int, Int)] = {
    fMat.rows.zipWithIndex.flatMap(r => r._1.map(v => (r._2, v)))
  }

  def hasEdge(u:Int, v:Int):Boolean = fMat(u, v) >= 0

  def vertices:Iterator[Int] = 0.until(nv).toIterator
  def outNeighbors(v:Int):ParVector[Int] = fMat.getRow(v)
  def outNeighbors: Iterator[ParVector[Int]] = fMat.rows
  def inNeighbors(v:Int):ParVector[Int] = bMat.getRow(v)
  def inNeighbors: Iterator[ParVector[Int]] = bMat.rows

//  def outNeighbors(u:Int) =
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


  def dijkstra(us:Seq[Int], distance:(Int, Int) => Double = (_, _) => 1):DijkstraState = {
    var dists = scala.collection.mutable.ArrayBuffer.fill(nv)(Double.PositiveInfinity)
    var parents = scala.collection.mutable.ArrayBuffer.fill(nv)(0)
    var visited = scala.collection.mutable.ArrayBuffer.fill(nv)(false)
    var pathcounts = scala.collection.mutable.ArrayBuffer.fill(nv)(0)
    var predecessors = vertices.map(_ => scala.collection.mutable.ArrayBuffer[Int]()).toVector
    var H = scala.collection.mutable.PriorityQueue[(Double, Int)]()
    us.foreach(u => {
      dists(u) = 0.0
      pathcounts(u) = 1
      H.enqueue((0.0, u))
      visited(u) = true
    })

    while (H.nonEmpty) {
      val hentry = H.dequeue()
      val (dist, u) = hentry

      outNeighbors(u).foreach(v => {
        val alt = if (dists(u) == Double.PositiveInfinity) Double.PositiveInfinity else dists(u) + distance(u, v)
        if (!visited(v)) {
          dists(v) = alt
          parents(v) = u
          pathcounts(v) += pathcounts(u)
          visited(v) = true
          predecessors(v) += u

        }
      })

    }
    
    DijkstraState(Vector(), Vector(), Vector())
  }
}

object Graph {
  def apply(m:SparseMatrixCSR):Graph = Graph(m, m.transpose, Vector[Vector[Int]]())
  def apply(m:Array[Array[Int]]):Graph = Graph(SparseMatrixCSR(m), SparseMatrixCSR(m).transpose, Vector[Vector[Int]]())
  def apply(edgeList:Seq[(Int, Int)]):Graph = {
    val el = edgeList.sorted.distinct
    val ss = el.map(_._1)
    val ds = el.map(_._2)
    val nv = List(ss.max, ds.max).max + 1

    val byrow = el.groupBy(_._1).mapValues(_.map(_._2).sorted)

    val elemsPerRow = 0.until(nv).map(i=>byrow.getOrElse(i, Vector()).length)

    val rp = Vector(0) ++ elemsPerRow.scanLeft(0)(_ + _).tail
    val ci = ds.toVector

    Graph(SparseMatrixCSR(rp.par, ci.par))
  }
}
object TestSparseMatrixCSR{

  def main(args: Array[String]) {
    // (1) use the primary constructor

    // (2) use a secondary constructor
    val matrix = Array.ofDim[Int](4,4)
    matrix(1)(0) = 5
    matrix(1)(1) = 8
    matrix(2)(2) = 3
    matrix(3)(1) = 6
    val B = SparseMatrixCSR(matrix)

    val C = SparseMatrixCSR(matrix.transpose)

    val D = B.transpose
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

    println("E.nzIndices = " + E.nzIndices.toList)
    println("E.revIndices = " + E.nzIndices.map(x => (x._2, x._1)).toList.sortBy(_._1))

    val g = Graph(B)
    println("g = " + g)
    g.edges.foreach(println)
    g.vertices.foreach(v => println("g.outNeighbors(" + v + ") = " + g.outNeighbors(v)))
    println("---")
    g.outNeighbors.foreach(r => println("out r = " + r))
    g.inNeighbors.foreach(r => println("in r = " + r))

    val z = List((1, 2), (1,2), (1,2), (1,2), (2,1), (3,1), (3,2), (1,1), (3,0) )
    val g2 = Graph(z)
    println("g2 = " + g2)
    g2.edges.foreach(println)
  }

}