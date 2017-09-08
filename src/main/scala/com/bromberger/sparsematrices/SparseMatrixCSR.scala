package com.bromberger.sparsematrices

import scala.collection.parallel.immutable.ParVector

case class SparseMatrixCSR(private val rowPtr:ParVector[Int], private val colVal:ParVector[Int]) {

  def apply(r: Int, c: Int): Int = {
    val rStart = rowPtr(r)
    val rEnd = rowPtr(r + 1)
    val cOffset = colVal.slice(rStart, rEnd).indexOf(c)
    if (cOffset >= 0) cOffset + rStart else -1
  }
  val nnz:Int = colVal.length
  val size = Tuple2(rowPtr.length - 1, colVal.max)

  def nzIndices:Iterator[(Int, Int)] = {
    0.until(rowPtr.length - 1).flatMap(r => {
      //      println("r = " + r)
      val cs = colVal.slice(rowPtr(r), rowPtr(r + 1))
      cs.map(c => (r, c))
    }).toIterator
  }
  def transpose:SparseMatrixCSR = {
    val revIndices = nzIndices.map(x => (x._2, x._1)).toList.sortBy(_._1)
    //    println("transpose: revIndices = " + revIndices)
    val ci = revIndices.map(_._2).toVector
    //    println("transpose: ci = " + ci)
    val rpFrequencyMap= revIndices.map(_._1).groupBy(identity).mapValues(_.size)
    //    println("transpose: rpFrequencyMap = " + rpFrequencyMap)
    val rpFreq = ci.indices.map(i => rpFrequencyMap.getOrElse(i, 0))
    val rp = Vector(0)++ rpFreq.scanLeft(0)(_ + _).tail

    //    println("transpose: rp = " + rp)


    SparseMatrixCSR(rp.par, ci.par)
  }

  def getRow(r:Int):ParVector[Int] = {
    val rStart = rowPtr(r)
    val rEnd = rowPtr(r + 1)
    colVal.slice(rStart, rEnd)
  }

  def rows:Iterator[ParVector[Int]] = 0.until(this.size._1).toIterator.map(r => getRow(r))

  def toVectors:Vector[ParVector[Int]] = rows.toVector
}

object SparseMatrixCSR {
  def apply(m:Array[Array[Int]]):SparseMatrixCSR = {
    val rp = Vector(0) ++ m.map(_.count(_ != 0)).scanLeft(0)(_ + _).tail
    val ci = m.flatMap(_.zipWithIndex.filter(_._1 != 0).map(_._2)).toVector
    SparseMatrixCSR(rp, ci)
  }

  def apply(rp:Seq[Int], ci:Seq[Int]):SparseMatrixCSR = SparseMatrixCSR(rp.toVector.par, ci.toVector.par)
}