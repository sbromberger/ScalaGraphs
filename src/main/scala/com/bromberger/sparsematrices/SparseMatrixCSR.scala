package com.bromberger.sparsematrices

case class SparseMatrixCSR(private val rowPtr:Array[Int], private val colVal:Array[Int]) {

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
    val ci = revIndices.map(_._2).toArray
    //    println("transpose: ci = " + ci)
    val rpFrequencyMap= revIndices.map(_._1).groupBy(identity).mapValues(_.size)
    //    println("transpose: rpFrequencyMap = " + rpFrequencyMap)
    val rpFreq = ci.indices.map(i => rpFrequencyMap.getOrElse(i, 0))
    val rp = Array(0) ++ rpFreq.scanLeft(0)(_ + _).tail

    //    println("transpose: rp = " + rp)


    SparseMatrixCSR(rp, ci)
  }

  def +(that:SparseMatrixCSR):SparseMatrixCSR = {
    assert(size == that.size)
    val rows = 0.until(size._1).map(i => {
      val thisr = this.getRow(i)
      val thatr = that.getRow(i)
      (thisr ++ thatr).distinct.sorted
    })
    val rp = rows.map(_.length).scanLeft(0)(_ + _).toArray
    val ci = rows.flatten.toArray

//    println("  add rp = " + rp)
//    println("  add ci = " + ci)

    SparseMatrixCSR(rp, ci)
  }
  def getRow(r:Int):Array[Int] = {
    val rStart = rowPtr(r)
    val rEnd = rowPtr(r + 1)
    colVal.slice(rStart, rEnd)
  }

  def rows:Iterator[Array[Int]] = 0.until(this.size._1).toIterator.map(getRow)

  def toArrays:Array[Array[Int]] = rows.toArray
}

object SparseMatrixCSR {
  def apply(m:Array[Array[Int]]):SparseMatrixCSR = {
    val rp = m.map(_.count(_ != 0)).scanLeft(0)(_ + _)
    val ci = m.flatMap(_.zipWithIndex.filter(_._1 != 0).map(_._2))
    SparseMatrixCSR(rp, ci)
  }

  def apply(rp:Seq[Int], ci:Seq[Int]):SparseMatrixCSR = SparseMatrixCSR(rp, ci)
}