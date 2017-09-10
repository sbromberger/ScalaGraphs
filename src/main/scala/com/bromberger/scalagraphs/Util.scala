package com.bromberger.scalagraphs

object Util {
  def loadCsvAsEdgeList(path: String): Array[(Int, Int)] = {
    time {
      println(s"LOADING $path")

      val bufferedSource = io.Source.fromFile(path)

      val edgeList = bufferedSource.getLines.map(line => {
        val cols = line.split(",").map(_.trim)
        (cols(0).toInt, cols(1).toInt)
      }).toArray

      bufferedSource.close

      println(s"LOADED $path")

      edgeList
    }
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    val ns = t1 - t0
    val s = ns / 1000000000f
    println(s"Elapsed time: $ns ns ($s s)")
    result
  }
}
