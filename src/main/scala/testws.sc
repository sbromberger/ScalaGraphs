import com.bromberger.scalagraphs.Graph
import com.bromberger.sparsematrices.SparseMatrixCSR

val matrix = Array.ofDim[Int](4,4)
matrix(1)(0) = 5
matrix(1)(1) = 8
matrix(2)(2) = 3
matrix(3)(1) = 6

// (0, 1)
// (1, 1)
// (2, 2)
// (1, 3)

// sort:
// (0, 1)
// (1, 1)
// (1, 3)
// (2, 2)

//[ . . . . ]
//[ 5 8 . . ]
//[ . . 3 . ]
//[ . 6 . . ]

// transposed
//[ . 5 . . ]
//[ . 8 . 6 ]
//[ . . 3 . ]
//[ . . . . ]

// rowptr = 0 1 3 4 4
// colval = 1 1 3 2
val t = matrix.transpose

val m2 = Array.ofDim[Int](3, 4)
m2(0)(0) = 1
m2(0)(3) = 2
m2(1)(2) = 3
m2(2)(2) = 4

m2.transpose


val z = List((1, 2), (2,1), (3,1), (3,2), (1,1), (3,0) )
z.sorted
val y = z.groupBy(_._1).mapValues(_.map(_._2).sorted)

val g3 = Graph(z)
g3.edges.foreach(println)

val byrow2 = Map(1 -> List(2))
val zz = byrow2.getOrElse(1, List()).toArray

val bb = List(false, false, false, false, false, false, false)

bb.reduce(_ || _)
