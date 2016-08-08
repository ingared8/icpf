package DataStructures

/**
  * Created by greddy on 8/5/16.
  */
trait
PolygonT {

  val vertices: Seq[PointT]

  def numOfEdges:Int = vertices.size

  def isBounded:Boolean = false
  //def getArea:FractionT =

  override def toString : String = {
    vertices.length.toString + "\n" +
    vertices.map(_.toString).mkString("\n")
  }

  def getEdges:Seq[EdgeT] = {
    val pairs: Seq[PointT] = this.vertices.tail.toList :+ this.vertices.head
    this.vertices.zip(pairs).map { case (a) => Edge(a._1, a._2) }
  }

  def getMidPoint:PointT = {
    val zeroOrdinate = FractionPointCoordinate(0,1)
    val zeroPoint:PointT = Point(zeroOrdinate,zeroOrdinate)
    this.vertices.foldLeft(zeroPoint)((b,a) => a + b )
  }

  def areaUnderThePolygon:FractionT = {

    val edges = getEdges
    val midPoint = getMidPoint
    val zerof:FractionT = FractionPointCoordinate(0,1)
    edges.foldLeft(zerof)((b,a) => a.areaOfTriangle(midPoint) + b)
  }

  def areaUnderThePolygonAbs:FractionT = areaUnderThePolygon.abs

  /*
  Contains the polygon inside
  All the boundaries should be inside the boundaries
   */
  def contains(polygon: PolygonT):Boolean = {

    val fmin:FractionT = FractionPointCoordinate(Int.MinValue,1)
    val fmax:FractionT = FractionPointCoordinate(Int.MaxValue,1)

    def getXMax(points: Seq[PointT]):FractionT
          = points.foldLeft(fmin)((b,a) => if (b > a.x) b else a.x )
    def getYMax(points: Seq[PointT]):FractionT
      = points.foldLeft(fmin)((b,a) => if (b > a.y) b else a.y )
    def getXMin(points: Seq[PointT]):FractionT
      = points.foldLeft(fmax)((b,a) => if (b < a.x) b else a.x )
    def getYMin(points: Seq[PointT]):FractionT
      = points.foldLeft(fmax)((b,a) => if (b > a.x) b else a.x )

    val myVertices = this.vertices

    getXMin(myVertices) <= getXMin(polygon.vertices) &&
    getXMax(myVertices) >= getXMax(polygon.vertices) &&
    getYMin(myVertices) <= getYMin(polygon.vertices) &&
    getYMax(myVertices) >= getYMax(polygon.vertices)

  }

  def myEdge:EdgeT = {
    require(this.vertices.size > 1)
    Edge(this.vertices.head, this.vertices.tail.head)
  }


  def mirrorImage: PolygonT = {
    val p = this.vertices.head
    val q = this.vertices.tail.head

     Polygon(p::q :: Edge(p,q).mirrorImageOfSeqOfPoints(this.vertices.tail.tail).toList)
  }

}

case class Polygon(vertices: Seq[PointT]) extends PolygonT {}

object PolygonExample extends  App {

  import Solver._
  val addr = "input.txt"
  val problem = ReadInput(addr).problem

  val srcPositions = problem.silhouette.polyList.head.vertices
  val dstPositions = srcPositions

  println("Printing Sample Input ")
  println("*********************")
  println("*********************")
  println(problem.toString)
  println("*********************")
  println("*********************")

  val p = Polygon(problem.silhouette.polyList.head.vertices)
  println("Polygon", p)
  println("Polygon", p.getEdges)
  println("Area of polygon", p.areaUnderThePolygon)
  println("Area of polygon abs", p.areaUnderThePolygonAbs)
  println("Mid Point", p.getMidPoint)

}
