package DataStructures

/**
  * Created by greddy on 8/5/16.
  */

trait ProblemT {

  val silhouette:SilhouetteT
  val skeleton: SkeletonT

  def boundaryEdges:Seq[EdgeT] = {
    val f0 = FractionPointCoordinate(0,1)
    val f1 = FractionPointCoordinate(1,1)
    val point0 = Point(f0,f0)
    val point1 = Point(f1,f0)
    val point2 = Point(f1,f1)
    val point3 = Point(f0,f1)

    List(Edge(point0,point1), Edge(point1,point2),Edge(point2,point3),Edge(point3,point0))
  }

  def boundaryPoints:Seq[PointT] = {
    val f0 = FractionPointCoordinate(0,1)
    val f1 = FractionPointCoordinate(1,1)
    val point0 = Point(f0,f0)
    val point1 = Point(f1,f0)
    val point2 = Point(f1,f1)
    val point3 = Point(f0,f1)

    List(point0,point1,point2,point3)
  }

  override def toString = silhouette.toString + "\n" + skeleton.toString
}

case class Problem(silhouette: SilhouetteT, skeleton: SkeletonT) extends ProblemT

object ProblemExample extends App {

  val addr = "input.txt"
  val problem = ReadInput(addr).problem

  println("A simple problem ")
  println(problem)
}