package DataStructures

/**
  * Created by greddy on 8/5/16.
  */


trait FacetT  {

  val verticesIndex: Seq[Int]

  override def toString:String = {
    verticesIndex.size + " " +  verticesIndex.map(_.toString).mkString(" ")
  }

  def getVertexIndex(p: PointT, points:Seq[PointT]):Int = points.indexOf(p)

  def getVertexIndex(points:Seq[PointT],pointsDict:Seq[PointT]):Seq[Int] =
      points.map(p => getVertexIndex(p,pointsDict))

  def getPolygon(pointsDict:Seq[PointT]):Polygon = {
    Polygon(verticesIndex.map(x => pointsDict(x)))
  }

}

case class Facet(verticesIndex:Seq[Int]) extends FacetT

object FacetExample extends App {

  val facet1 = Facet(List(0,1,5,4))
  val facet2 =  Facet(List(2,6,5))
  val facet3 = Facet(List(4,5,3))
  val facet4 = Facet(List(4,5,6,3))

  println(facet1)
  println(facet2)
  println(facet4)

  val facets = Seq(facet1,facet2,facet3)


}