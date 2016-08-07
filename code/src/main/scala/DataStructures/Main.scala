package DataStructures

import cats.Monad
import cats.data.{State, StateT}
import com.fasterxml.jackson.annotation.ObjectIdGenerators

/**
  * Created by greddy on 8/5/16.
  */
object Solver  {

  trait AdjEdgeT {
    val edge: EdgeT
    val neighbors: Seq[(EdgeT,FractionT)]
  }

  trait AdjNodeT {
    val node:PointT
    val neighbors: Seq[PointT]
  }

  case class AdjEdge(edge:EdgeT,neighbors:Seq[(EdgeT,FractionT)]) extends AdjEdgeT
  case class AdjNode(node: PointT, neighbors:Seq[PointT]) extends AdjNodeT

  /*
  type GState = Seq[AdjEdge]

  def getState(problem: ProblemT):GState = {
    val ske = problem.skeleton
    ske.edges.map(x => AdjEdge(x,x.neighbors(ske.edges)))
    ???
  }

  */

  def getOuterEdges(problem: ProblemT):Seq[EdgeT] = {
    problem.silhouette.polyList.flatMap{
       polygon =>  {
         val pairs:Seq[PointT] = polygon.vertices.tail.toList :+ polygon.vertices.head
         polygon.vertices.zip(pairs).map { case (a) => Edge(a._1,a._2)}
      }
    }
  }

  def getInnerEdges(problem: ProblemT):Seq[EdgeT] = {
    lazy val outerEdges = getOuterEdges(problem)
    problem.skeleton.edges.filter { case Edge(a, b) =>
      !outerEdges.contains(Edge(a, b)) &&
        !outerEdges.contains(Edge(b, a))
    }
  }

  def getAllPointsWithCounts(problem: ProblemT):Seq[(PointT,Int)] = {
    val aa = problem.skeleton.edges.map{ case Edge(a,b) => a}.toList
    val bb = problem.skeleton.edges.map{ case Edge(a,b) => b}.toList
    val cd = aa ::: bb
    cd.map(x => (x, cd.count(p=> p == x))).distinct
  }

  def getAllPoints(edges:Seq[EdgeT]):Seq[PointT] = {
    val aa = edges.map{ case Edge(a,b) => a}.toList
    val bb = edges.map{ case Edge(a,b) => b}.toList
    val cd = aa ::: bb
    cd.distinct
  }

  def getInnerPoints(problem: ProblemT):Seq[(PointT,Int)] = {
    val boundaryPoints = problem.boundaryPoints
    getAllPointsWithCounts(problem).filterNot{ case(p,k) => boundaryPoints.contains(p)}
  }

  def getFoldingEdges(problem: ProblemT):Seq[EdgeT] = {
    lazy val boundaryEdges = problem.boundaryEdges
    problem.skeleton.edges.filter { case Edge(a, b) =>
      !boundaryEdges.contains(Edge(a, b)) &&
        !boundaryEdges.contains(Edge(b, a))
    }
  }

  // Other Conditions to check
  def isEdgeFoldable(edgeT: EdgeT,problem: ProblemT):Boolean = ???
  def isEdgeOutSide(edge:EdgeT, problem: ProblemT):Boolean = ???

  // Conditions to stop

  def isCompleteUnfold(problem: ProblemT):Boolean = {

    def skeletonShouldContainFourEdges: Boolean =
      problem.skeleton.edges.toSet == problem.boundaryEdges.toSet

    def polygonShouldbeOne: Boolean =
      problem.silhouette.polyList.forall { polygon => polygon.areaUnderThePolygonAbs == 1 }

    def numOfPolygonsShouldBeOne:Boolean =
      problem.silhouette.polyList.size == 1

    numOfPolygonsShouldBeOne && skeletonShouldContainFourEdges && polygonShouldbeOne

  }

  def isSkeletonLegal(skeleton: SkeletonT):Boolean = {
    skeleton.numOfEdges >= 4 &&
    skeleton.edges.forall(edge => isEdgeLegal(edge))
  }

  def isSilhouetteLegal(sil :SilhouetteT):Boolean = {
    sil.polyList.forall(isPolygonLegal) &&
    sil.numOfPolygons >= 1
  }

  def isEdgeLegal(edge: EdgeT):Boolean= {
      (edge.distanceSquare <= 2) && edge.p != edge.q
  }

  def isPointLegal(point: PointT):Boolean = {
    point.x <= 1 &&
    point.x >= 0 &&
    point.y <= 1 &&
    point.y >= 0
  }

  def isPolygonLegal(polygon: PolygonT):Boolean = {
    polygon.vertices.size > 2   &&
    polygon.vertices.forall(isPointLegal) &&
    polygon.getEdges.forall(isEdgeLegal) &&
    polygon.areaUnderThePolygon != 0 // No need to check the area any projection of three points will be a line
  }

  def isPolygonLegal(points: Seq[PointT]):Boolean = {
    val polygon = Polygon(points)
    polygon.vertices.size > 2   &&
    polygon.vertices.forall(isPointLegal) &&
    polygon.getEdges.forall(isEdgeLegal) &&
    polygon.areaUnderThePolygon != 0 //
  }

  def isFacetLegal(facet: FacetT, pointsDict:Seq[PointT]):Boolean = {
      isPolygonLegal(facet.getPolygon(pointsDict))
  }

  def isProblemLegal(problem: ProblemT):Boolean = {
    val skeleton = problem.skeleton
    val sil = problem.silhouette
    isSkeletonLegal(skeleton) && isSilhouetteLegal(sil)
  }

  type GState = (ProblemT,SolutionPointsT)


  def getFacetPolygons(problem: ProblemT,edge: EdgeT):Seq[PolygonT] = {

    // Create a graph and solve using backtracking
    val nodes = getAllPoints(problem.skeleton.edges)
    val edges = problem.skeleton.edges.union(problem.boundaryEdges)
    val adjEdgeList = edges.map(x => AdjEdge(x, x.neighborEdges(edges)))
    val adjNodeList1 = edges.map(x => AdjNode(x.p, x.neighborNodes(x.p, edges)))
    val adjNodeList2 = edges.map(x => AdjNode(x.q, x.neighborNodes(x.q, edges)))
    val adjNodes = adjNodeList1.union(adjNodeList2).distinct.toList

    adjNodes.foreach(println)

    adjEdgeList.foreach(println)


    def dfsCycles(adjNodes: List[AdjNode]): Seq[Seq[PointT]] = {

      val start = edge.p
      val end = edge.q
      val path = List[PointT]()
      val visited = List[PointT]()

      def dfs(node: PointT, visited: List[PointT], path: List[PointT]): Seq[Option[Seq[PointT]]] = {
        if (node == end) List(Some(end::path))
        else if (visited.contains(node)) List()
        else {
          val neighbors = adjNodes.find(x => x.node == node)
          val newVisited = node :: visited
          val newPath = node :: path
          neighbors match {
            case None => List()
            case Some(adjnode) => {
              adjnode.neighbors.flatMap(x => dfs(x, newVisited, newPath))
            }
          }
        }
      }

      val res = dfs(start, visited, path)

      res
      res.filter(x => x match {
        case None => false
        case _ => true
      }).map { case Some(x) => x }
    }

    dfsCycles(adjNodes).filter(x => x.length > 2).map(x => Polygon(x))

  }

  def unfold(gstate:GState ,edge: EdgeT):Seq[GState] = {

    val problem = gstate._1
    val solution = gstate._2
    val facetPolygon = getFacetPolygons(problem, edge)

    facetPolygon.map(polygon => updateState(gstate,polygon))
      .map{ case Some(validState) => validState }
  }


  def getInitialSolution(problem: ProblemT):SolutionPointsT = {
      val srcPositons = getAllPointsWithCounts(problem).map(a => a._1).distinct
      SolutionPoints(srcPositons,List[PolygonT](),srcPositons)
  }

  def updateSil(silhouette: SilhouetteT,facet: PolygonT):SilhouetteT = {
      val newSil = silhouette.polyList.map { polygon =>
      val a = facet.vertices.head
      val b = facet.vertices.tail.head
      val index = polygon.vertices.indexOf(a)
      val newPolygon = Polygon(polygon.vertices.splitAt(index)._1.toList ::: facet.vertices.tail.tail.toList)
      newPolygon }

    Silhouette(newSil)
  }

  def updateSkeleton(skeleton: SkeletonT,facet: PolygonT):SkeletonT = {
      val res = skeleton.edges.filterNot(edge => edge == facet.myEdge)
      Skeleton(res)
  }

  // Project all the polygons which lies to new

  def projectExistingFacets(polygon: PolygonT, facets:Seq[PolygonT]):Seq[PolygonT] ={
      val additions = facets.filter(facet => polygon.contains(facet))
      .map(facet => Polygon(polygon.myEdge.mirrorImageOfSeqOfPoints(facet.vertices)))

    additions
  }

  /**
    * project the facet along the the axis
    *
    * @param gState
    * @param polygon
    * @return
    */

  def updateState(gState: GState,polygon: PolygonT): Option[GState] = {
    if (!isPolygonLegal(polygon) || !isProblemLegal(gState._1)) None
    else {
      val edge = Edge(polygon.vertices.head, polygon.vertices.tail.head)
      val projectedPolygon = Polygon(edge.mirrorImageOfSeqOfPoints(polygon.vertices.tail.tail))
      if (!isPolygonLegal(projectedPolygon)) None
      else {
        val newFacets = projectExistingFacets(polygon,gState._2.facets)
        val newPositions = gState._2.srcPositions // Needs update

        val newSil = updateSil(gState._1.silhouette, projectedPolygon)
        val newSkeleton = updateSkeleton(gState._1.skeleton, projectedPolygon)
        val newProblem:ProblemT = Problem(newSil, newSkeleton)
        val oldSol = gState._2
        val newSol:SolutionPointsT = SolutionPoints(oldSol.srcPositions,newFacets,oldSol.srcPositions)
        val res = (newProblem,newSol)
        Some(res)
      }
    }
  }

  def solver(problem: Problem):Seq[SolutionT]= {

    // Initial Solution
    val initSolution = getInitialSolution(problem)


    ???
  }


}

object ProblemSolutionPrintTest extends App {

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

  println("Outer Edges")

  val outerEdges = getOuterEdges(problem)
  println(problem.silhouette.polyList.head.vertices)
  outerEdges.foreach(println)
  //println(outerEdges)

  println("Inner Edges")
  println("All edges ", problem.skeleton.edges)
  val innerEdges = getInnerEdges(problem)
  innerEdges.foreach(println)

  println("Folding Edges")
  val foldingEdges = getFoldingEdges(problem)
  foldingEdges.foreach(println)

  println("get All points")
  val allPoints = getAllPointsWithCounts(problem)
  allPoints.foreach(println)

  println("Inner Points")
  val innerPoints = getInnerPoints(problem)
  innerPoints.foreach(println)

  val completedSkeleton = Skeleton(problem.boundaryEdges.reverse)

  val idealProblem = ReadInput("ideal.txt").problem
  val isIdeal1 = isCompleteUnfold(idealProblem)
  val isIdeal2 = isCompleteUnfold(problem)

  println("Result",isIdeal1,isIdeal2)

  val solution1 = getInitialSolution(problem)

  println("**************")
  println("**************")
  println(solution1.toString)


  println("gangaReddy".toCharArray.splitAt(5)._1.foreach(print))
  println("gangaReddy".toCharArray.splitAt(5)._2.foreach(print))

  val facets = getFacetPolygons(problem,problem.skeleton.edges(2)):Seq[PolygonT]
  println(facets)

}
