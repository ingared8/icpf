package DataStructures

import java.io.{BufferedWriter, File, FileWriter}

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
    //polygon.getEdges.forall(isEdgeLegal) &&
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
    val adjNodes = adjNodeList1.union(adjNodeList2).toList


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
            case Some(adjnode) => adjnode.neighbors.flatMap(x => dfs(x, newVisited, newPath))
          }
        }
      }

      val res = dfs(start, visited, path)

      res.filter(x => x match {
        case None => false
        case _ => true
      }).map { case Some(x) => x }
    }


    def helper(l: Seq[PointT]):Seq[PointT]= {
      val list = l.toList
      val h = list.head
      val t = list.last
      val b = list.tail.take(list.size - 2)
      h :: t :: b.reverse
    }


    dfsCycles(adjNodes).filter(x => x.length > 2).map(x => Polygon(x)).map{
      polygon => Polygon(helper(polygon.vertices))}

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

  def toIndexState(gstate:GState): (ProblemT,SolutionT) ={

    val inp:SolutionPointsT = gstate._2
    def getIndexOfPoint(point: PointT,l:List[(PointT,Int)]):Int = {
      l.find(x => x._1 == point) match {
        case None => 0
        case Some(a) => a._2
      }
    }

    val src = inp.srcPositions.toList.zipWithIndex
    val facets = inp.facets.map(p => Facet(p.vertices.map(point => getIndexOfPoint(point,src))))

    (gstate._1, Solution(inp.srcPositions,facets,inp.destPositions))
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
      val projectedPolygon = polygon.mirrorImage
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

  def getPolygons(gState: GState,polygon: PolygonT): Option[GState] = {
    if (!isPolygonLegal(polygon) || !isProblemLegal(gState._1)) None
    else {
      val edge = Edge(polygon.vertices.head, polygon.vertices.tail.head)
      val projectedPolygon = polygon.mirrorImage
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

    val res = getInnerEdges(problem).flatMap(x => getFacetPolygons(problem,x)).
      flatMap( poly => updateState((problem,initSolution),poly))

    res.map{ x => toIndexState(x)._2}
  }

  def solver2(problem: Problem):SolutionPointsT= {

    // Initial Solution
   getInitialSolution(problem)

  }
}

object ProblemSolver extends App(){

  import Solver._

  for (i <- 1 until 1000) yield {

    val addr =  i + ".txt"
    try {
      val problem = ReadInput("problems/" + i + ".txt").problem
      val res = solver2(problem)

      val file = new File("solutions/" + i + ".txt")
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(res.toString)
      bw.close()

    } catch {
      case e:Exception => {
        println("exception")
        //e.printStackTrace()
      }
    }

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

  println("Result", isIdeal1, isIdeal2)

  val solution1 = getInitialSolution(problem)

  println("**************")
  println("**************")
  println(solution1.toString)


  val facets = getFacetPolygons(problem, problem.skeleton.edges(2)): Seq[PolygonT]

  //facets.foreach(println(_, "GG"))
  //facets.foreach(p => println(isPolygonLegal(p)))
  val mirrorImages = facets.map(x => x.mirrorImage)
  //mirrorImages.foreach(println)
  //mirrorImages.foreach(x => println(isPolygonLegal(x)))
  val updats = updateState((problem, solution1), mirrorImages(2)).get

  val finn = toIndexState(updats)

  println("Gnaga")
  println(updats._2.facets)
  println(finn._2.facets)

  val xx = solver(problem)

  println(getInnerEdges(problem))
  //xx.foreach(println(_, "*********"))
}
