package DataStructures

/**
  * Created by greddy on 8/5/16.
  */
trait PointT {

  val x:FractionT
  val y:FractionT

  override def toString:String =  x.toString + "," + y.toString

  def + (point:PointT):PointT
  def negate:PointT
  def - (point:PointT):PointT

  def * (point: PointT):PointT

  def / (point: PointT):PointT

  def * (i:Int):PointT

  def / (i:Int):PointT

  def abs:PointT

  def distance(point: PointT):FractionT

  def moduls:FractionT

  def == (point: PointT):Boolean = {
    (this.x == point.x) && (this.y == point.y)
  }

}

//case class Point(x:FractionPointCoordinate,y:FractionPointCoordinate) extends PointT
case class Point(x:FractionT,y:FractionT) extends PointT {

  override def + (point:PointT):PointT = {
    Point(this.x + point.y, this.y + point.y)
  }

  override def negate:PointT = {
    Point(this.x.negate, this.y.negate)
  }

  override def- (point:PointT):PointT = {
    this + point.negate
  }

  override def * (point: PointT):PointT = {
    Point(this.x * point.x , this.y *point.y)
  }

  override def / (point: PointT):PointT = {
    Point(this.x / point.x , this.y / point.y)
  }

  override def * (i:Int):PointT = {
    Point(this.x * i , this.y *i)
  }

  override def / (i:Int):PointT = {
    Point(this.x / i , this.y / i)
  }

  override def abs:PointT = {
    Point(this.x.abs, this.y.abs)
  }

  override def distance(point: PointT):FractionT = {
    (this.x - point.x)*(this.x -point.x) +
      (this.y - point.y)*(this.y - point.y)
  }

  override def moduls:FractionT = {
    (this.x * this.x) + (this.y * this.y )
  }

  override def == (point: PointT):Boolean = {
    (this.x == point.x) && (this.y == point.y)
  }


}

object PointExample extends App {

  val f1 = FractionPointCoordinate(1,2)
  val f2 = FractionPointCoordinate(2,1)
  val point = Point(f1,f2)

  println(point)

}
