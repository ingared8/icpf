package DataStructures

/**
  * Created by greddy on 8/7/16.
  */


import scalaj.http.Http

object API extends App {
  val KEY = "54-2308a3a7aa9d971f3c5beba539565513"
  val keyHeader = ("X-API-Key", KEY)
  val helloWorldURL = "http://2016sv.icfpcontest.org/api/hello"
  def blobLookupURL(hash: String): String = s"http://2016sv.icfpcontest.org/api/blob/$hash"
  val snapshotURL = "http://2016sv.icfpcontest.org/api/snapshot/list"
  val problemSubmitURL = "http://2016sv.icfpcontest.org/api/problem/submit"
  val solutionSubmitURL = "http://2016sv.icfpcontest.org/api/solution/submit"

  def helloWorld() = {
    val body = Http(helloWorldURL).headers(keyHeader).asString.body
  }

  val res = Http(helloWorldURL).headers(keyHeader).asString.body
  println(res)

  def body(s:String) = Http(blobLookupURL(s)).headers(keyHeader).asString.body

  println(body("7482c1c4916ed7cdec61f3bd98eb3a462bafb305"))

}