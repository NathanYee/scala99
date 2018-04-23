object P01 {
  def last[A](xs: List[A]): A = xs match {
    case h :: Nil => h
    case h :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }

  def main(args: Array[String]): Unit = {
    println(last(List(1, 1, 2, 3, 5, 8)))
  }
}