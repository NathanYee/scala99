// Find the last element of a list.
object P01 {
  def last[A](l: List[A]): A = l match {
    case h :: Nil => h
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }

  def last2[A](l: List[A]): A = l.last

  def main(args: Array[String]): Unit = {
    println(last(List(1, 1, 2, 3, 5, 8)))
    println(last2(List(1, 1, 2, 3, 5, 8)))
  }
}


// Find the last but one element of a list.
object P02 {
  // List(t) matches to a list with 1 element
  def penultimateRec[A](l: List[A]): A = l match {
//    case p :: h :: Nil => p
    case h :: List(t) => h
    case _ :: tail => penultimateRec(tail)
    case _ => throw new NoSuchElementException
  }

  // init returns all elements except the last
  def penultimatePro[A](l: List[A]): A = {
    if (l.isEmpty) throw new NoSuchElementException
    l.init.last
  }

  // takeRight retrieves the last n elements from a list
  // head returns the head of a given list
  def lastNthPro[A](n: Int, l: List[A]): A = {
    if (n <= 0) throw new IllegalArgumentException
    if (n > l.length) throw new NoSuchElementException
    l.takeRight(n).head
  }

  // you can use pattern guards to add if expressions (guards) to match case expressions
  def lastNthRec[A](n: Int, l: List[A]): A = l match {
    case tail if tail.length == n => tail.head // tail is the whole list in this case
    case _ :: tail => lastNthRec(n, tail)
    case _ => throw new NoSuchElementException
  }

  def main(args: Array[String]): Unit = {
    println(penultimateRec(List(1, 1, 2, 3, 5, 8)))

    println(penultimatePro(List(1, 1, 2, 3, 5, 8)))

    println(lastNthPro(3, List(1, 1, 2, 3, 5, 8)))

    println(lastNthRec(3, List(1, 1, 2, 3, 5, 8)))

    assert(lastNthPro(3, List(1, 1, 2, 3, 5, 8)) == 3)
    assert(lastNthRec(3, List(1, 1, 2, 3, 5, 8)) == 3)
    println("P02 passed")
  }
}


object P03 {

  def nthPro[A](k: Int, l: List[A]): A = {
    try l(k)
    catch {
      case e:IndexOutOfBoundsException => throw new NoSuchElementException
    }
  }

  def nthRec1[A](n: Int, l: List[A]): A = (n,l) match {
    case (0, h::_) => h
    case (`n`, _::tail) if n > 0 => nthRec1(n - 1, tail)
    case _ => throw new NoSuchElementException
  }

  def nthRec2[A](n: Int, l: List[A]): A = n match {
    case 0 => l.head
    case `n` if n > 0 => nthRec2(n - 1, l.tail)
    case _ => throw new NoSuchElementException
  }

  def main(args: Array[String]): Unit = {
    assert(nthPro(2, List(1, 1, 2, 3, 5, 8)) == 2)
    assert(nthRec1(2, List(1, 1, 2, 3, 5, 8)) == 2)
    assert(nthRec2(2, List(1, 1, 2, 3, 5, 8)) == 2)
    println("P03 passed")
  }

}

object P04 {
  def lengthPro[A](l: List[A]): Int = l.length

  def lengthRec[A](l: List[A]): Int = l match {
    case Nil => 0
    case _ :: tail => 1 + lengthRec(tail)
  }

  def lengthFold[A](l: List[A]): Int = l.foldLeft(0) { (c,_) => c + 1}

  def main(args: Array[String]): Unit = {
    assert(lengthPro(List(1, 1, 2, 3, 5, 8)) == 6)
    assert(lengthRec(List(1, 1, 2, 3, 5, 8)) == 6)
    assert(lengthFold(List(1, 1, 2, 3, 5, 8, 9)) == 7)
    println("P04 passed")
  }
}


















































