import org.graalvm.compiler.lir.sparc.SPARCArithmetic.RemOp.Rem

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

object P05 {
  def reversePro[A](l: List[A]): List[A] = l.reverse

  def reverseRec[A](l: List[A]): List[A] = l match {
    case h :: tail => reverseRec(tail) ::: List(h)
    case Nil => Nil
  }

  def reverseTailRec[A](l: List[A]): List[A] = {
    def _reverseTailRec(res: List[A], rem: List[A]): List[A] = rem match {
      case Nil => res
      case h :: tail => _reverseTailRec(h:: res, tail)
    }
    _reverseTailRec(Nil, l)
  }

  def reverseFold[A](l: List[A]): List[A] = l.foldLeft(List[A]()) { (res, h) => h :: res}

  def main(args: Array[String]): Unit = {
    val l = List(1, 1, 2, 3, 5, 8)
    println(reversePro(l))
    println(reverseRec(l))
    println(reverseTailRec(l))
    println(reverseFold(l))
  }
}

object flr {
  def unapply[A] (l: List[A]) = l match {
    case Nil => None
    case l if (l.length == 1) => Some(l.head, List(), l.last)
    case l => Some(l.head, l.init.tail, l.last)
  }
}

object P06 {

  // note that this one uses additional stack frames - but short circuits
  def isPalindromeRec[A](l: List[A]): Boolean = l match {
    case Nil => true
    case List(_) => true
    case list => list.head == list.last && isPalindromeRec(list.tail.init)
  }

  // this one does not use additional stack frames (tail recursive) - but must go through entire list
  // to determine if it is a palindrome
  def isPalindromeTailRec[A](l: List[A]): Boolean = {
    def _isPalindromeTailRec(res: Boolean, rem: List[A]): Boolean = rem match {
      case Nil => res
      case List(a) => res
      case list => _isPalindromeTailRec(res && rem.head == rem.last, rem.tail.init)
    }
    _isPalindromeTailRec(true, l)
  }

  def isPalindromeRec2[A](l: List[A]): Boolean = l match {
    case Nil => true
    case List(_) => true
    case flr(first, rem, last) => (first == last) && isPalindromeRec2(rem)
  }


  def main(args: Array[String]): Unit = {
    assert(isPalindromeRec(List(1, 2, 3, 2, 1)))
    assert(!isPalindromeRec(List(1, 3, 3, 2, 1)))
    assert(isPalindromeTailRec(List(1, 2, 3, 2, 1)))
    assert(!isPalindromeTailRec(List(1, 3, 3, 2, 1)))
    assert(isPalindromeRec2(List(1, 2, 3, 2, 1)))
    assert(!isPalindromeRec2(List(1, 3, 3, 2, 1)))
    println("P06 passed")
  }
}




















































