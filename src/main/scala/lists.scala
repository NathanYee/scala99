import org.graalvm.compiler.lir.sparc.SPARCArithmetic.RemOp.Rem

// Find the last element of a list.
object P01 {
  def last[A](l: List[A]): A = l match {
    case h :: Nil  => h
    case _ :: tail => last(tail)
    case _         => throw new NoSuchElementException
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
    case _ :: tail    => penultimateRec(tail)
    case _            => throw new NoSuchElementException
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
    case tail if tail.length == n =>
      tail.head // tail is the whole list in this case
    case _ :: tail => lastNthRec(n, tail)
    case _         => throw new NoSuchElementException
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
      case e: IndexOutOfBoundsException => throw new NoSuchElementException
    }
  }

  def nthRec1[A](n: Int, l: List[A]): A = (n, l) match {
    case (0, h :: _)               => h
    case (`n`, _ :: tail) if n > 0 => nthRec1(n - 1, tail)
    case _                         => throw new NoSuchElementException
  }

  def nthRec2[A](n: Int, l: List[A]): A = n match {
    case 0            => l.head
    case `n` if n > 0 => nthRec2(n - 1, l.tail)
    case _            => throw new NoSuchElementException
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
    case Nil       => 0
    case _ :: tail => 1 + lengthRec(tail)
  }

  def lengthFold[A](l: List[A]): Int = l.foldLeft(0) { (c, _) =>
    c + 1
  }

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
    case Nil       => Nil
  }

  def reverseTailRec[A](l: List[A]): List[A] = {
    def _reverseTailRec(res: List[A], rem: List[A]): List[A] = rem match {
      case Nil       => res
      case h :: tail => _reverseTailRec(h :: res, tail)
    }

    _reverseTailRec(Nil, l)
  }

  def reverseFold[A](l: List[A]): List[A] = l.foldLeft(List[A]()) { (res, h) =>
    h :: res
  }

  def main(args: Array[String]): Unit = {
    val l = List(1, 1, 2, 3, 5, 8)
    println(reversePro(l))
    println(reverseRec(l))
    println(reverseTailRec(l))
    println(reverseFold(l))
  }
}

object flr {
  def unapply[A](l: List[A]) = l match {
    case Nil                  => None
    case l if (l.length == 1) => Some(l.head, List(), l.last)
    case l                    => Some(l.head, l.init.tail, l.last)
  }
}

object P06 {

  // note that this one uses additional stack frames - but short circuits
  def isPalindromeRec[A](l: List[A]): Boolean = l match {
    case Nil     => true
    case List(_) => true
    case list    => list.head == list.last && isPalindromeRec(list.tail.init)
  }

  // this one does not use additional stack frames (tail recursive) - but must go through entire list
  // to determine if it is a palindrome
  def isPalindromeTailRec[A](l: List[A]): Boolean = {
    def _isPalindromeTailRec(res: Boolean, rem: List[A]): Boolean = rem match {
      case Nil     => res
      case List(a) => res
      case list =>
        _isPalindromeTailRec(res && rem.head == rem.last, rem.tail.init)
    }

    _isPalindromeTailRec(true, l)
  }

  def isPalindromeRec2[A](l: List[A]): Boolean = l match {
    case Nil                   => true
    case List(_)               => true
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

object P07 {

  // h:List[_] is a type pattern
  // type of h will be checked in runtime against List[_]
  // Since _ is a wild card, List[_] is a list of any type
  //
  // (h:List[_]) is a List, h, of any type
  // keep calling flatten until h is no longer a list
  //
  // once h is a single element in a list, cons with the flattened tail
  def flatten(l: List[Any]): List[Any] = l match {
    case Nil                  => Nil
    case (h: List[_]) :: tail => flatten(h) ::: flatten(tail)
    case h :: tail            => h :: flatten(tail)
  }

  // TODO: figure out what h:List[_] is doing and how it uses case (h:List[_])::tail
  // might be Type ascription, https://stackoverflow.com/questions/2087250/what-is-the-purpose-of-type-ascriptions-in-scala
  def flattenTail(l: List[Any]): List[Any] = {
    def _flattenTail(res: List[Any], rem: List[Any]): List[Any] = rem match {
      case Nil => res
      case (h: List[_]) :: Nil =>
        _flattenTail(res, h) // turns List(List()) into List()
      case (h: List[_]) :: tail => _flattenTail(res ::: h, tail)
      case h :: tail            => _flattenTail(res ::: List(h), tail)
    }

    _flattenTail(List(), l)
  }

  // using flatMap
  def flatten2(l: List[Any]): List[Any] = l flatMap {
    case ls: List[_] => flatten2(ls)
    case h           => List(h)
  }

  def main(args: Array[String]): Unit = {
    println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
    println(flattenTail(List(List(1, 1), 2, List(3, List(List(5, 8))))))
    println(flattenTail(List(List(1, 2, 3))))
    println(flatten2(List(List(1, 2, 3))))
  }
}

object P08 {
  def compressRec[A](l: List[A]): List[A] = l match {
    case Nil         => Nil
    case h :: List() => List(h)
    case h :: tail =>
      if (h == tail.head) compressRec(tail) else h :: compressRec(tail)
  }

  // I did this myself!
  def compressTail[A](l: List[A]): List[A] = {
    def _compressTail(res: List[A], rem: List[A]): List[A] = rem match {
      case Nil                           => Nil
      case h :: List()                   => res ::: List(h)
      case h :: tail if (h == tail.head) => _compressTail(res, tail)
      case h :: tail                     => _compressTail(res ::: List(h), tail)
    }

    _compressTail(List(), l)
  }

  //  {
  //    case (List(), e) => List(e)
  //    case (ls, e) if (ls.last == e) => ls
  //    case (ls, e) => ls:::List(e)
  //  }
  def compressFold[A](l: List[A]): List[A] = l.foldLeft(List[A]()) {
    case (ls, e) if (ls.isEmpty || ls.last != e) => ls ::: List(e)
    case (ls, e)                                 => ls
  }

  def compressRight[A](l: List[A]): List[A] = l.foldRight(List[A]()) {
    case (e, ls) if ls.isEmpty => List(e)
    case (e, ls)               => if (ls.head != e) e :: ls else ls
  }

  def compressRight2[A](l: List[A]): List[A] = l.foldRight(List[A]()) {
    case (e, ls) if (ls.isEmpty || ls.head != e) => e :: ls
    case (e, ls)                                 => ls
  }

  def main(args: Array[String]): Unit = {
    println(
      compressRec(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(
      compressTail(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(
      compressFold(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(
      compressRight(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(
      compressRight2(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}

object P09 {
  // this is so cool!
  def pack[A](l: List[A]): List[List[A]] = {
    def _pack(res: List[List[A]], rem: List[A]): List[List[A]] = rem match {
      case Nil => res
      // create a new list for the non duplicate element
      case h :: tail if (res.isEmpty || res.last.head != h) =>
        _pack(res ::: List(List(h)), tail)
      // if h is duplicate, join it with the last list and join that to the end other lists
      case h :: tail => _pack(res.init ::: List(res.last ::: List(h)), tail)
    }

    _pack(List(), l)
  }

  def main(args: Array[String]): Unit = {
    println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}

object P10 {
  def encode[A](l: List[A]): List[(Int, A)] = {
    def _encode(res: List[(Int, A)], rem: List[List[A]]): List[(Int, A)] =
      rem match {
        case Nil       => res
        case h :: tail => _encode(res ::: List((h.length, h.head)), tail)
      }
    _encode(List(), P09.pack(l))
  }

  def encodeMap[A](l: List[A]): List[(Int, A)] = {
    P09.pack(l) map { e =>
      (e.length, e.head)
    }
  }

  def main(args: Array[String]): Unit = {
    println(
      encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(
      encodeMap(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}

object P11 {
  def encodeModified[A](l: List[A]): List[Either[A, (Int, A)]] = {
    P09.pack(l) map {
      case e if e.length == 1 => Left(e.head)
      case e                  => Right((e.length, e.head))
    }
  }

  def encodeUnsafe[A](l: List[A]): List[Any] = {
    P09.pack(l) map {
      case e if e.length == 1 => e.head
      case e                  => (e.length, e.head)
    }
  }

  def main(args: Array[String]): Unit = {
    println(
      encodeModified(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(
      encodeUnsafe(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}

object P12 {
  // my recursive solution!
  def decode[A](l: List[(Int, A)]): List[A] = {
    def _decode(res: List[A], rem: List[(Int, A)]): List[A] = rem match {
      case Nil => res
      case h :: tail =>
        h match {
          case (0, _) => _decode(res, tail)
          case (n, h) => _decode(res ::: List(h), (n - 1, h) :: tail)
        }
    }
    _decode(List(), l)
  }

  // using flatMap
  def decodeFlatMap[A](l: List[(Int, A)]): List[A] = {
    def _expand[A](res: List[A], rem: (Int, A)): List[A] = rem match {
      case (0, _) => res
      case (n, h) => _expand(res ::: List(h), (n - 1, h))
    }
    l flatMap { e =>
      _expand(List(), e)
    }
  }

  // using fill function
  def decodeFill[A](l: List[(Int, A)]): List[A] = {
    l flatMap { e =>
      List.fill(e._1)(e._2)
    }
  }

  def main(args: Array[String]): Unit = {
    val in = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    val out = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    println(decode(in))
    assert(decode(in) == out)

    println(decodeFlatMap(in))
    assert(decodeFlatMap(in) == out)

    println(decodeFill(in))
    assert(decodeFill(in) == out)
  }
}

object P13 {
  def encodeDirect[A](l: List[A]): List[(Int, A)] = {
    def _encodeDirect(res: List[(Int, A)], rem: List[A]): List[(Int, A)] =
      rem match {
        case Nil => res
        case ls => {
          val (s, r) = rem span { _ == rem.head }
          _encodeDirect(res ::: List((s.length, s.head)), r)
        }
      }
    _encodeDirect(List(), l)
  }

  def main(args: Array[String]): Unit = {
    val in = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val out = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    println(encodeDirect(in))
    assert(encodeDirect(in) == out)
  }
}

object P14 {

  def duplicate[A](l: List[A]): List[A] = {
    def _duplicate(res: List[A], rem: List[A]): List[A] = rem match {
      case Nil       => res
      case h :: tail => _duplicate(res ::: List(h, h), tail)
    }
    _duplicate(List(), l)
  }

  def duplicateFlatMap[A](l: List[A]): List[A] = {
    l flatMap { e =>
      List(e, e)
    }
  }

  def main(args: Array[String]): Unit = {
    val in = List('a, 'b, 'c, 'c, 'd)
    val out = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)

    println(duplicate(in))
    assert(duplicate(in) == out)

    println(duplicateFlatMap(in))
    assert(duplicateFlatMap(in) == out)
  }
}

object P15 {

  def duplicateN[A](n: Int, l: List[A]): List[A] = {
//    l flatMap { e => List.fill(n)(e) }
    l flatMap { List.fill(n)(_) }
  }

  def main(args: Array[String]): Unit = {
    val in = List('a, 'b, 'c, 'c, 'd)
    val out = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)

    println(duplicateN(3, in))
    assert(duplicateN(3, in) == out)

  }
}
