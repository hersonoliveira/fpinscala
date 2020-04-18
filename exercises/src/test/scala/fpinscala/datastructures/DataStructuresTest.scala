package fpinscala.datastructures

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DataStructuresTest extends AnyFlatSpec with Matchers {

  "length" should "return correct List size" in {
    val l = List(1, 2, 4, 5)

    val result = List.length(l)

    result should be(4)
  }

  "foldLeft" should "return correct sum of list" in {
    val l = List(1, 5, 6)
    val expected = 12

    val result = List.foldLeft(l, 0)(_ + _)

    result shouldBe expected
  }

  "sum3" should "return correct sum of list" in {
    val l = List(1, 5, 6)
    val expected = 12

    val result = List.sum3(l)

    result shouldBe expected
  }

  "reverse" should "return list reversed" in {
    val l = List(1, 5, 6)
    val expected = List(6, 5, 1)

    val result = List.reverse(l)

    result shouldBe expected
  }

  "appendViaFoldLeft" should "append list correctly" in {
    val l1 = List(1, 2, 3)
    val l2 = List(4, 5, 6)
    val expected = List(1, 2, 3, 4, 5, 6)

    val result = List.appendViaFoldLeft(l1, l2)

    result shouldBe expected
  }

  "appendViaFoldRight" should "append list correctly" in {
    val l1 = List(1, 2, 3)
    val l2 = List(4, 5, 6)
    val expected = List(1, 2, 3, 4, 5, 6)

    val result = List.appendViaFoldRight(l1, l2)

    result shouldBe expected
  }

  "concatLists" should "concat lists into a single list" in {
    val l = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    val expected = List(1, 2, 3, 4, 5, 6, 7, 8, 9)

    val result = List.concatLists(l)

    result shouldBe expected
  }

  "addOneToList" should "return same list with elements + 1" in {
    val l1 = List(1, 2, 3)
    val expected = List(2, 3, 4)

    val result = List.addOneToList(l1)

    result shouldBe expected
  }

  "addLists" should "return the sum of the two lists" in {
    val l1 = List(1, 2, 3)
    val l2 = List(4, 5, 6)
    val expected = List(5, 7, 9)

    val result = List.addLists(l1, l2)

    result shouldBe expected
  }
}
