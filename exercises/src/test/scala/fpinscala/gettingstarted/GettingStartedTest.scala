package fpinscala.gettingstarted

import fpinscala.gettingstarted.PolymorphicFunctions.isSorted
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GettingStartedTest extends AnyFlatSpec with Matchers {

  "isSorted[Int]" should "return true for ordered array" in {
    // GIVEN
    val arr = Array(1, 4, 5, 7, 9)
    val bt = (a: Int, b: Int) => a > b

    // WHEN
    val result = isSorted(arr, bt)

    // THEN
    result should be(true)
  }

  it should "return false for unordered array" in {
    // GIVEN
    val arr = Array(5, 4, 5, 7, 9)
    val bt = (a: Int, b: Int) => a > b

    // WHEN
    val result = isSorted(arr, bt)

    // THEN
    result should be(false)
  }

}
