package ex1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.immutable.Set

import ex1.Parsers.*

class ParsersScalaTest extends AnyFlatSpec with Matchers:

    "A basic Parser" should "parse correctly" in {
      def parser = new BasicParser(Set('a', 'b', 'c'))

      parser.parseAll("aabc".toList) should be(true)
      parser.parseAll("aabcdc".toList) should be(false)
      parser.parseAll("".toList) should be(true)
    }

    "A not empty Parser" should "parse correctly" in {
      def parser = new NonEmptyParser(Set('0', '1'))

      parser.parseAll("0101".toList) should be(true)
      parser.parseAll("0123".toList) should be(false)
      parser.parseAll(List()) should be(false)
    }

    "A not two consecutive Parser" should "parse correctly" in {
      def parser = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))

      parser.parseAll("XYZ".toList) should be(true)
      parser.parseAll("XYYZ".toList) should be(false)
      parser.parseAll("".toList) should be(true)
    }

    "A not empty and two consecutive Parser" should "parse correctly" in {
      def parser = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]

      parser.parseAll("XYZ".toList) should be(true)
      parser.parseAll("XYYZ".toList) should be(false)
      parser.parseAll("".toList) should be(false)
    }

    "A string Parser" should "parse correctly" in {
      def parser = "abc".charParser()

      parser.parseAll("aabc".toList) should be(true)
      parser.parseAll("aabcdc".toList) should be(false)
      parser.parseAll("".toList) should be(true)
    }
