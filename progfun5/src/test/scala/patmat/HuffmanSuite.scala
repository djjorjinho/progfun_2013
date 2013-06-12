package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
      assert(weight(t2) === 9)
    }
  }

  test("times") {
    new TestTrees {
      val l1 = List('a','a','b','c','a')
      val l2 = times(l1)
      assert(l2.head._1 === 'a')
      assert(l2.head._2 === 3)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t1) === List('a','b'))
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      val encoded = encode(t2)("ab".toList)
      assert(decode(t2, encoded) === "ab".toList)
    }
  }

  test("until") {
    new TestTrees {
      val l1 = List(t1, t2)
      val l2 = until(singleton, combine)(l1)
      assert(weight(l2.head) === 14)
    }
  }

  test("createCodeTree") {
    new TestTrees {
      val l1 = List('a','a','b','c','a','d','b')
      val ct = createCodeTree(l1)
      assert(ct.asInstanceOf[Fork].weight === 7)
    }
  }

  test("decoded secret") {
    new TestTrees {
      val ds = decodedSecret
      assert(ds === string2Chars("huffmanestcool"))
    }
  }

  test("convert code tree") {
    new TestTrees {
      val ct1 = convert(t2)
      val ct2 = convert2(t2)
      assert(ct2.length === 3)
    }
  }

}
