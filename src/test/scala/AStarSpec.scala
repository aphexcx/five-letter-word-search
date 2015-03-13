import org.scalatest.{FunSpec, Matchers}
import wordsearch.Immutable._


class AStarSpec extends FunSpec with Matchers {
  describe("distance") {
    describe("calculates the Hamming Distance of two equal length strings") {
      it("with one difference") {
        distance("hello", "helpo") shouldBe 1
      }

      it("with more than one difference") {
        distance("hello", "burpo") shouldBe 4
      }

      it("with no differences") {
        distance("hello", "hello") shouldBe 0
      }
    }
  }

  describe("oneLetterOff") {
    describe("generates all words that are one letter away from a given word") {
      it("in the trivial single letter case") {
        oneLetterOff("h") should contain theSameElementsAs ('a' to 'z' map (_.toString) filterNot (_ == "h"))
      }

      it("in a longer word case") {
        oneLetterOff("smart") should contain allOf("amart", "bmart", "zmart",
                                                            "smbrt", "smzrt",
                                                   "smara", "smarb", "smarz")

        oneLetterOff("smart") should not contain "smart"
      }
    }
  }

  describe("neighbors") {
    describe("finds all valid words that are one letter off") {
      it("for valid words") {
        neighbors("brain") should contain only("drain", "braid", "brawn", "grain", "train", "bruin")
      }

      it("for invalid words, finds nothing") {
        neighbors("blblb") shouldBe empty
      }
    }
  }

  describe("reconstructPath") {
    describe("builds an ordered list of words") {
      it("for a given word with significant ancestry") {
        val parents = Map[Word, Word]("brain" -> "braid",
          "braid" -> "brand",
          "brand" -> "bland",
          "bland" -> "blank",
          "blank" -> "black")
        reconstructPath("brain", parents) should contain inOrderOnly("brain", "braid", "brand", "bland", "blank", "black")
      }

      it("for a given word and a single parent") {
        val parents = Map[Word, Word]("brain" -> "braid")
        reconstructPath("brain", parents) should contain inOrderOnly("brain", "braid")
      }
    }
  }

  describe("AStar") {
    describe("g(x)") {
      pending
    }
    describe("h(x)") {
      pending
    }
    describe("f(x)") {
      pending
    }

    describe("search") {
      pending
      describe("finds the best path through valid words from a start word to a goal word") {
        it("given a start and goal that are the same") {
          AStar("smart", "smart").search should contain only "smart"
        }
        it("given a start and goal that are a single letter apart") {
          AStar("smart", "start").search should contain inOrderOnly("smart", "start")
        }
        it("given a start and goal that are a significant distance apart") {
          AStar("smart", "brain").search should contain only("smart", "start", "stark", "stack", "slack", "black", "blank", "bland", "brand", "braid", "brain")
        }
      }
    }
  }
}