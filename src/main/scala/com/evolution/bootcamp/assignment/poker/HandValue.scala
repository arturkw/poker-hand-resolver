package com.evolution.bootcamp.assignment.poker

trait HandValue extends Ordered[HandValue] {
  val handRanking: Int
  val cards: Seq[Card]
  val cardSort: (Card, Card) => Boolean = (a: Card, b: Card) => b.value.id < a.value.id


  override def hashCode(): Int = handRanking.hashCode() + cards.sortWith(cardSort).hashCode()


  override def equals(obj: Any): Boolean = {
    val o = obj.asInstanceOf[HandValue]
    cards.equals(o.cards) && handRanking.equals(o.handRanking)
  }

  override def compare(that: HandValue): Int = {
    if (handRanking == that.handRanking) {
      this match {
        case thisCard: HighCard =>
          return rightCardSetHasHigherRank(toCardValues(thisCard.cards), toCardValues(that.cards))
        case thisCard: Pair =>
          val thatCard = that.asInstanceOf[Pair]
          if (thisCard.highCard1.value.id == thatCard.highCard1.value.id) {
            return rightCardSetHasHigherRank(toCardValues(thisCard.freeCards), toCardValues(thatCard.freeCards))
          } else {
            return thatCard.highCard1.value.id - thisCard.highCard1.value.id
          }
        case thisCard: TwoPairs =>
          val thatCard = that.asInstanceOf[TwoPairs]
          if (thisCard.highPairCards.head.value.id == thatCard.highPairCards.head.value.id) {
            if (thisCard.lowPairCards.head.value.id == thatCard.lowPairCards.head.value.id) {
              return thatCard.kicker.value.id - thisCard.kicker.value.id
            } else {
              return thatCard.lowPairCards.head.value.id - thisCard.lowPairCards.head.value.id
            }
          } else {
            return thatCard.highPairCards.head.value.id - thisCard.highPairCards.head.value.id
          }
        case thisCard: ThreeOfaKind =>
          if (thisCard.threeCard.head.value.id < that.asInstanceOf[ThreeOfaKind].threeCard.head.value.id) {
            return 1
          } else {
            return -1
          }
        case thisCard: Straight =>
          if (thisCard.isLowStraight) {
            return 1
          }
          if (that.asInstanceOf[Straight].isLowStraight) {
            return -1
          }
          return rightCardSetHasHigherRank(toCardValues(thisCard.straightCards), toCardValues(that.asInstanceOf[Straight].straightCards))
        case thisCard: Colour =>
          return rightCardSetHasHigherRank(toCardValues(thisCard.cards), toCardValues(that.cards))
        case thisCard: FullHouse =>
          if (thisCard.threeOfaKindCards.head.value.id < that.asInstanceOf[FullHouse].threeOfaKindCards.head.value.id) {
            return 1
          } else {
            return -1
          }
        case thisCard: FourOfaKind =>
          if (thisCard.fourOfoKindCards.head.value.id < that.asInstanceOf[FourOfaKind].fourOfoKindCards.head.value.id) {
            return 1
          } else {
            return -1
          }
        case thisCard: StraightFlush =>
          if (thisCard.isLowStraightFlush) {
            return 1
          }
          if (that.asInstanceOf[StraightFlush].isLowStraightFlush) {
            return -1
          }
          return rightCardSetHasHigherRank(toCardValues(cards), toCardValues(that.cards))
      }
      return 0
    }
    that.handRanking - handRanking
  }

  def toCardValues: Seq[Card] => Seq[Int] = (cards: Seq[Card]) => {
    cards.map(e => e.value.id).sorted
  }

  private def rightCardSetHasHigherRank(lVals: Seq[Int], rVals: Seq[Int]): Int = {
    if (rVals.isEmpty) {
      return 1
    }
    val rMax = rVals.max
    val lMax = lVals.max
    lMax - rMax match {
      case 0 => rightCardSetHasHigherRank(lVals.filter(_ < lMax), rVals.filter(_ < rMax))
      case _ => rMax - lMax
    }
  }

}

class HighCard(var highCards: Seq[Card], val handRanking: Int = 1) extends HandValue {
  highCards = highCards.sortWith((a, b) => a.value.id > b.value.id)

  override val cards: scala.Seq[Card] = highCards.sortWith(cardSort)
}

class Pair(var highCard1: Card, var highCard2: Card, var freeCards: Seq[Card], val handRanking: Int = 2) extends HandValue {

  if (freeCards.size != 3) throw new RuntimeException("Free cards list has wrong size")

  override val cards: scala.Seq[Card] = (freeCards ++ Seq(highCard1, highCard2)).sortWith(cardSort)

}

class TwoPairs(var highPairCards: Seq[Card], var lowPairCards: Seq[Card], var kicker: Card, val handRanking: Int = 3) extends HandValue {
  override val cards: scala.Seq[Card] = (highPairCards ++ lowPairCards ++ Seq(kicker)).sortWith(cardSort)
}

class ThreeOfaKind(val threeCard: Seq[Card], val handRanking: Int = 4) extends HandValue {
  override val cards: scala.Seq[Card] = threeCard
}

class Straight(var straightCards: Seq[Card], val handRanking: Int = 5) extends HandValue {
  straightCards = straightCards.sortWith((a, b) => a.value.id > b.value.id)
  override val cards: scala.Seq[Card] = straightCards.sortWith(cardSort)

  def isLowStraight: Boolean = {
    LowStraight.lowStraightCards.intersect(cards.map(e => e.value)).size == 5
  }
}

class Colour(val cardsInColour: Seq[Card], val handRanking: Int = 6) extends HandValue {
  override val cards: scala.Seq[Card] = cardsInColour.sortWith(cardSort)
}

class FullHouse(val threeOfaKindCards: Seq[Card], val pairCard: Seq[Card], val handRanking: Int = 7) extends HandValue {
  override val cards: scala.Seq[Card] = (threeOfaKindCards ++ pairCard).sortWith(cardSort)
}

class FourOfaKind(val fourOfoKindCards: Seq[Card], val kicker: Card, val handRanking: Int = 8) extends HandValue {
  override val cards: scala.Seq[Card] = (fourOfoKindCards ++ Seq(kicker)).sortWith(cardSort)
}

class StraightFlush(var straightFlushCards: Seq[Card], val handRanking: Int = 9) extends HandValue {
  straightFlushCards = straightFlushCards.sortWith((a, b) => a.value.id < b.value.id)
  override val cards: scala.Seq[Card] = straightFlushCards.sortWith(cardSort)

  def isLowStraightFlush: Boolean = {
    LowStraight.lowStraightCards.intersect(cards.map(e => e.value)).size == 5
  }

}
