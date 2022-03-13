package com.evolution.bootcamp.assignment.poker

import scala.collection.mutable.ListBuffer

import PokerHand._

object PokerHand {

  def findPair(cardGroups: Map[CardValue.Value, Seq[Card]]): HandValue = {
    val freeCards = ListBuffer[Card]()
    var pairCard1: Card = null
    var pairCard2: Card = null
    cardGroups.foreach { case (cardVal, cards) =>
      if (cards.size == 1) {
        freeCards += cards.head
      } else if (cards.size == 2) {
        pairCard1 = cards.head
        pairCard2 = cards(1)
      }
    }
    new Pair(pairCard1, pairCard2, freeCards.toSeq)
  }

  private def findTwoPairsOrBetterHands(cardGroups: Map[CardValue.Value, Seq[Card]]): HandValue = {
    var kicker: Card = null
    var firstPairCards: Seq[Card] = null
    var secondPairCards: Seq[Card] = null
    var threeOfaKindCards: Seq[Card] = null
    var fourOfaKindCards: Seq[Card] = null
    cardGroups.foreach { case (_, cards) =>
      cards.size match {
        case 1 => kicker = cards.head
        case 2 =>
          if (firstPairCards == null) {
            firstPairCards = cards
          } else {
            secondPairCards = cards
          }
        case 3 => threeOfaKindCards = cards
        case 4 => fourOfaKindCards = cards
      }
    }

    if (threeOfaKindCards != null) {
      if (firstPairCards != null) {
        return new FullHouse(threeOfaKindCards, firstPairCards)
      }
      return new ThreeOfaKind(threeOfaKindCards)
    }

    if (fourOfaKindCards != null) {
      return new FourOfaKind(fourOfaKindCards, kicker)
    }

    if (firstPairCards.head.value.id > secondPairCards.head.value.id) {
      new TwoPairs(firstPairCards, secondPairCards, kicker)
    } else {
      new TwoPairs(secondPairCards, firstPairCards, kicker)
    }

  }

  private def findSequenceType(cards: Seq[Card]): HandValue = {
    (isStraight(cards), isColour(cards)) match {
      case t if t._1 && t._2 => new StraightFlush(cards)
      case t if t._1 => new Straight(cards)
      case t if t._2 => new Colour(cards)
      case _ => new HighCard(cards)
    }
  }

  private def isColour(cards: Seq[Card]) = {
    !cards.exists(c => c.color.id != cards.head.color.id)
  }

  private def isStraight(cards: Seq[Card]) = {
    val sortedCardValues = cards.map(e => e.value.id).sorted
    val straightExpectedValues = Array.range(sortedCardValues.min, sortedCardValues.min + 5).toSeq
    LowStraight.lowStraightCards.intersect(cards.map(e => e.value)).size == 5 || straightExpectedValues == sortedCardValues
  }
}

class PokerHand(val cardsAsStrings: List[String]) extends Ordered[PokerHand] {
  val cards: Seq[Card] = cardsAsStrings.map(c => new Card(c))
  private val cardGroups: Map[CardValue.Value, Seq[Card]] = cards.groupBy(c => c.value)

  val handValue: HandValue = cardGroups.size match {
    case 2 | 3 => findTwoPairsOrBetterHands(cardGroups);
    case 4 => findPair(cardGroups);
    case 5 => findSequenceType(cards)
  }

  override def compare(that: PokerHand): Int = {
    that.handValue.compare(handValue)
  }

  override def hashCode(): Int = {
    handValue.hashCode()
  }

  override def equals(obj: Any): Boolean = handValue.equals(obj.asInstanceOf[PokerHand].handValue)
}
