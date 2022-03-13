package com.evolution.bootcamp.assignment.poker

import junit.framework.TestCase
import Card.card

import org.junit.Assert._

import scala.collection.mutable.ListBuffer

class HandValueTest extends TestCase {

  def test_high_card(): Unit = {
    val betterHand = new HighCard(Seq(card("Ah"), card("7c"), card("Td"), card("2h"), card("Ks")))
    val weakerHand = new HighCard(Seq(card("Ac"), card("7d"), card("9s"), card("2s"), card("Kh")))

    val s1: Seq[HighCard] = Seq(weakerHand, betterHand).sorted
    val s2: Seq[HighCard] = Seq(betterHand, weakerHand).sorted

    assertTrue(s1.head.cards(2).value.id > weakerHand.cards(2).value.id)
    assertTrue(s2.head.cards(2).value.id > weakerHand.cards(2).value.id)
  }

  def test_pairs(): Unit = {
    val kickers1 = new ListBuffer[Card]
    kickers1.addOne(card("7d"))
    kickers1.addOne(card("Qd"))
    kickers1.addOne(card("Ts"))
    val betterHand = new Pair(card("Ac"), card("Ad"), kickers1.toSeq)

    val kickers2 = new ListBuffer[Card]
    kickers2.addOne(card("7s"))
    kickers2.addOne(card("8s"))
    kickers2.addOne(card("Qs"))
    val weakerHand = new Pair(card("9h"), card("9s"), kickers2.toSeq)

    val s1: Seq[Pair] = Seq(weakerHand, betterHand).sorted
    val s2: Seq[Pair] = Seq(betterHand, weakerHand).sorted

    assertTrue(s1.head.highCard1.value.id > s1(1).highCard1.value.id)
    assertTrue(s2.head.highCard1.value.id > s2(1).highCard1.value.id)

    assertTrue(s1.head.highCard2.value.id > s1(1).highCard2.value.id)
    assertTrue(s2.head.highCard2.value.id > s2(1).highCard2.value.id)
  }

  def test_pair_kicker(): Unit = {
    val kickers1 = new ListBuffer[Card]
    kickers1.addOne(card("7d"))
    kickers1.addOne(card("Qd"))
    kickers1.addOne(card("Ts"))
    val betterHand = new Pair(card("Ac"), card("Ad"), kickers1.toSeq)

    val kickers2 = new ListBuffer[Card]
    kickers2.addOne(card("7s"))
    kickers2.addOne(card("8s"))
    kickers2.addOne(card("Qs"))
    val weakerHand = new Pair(card("Ah"), card("As"), kickers2.toSeq)

    val s1: Seq[Pair] = Seq(weakerHand, betterHand).sorted
    val s2: Seq[Pair] = Seq(betterHand, weakerHand).sorted

    assertTrue(s1.head.highCard1.value.id == s1(1).highCard1.value.id)
    assertTrue(s2.head.highCard1.value.id == s2(1).highCard1.value.id)

    assertTrue(s1.head.highCard2.value.id == s1(1).highCard2.value.id)
    assertTrue(s2.head.highCard2.value.id == s2(1).highCard2.value.id)

    assertTrue(s1.head.freeCards.map(e => e.value.id).max >= s1(1).freeCards.map(e => e.value.id).max)
    assertTrue(s2.head.freeCards.map(e => e.value.id).max >= s2(1).freeCards.map(e => e.value.id).max)
  }

  def test_two_pairs_with_both_pairs_different(): Unit = {
    val betterHand = new TwoPairs(Seq(card("Ks"), card("Kh")), Seq(card("9s"), card("9h")), card("7c"))
    val weakerHand = new TwoPairs(Seq(card("Qd"), card("Qc")), Seq(card("5d"), card("5c")), card("2s"))

    val s1: Seq[TwoPairs] = Seq(weakerHand, betterHand).sorted
    val s2: Seq[TwoPairs] = Seq(betterHand, weakerHand).sorted

    assertTrue(s1.head.highPairCards.head.value.id > weakerHand.highPairCards.head.value.id)
    assertTrue(s2.head.highPairCards.head.value.id > weakerHand.highPairCards.head.value.id)
  }

  def test_two_pairs_with_different_low_pair(): Unit = {
    val betterHand = new TwoPairs(Seq(card("Ks"), card("Kh")), Seq(card("9s"), card("9h")), card("7c"))
    val weakerHand = new TwoPairs(Seq(card("Kd"), card("Kc")), Seq(card("5d"), card("5c")), card("2s"))

    val s1: Seq[TwoPairs] = Seq(weakerHand, betterHand).sorted
    val s2: Seq[TwoPairs] = Seq(betterHand, weakerHand).sorted

    assertTrue(s1.head.lowPairCards.head.value.id > weakerHand.lowPairCards.head.value.id)
    assertTrue(s2.head.lowPairCards.head.value.id > weakerHand.lowPairCards.head.value.id)
  }

  def test_two_pairs_with_different_top_pair(): Unit = {
    val betterHand = new TwoPairs(Seq(card("As"), card("Ah")), Seq(card("5s"), card("5h")), card("7c"))
    val weakerHand = new TwoPairs(Seq(card("Kd"), card("Kc")), Seq(card("5d"), card("5c")), card("2s"))

    val s1: Seq[TwoPairs] = Seq(weakerHand, betterHand).sorted
    val s2: Seq[TwoPairs] = Seq(betterHand, weakerHand).sorted

    assertTrue(s1.head.highPairCards.head.value.id > weakerHand.highPairCards.head.value.id)
    assertTrue(s2.head.highPairCards.head.value.id > weakerHand.highPairCards.head.value.id)
  }

  def test_two_pairs_equal_with_different_kicker(): Unit = {
    val betterHand = new TwoPairs(Seq(card("As"), card("Ah")), Seq(card("5s"), card("5h")), card("7c"))
    val weakerHand = new TwoPairs(Seq(card("Ad"), card("Ac")), Seq(card("5d"), card("5c")), card("2s"))

    val s1: Seq[TwoPairs] = Seq(weakerHand, betterHand).sorted
    val s2: Seq[TwoPairs] = Seq(betterHand, weakerHand).sorted

    assertTrue(s1.head.kicker.value.id > weakerHand.kicker.value.id)
    assertTrue(s2.head.kicker.value.id > weakerHand.kicker.value.id)
  }

  def test_three_of_a_kind_order(): Unit = {
    val betterHand = new ThreeOfaKind(Seq(card("Ah"), card("Ac"), card("As")))
    val weakerHand = new ThreeOfaKind(Seq(card("Kh"), card("Kc"), card("Ks")))

    val s1 = Seq(betterHand, weakerHand).sorted
    val s2 = Seq(weakerHand, betterHand).sorted

    assertTrue(s1.head.threeCard.head.value.id > s1(1).threeCard.head.value.id)
    assertTrue(s2.head.threeCard.head.value.id > s2(1).threeCard.head.value.id)
  }

  def test_straight_rank(): Unit = {
    val betterHand = new Straight(Seq(card("8h"), card("4c"), card("5c"), card("6c"), card("7c")))
    val weakerHand = new Straight(Seq(card("3h"), card("4c"), card("5c"), card("6c"), card("7c")))

    val s1 = Seq(weakerHand, betterHand).sorted
    val s2 = Seq(betterHand, weakerHand).sorted

    assertTrue(s1.head.straightCards.head.value.id > weakerHand.straightCards.head.value.id)
    assertTrue(s2.head.straightCards.head.value.id > weakerHand.straightCards.head.value.id)
  }

  def test_low_straight_rank(): Unit = {
    val betterHand = new Straight(Seq(card("8h"), card("4c"), card("5c"), card("6c"), card("7c")))
    val weakerHand = new Straight(Seq(card("Ah"), card("4c"), card("5c"), card("2c"), card("3c")))


    val s1 = Seq(weakerHand, betterHand).sorted
    val s2 = Seq(betterHand, weakerHand).sorted

    assertTrue(s1.head.straightCards.head.value.id == 6)
    assertTrue(s2.head.straightCards.head.value.id == 6)
  }

  def test_high_straight_rank(): Unit = {
    val betterHand = new Straight(Seq(card("Ah"), card("Qc"), card("Tc"), card("Kc"), card("Jc")))
    val weakerHand = new Straight(Seq(card("8h"), card("4c"), card("5c"), card("6c"), card("7c")))

    val s1 = Seq(betterHand, weakerHand).sorted
    val s2 = Seq(weakerHand, betterHand).sorted

    assertTrue(s1.head.straightCards.head.value.id == 12)
    assertTrue(s2.head.straightCards.head.value.id == 12)
  }

  def test_colour_rank(): Unit = {
    val betterHand = new Colour(Seq(card("Jh"), card("9h"), card("5h"), card("3h"), card("4h")))
    val weakerHand = new Colour(Seq(card("Tc"), card("9c"), card("5c"), card("3c"), card("4c")))

    val s1 = Seq(betterHand, weakerHand).sorted
    val s2 = Seq(weakerHand, betterHand).sorted

    assertTrue(s1.head.cardsInColour.head.value.id > weakerHand.cardsInColour.head.value.id)
    assertTrue(s2.head.cardsInColour.head.value.id > weakerHand.cardsInColour.head.value.id)
  }

  def test_full_house_rank(): Unit = {
    val betterHand = new FullHouse(Seq(card("Th"), card("Tc"), card("Td")), Seq(card("9c"), card("9h")))
    val weakerHand = new FullHouse(Seq(card("8h"), card("8c"), card("8d")), Seq(card("3c"), card("3h")))

    val s1 = Seq(betterHand, weakerHand).sorted
    val s2 = Seq(weakerHand, betterHand).sorted

    assertTrue(s1.head.threeOfaKindCards.head.value.id > weakerHand.threeOfaKindCards.head.value.id)
    assertTrue(s2.head.threeOfaKindCards.head.value.id > weakerHand.threeOfaKindCards.head.value.id)
  }

  def test_four_of_a_kind(): Unit = {
    val betterHand = new FourOfaKind(Seq(card("Th"), card("Tc"), card("Td"), card("Ts")), card("9h"))
    val weakerHand = new FourOfaKind(Seq(card("8h"), card("8c"), card("8d"), card("8s")), card("7h"))

    val s1 = Seq(betterHand, weakerHand).sorted
    val s2 = Seq(weakerHand, betterHand).sorted
    assertTrue(s1.head.fourOfoKindCards.head.value.id > weakerHand.fourOfoKindCards.head.value.id)
    assertTrue(s2.head.fourOfoKindCards.head.value.id > weakerHand.fourOfoKindCards.head.value.id)
  }

  def test_straight_flush_rank(): Unit = {
    val betterHand = new StraightFlush(Seq(card("8h"), card("4h"), card("5h"), card("6h"), card("7h")))
    val weakerHand = new StraightFlush(Seq(card("3c"), card("4c"), card("5c"), card("6c"), card("7c")))

    val s1 = Seq(weakerHand, betterHand).sorted
    val s2 = Seq(betterHand, weakerHand).sorted

    assertTrue(s1.head.straightFlushCards.head.value.id > weakerHand.straightFlushCards.head.value.id)
    assertTrue(s2.head.straightFlushCards.head.value.id > weakerHand.straightFlushCards.head.value.id)
  }

  def test_low_straight_flush_rank(): Unit = {
    val betterHand = new StraightFlush(Seq(card("8c"), card("4c"), card("5c"), card("6c"), card("7c")))
    val weakerHand = new StraightFlush(Seq(card("Ah"), card("4h"), card("5h"), card("2h"), card("3h")))


    val s1 = Seq(weakerHand, betterHand).sorted
    val s2 = Seq(betterHand, weakerHand).sorted

    assertTrue(s1.head.cards.head.value.id == 6)
    assertTrue(s2.head.cards.head.value.id == 6)
  }

  def test_high_straight_flush_rank(): Unit = {
    val betterHand = new StraightFlush(Seq(card("Ac"), card("Qc"), card("Tc"), card("Kc"), card("Jc")))
    val weakerHand = new StraightFlush(Seq(card("8c"), card("4c"), card("5c"), card("6c"), card("7c")))

    val s1 = Seq(betterHand, weakerHand).sorted
    val s2 = Seq(weakerHand, betterHand).sorted

    assertTrue(s1.head.cards.head.value.id == 12)
    assertTrue(s2.head.cards.head.value.id == 12)
  }

  def test_poker_hand_rank(): Unit = {
    val hands = Seq(
      new Pair(card("As"), card("Ah"), Seq(card("5s"), card("6s"), card("7c"))),
      new ThreeOfaKind(Seq(card("Ks"), card("Kh"), card("Kd"))),
      new Straight(Seq(card("6h"), card("4h"), card("5s"), card("7d"), card("8c"))),
      new FourOfaKind(Seq(card("Th"), card("Tc"), card("Td"), card("Ts")), card("9h")),
      new FullHouse(Seq(card("Th"), card("Tc"), card("Td")), Seq(card("9c"), card("9h"))),
      new StraightFlush(Seq(card("6h"), card("4h"), card("5h"), card("7h"), card("8h"))),
      new HighCard(Seq(card("Ah"), card("7c"), card("Td"), card("2h"), card("Ks"))),
      new Colour(Seq(card("Th"), card("9h"), card("5h"), card("3h"), card("4h"))),
      new TwoPairs(Seq(card("Ah"), card("Ac")), Seq(card("2d"), card("2h")), card("Ks")))
    assertEquals(Seq(9, 8, 7, 6, 5, 4, 3, 2, 1), hands.sorted.map(e => e.handRanking))
  }

}
