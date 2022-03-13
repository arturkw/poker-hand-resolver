package com.evolution.bootcamp.assignment.poker

import junit.framework.TestCase
import org.junit.Assert._
import Card._

class HandValueResolverTest extends TestCase {

  def test_high_card(): Unit = {
    val expected = new HighCard(Seq(card("Ah"), card("7c"), card("9d"), card("2h"), card("Ks")))
    val is = new PokerHand(List("7c", "Ks", "2h", "Ah", "9d"))

    assertEquals(expected.highCards.head.value.id, is.handValue.asInstanceOf[HighCard].highCards.head.value.id)
    assertEquals(expected.highCards(1).value.id, is.handValue.asInstanceOf[HighCard].highCards(1).value.id)
    assertEquals(expected.highCards(2).value.id, is.handValue.asInstanceOf[HighCard].highCards(2).value.id)
    assertEquals(expected.highCards(3).value.id, is.handValue.asInstanceOf[HighCard].highCards(3).value.id)
    assertEquals(expected.highCards(4).value.id, is.handValue.asInstanceOf[HighCard].highCards(4).value.id)
  }

  def test_pair(): Unit = {
    val expected = new Pair(card("As"), card("Ah"), Seq(card("5s"), card("6s"), card("7c")))
    val is = new PokerHand(List("As", "5s", "6s", "7c", "Ah")).handValue
    assertEquals(expected.highCard1.value.id, is.asInstanceOf[Pair].highCard1.value.id)
    assertEquals(expected.highCard2.value.id, is.asInstanceOf[Pair].highCard2.value.id)
  }

  def test_two_pair(): Unit = {
    val expected = new TwoPairs(Seq(card("Ah"), card("Ac")), Seq(card("2d"), card("2h")), card("Ks"))
    val is = new PokerHand(List("Ac", "Ks", "2h", "Ah", "2d"))
    assertEquals(expected.highPairCards.head.value, is.handValue.asInstanceOf[TwoPairs].highPairCards.head.value)
    assertEquals(expected.lowPairCards.head.value, is.handValue.asInstanceOf[TwoPairs].lowPairCards.head.value)
    assertEquals(expected.kicker.value, is.handValue.asInstanceOf[TwoPairs].kicker.value)
  }

  def test_three_of_a_kind(): Unit = {
    val expected = new ThreeOfaKind(Seq(card("Ks"), card("Kh"), card("Kd")))
    val is = new PokerHand(List("Kh", "2d", "Ks", "5d", "Kd"))
    assertEquals(expected.threeCard.head.value.id, is.handValue.asInstanceOf[ThreeOfaKind].threeCard.head.value.id)
  }

  def test_straight(): Unit = {
    val expected = new Straight(Seq(card("6h"), card("4h"), card("5s"), card("7d"), card("8c")))
    val is = new PokerHand(List("5s", "7d", "6h", "8c", "4h"))

    assertEquals(expected.straightCards.head.value.id, is.handValue.asInstanceOf[Straight].straightCards.head.value.id)
    assertEquals(expected.straightCards(1).value.id, is.handValue.asInstanceOf[Straight].straightCards(1).value.id)
    assertEquals(expected.straightCards(2).value.id, is.handValue.asInstanceOf[Straight].straightCards(2).value.id)
    assertEquals(expected.straightCards(3).value.id, is.handValue.asInstanceOf[Straight].straightCards(3).value.id)
    assertEquals(expected.straightCards(4).value.id, is.handValue.asInstanceOf[Straight].straightCards(4).value.id)
  }

  def test_top_straight(): Unit = {
    val expected = new Straight(Seq(card("Ah"), card("Qh"), card("Js"), card("Td"), card("Kc")))
    val is = new PokerHand(List("Js", "Td", "Qh", "Kc", "Ah"))

    assertEquals(expected.straightCards.head.value.id, is.handValue.asInstanceOf[Straight].straightCards.head.value.id)
    assertEquals(expected.straightCards(1).value.id, is.handValue.asInstanceOf[Straight].straightCards(1).value.id)
    assertEquals(expected.straightCards(2).value.id, is.handValue.asInstanceOf[Straight].straightCards(2).value.id)
    assertEquals(expected.straightCards(3).value.id, is.handValue.asInstanceOf[Straight].straightCards(3).value.id)
    assertEquals(expected.straightCards(4).value.id, is.handValue.asInstanceOf[Straight].straightCards(4).value.id)
  }

  def test_low_straight(): Unit = {
    val expected = new Straight(Seq(card("Ah"), card("2h"), card("5s"), card("3d"), card("4c")))
    val is = new PokerHand(List("5s", "3d", "2h", "4c", "Ah"))

    assertEquals(expected.straightCards.head.value.id, is.handValue.asInstanceOf[Straight].straightCards.head.value.id)
    assertEquals(expected.straightCards(1).value.id, is.handValue.asInstanceOf[Straight].straightCards(1).value.id)
    assertEquals(expected.straightCards(2).value.id, is.handValue.asInstanceOf[Straight].straightCards(2).value.id)
    assertEquals(expected.straightCards(3).value.id, is.handValue.asInstanceOf[Straight].straightCards(3).value.id)
    assertEquals(expected.straightCards(4).value.id, is.handValue.asInstanceOf[Straight].straightCards(4).value.id)
  }

  def test_colour(): Unit = {
    val expected = new Colour(Seq(card("Th"), card("9h"), card("5h"), card("3h"), card("4h")))
    val is = new PokerHand(List("9h", "Th", "3h", "4h", "5h"))

    assertEquals(expected.cardsInColour.head.color.id, is.handValue.asInstanceOf[Colour].cardsInColour.head.color.id)
    assertEquals(expected.cardsInColour(1).color.id, is.handValue.asInstanceOf[Colour].cardsInColour(1).color.id)
    assertEquals(expected.cardsInColour(2).color.id, is.handValue.asInstanceOf[Colour].cardsInColour(2).color.id)
    assertEquals(expected.cardsInColour(3).color.id, is.handValue.asInstanceOf[Colour].cardsInColour(3).color.id)
    assertEquals(expected.cardsInColour(4).color.id, is.handValue.asInstanceOf[Colour].cardsInColour(4).color.id)
  }

  def test_full_house(): Unit = {
    val expected = new FullHouse(Seq(card("Th"), card("Tc"), card("Td")), Seq(card("9c"), card("9h")))
    val is = new PokerHand(List("9c", "Td", "Tc", "9h", "Th"))

    assertEquals(expected.threeOfaKindCards.head.value.id, is.handValue.asInstanceOf[FullHouse].threeOfaKindCards.head.value.id)
    assertEquals(expected.threeOfaKindCards(1).value.id, is.handValue.asInstanceOf[FullHouse].threeOfaKindCards(1).value.id)
    assertEquals(expected.threeOfaKindCards(2).value.id, is.handValue.asInstanceOf[FullHouse].threeOfaKindCards(2).value.id)
    assertEquals(expected.pairCard.head.value.id, is.handValue.asInstanceOf[FullHouse].pairCard.head.value.id)
    assertEquals(expected.pairCard(1).value.id, is.handValue.asInstanceOf[FullHouse].pairCard(1).value.id)
  }

  def test_four_of_a_kind(): Unit = {
    val expected = new FourOfaKind(Seq(card("Th"), card("Tc"), card("Td"), card("Ts")), card("9h"))
    val is = new PokerHand(List("9c", "Td", "Tc", "Ts", "Th"))

    assertEquals(expected.fourOfoKindCards.head.value.id, is.handValue.asInstanceOf[FourOfaKind].fourOfoKindCards.head.value.id)
    assertEquals(expected.fourOfoKindCards(1).value.id, is.handValue.asInstanceOf[FourOfaKind].fourOfoKindCards(1).value.id)
    assertEquals(expected.fourOfoKindCards(2).value.id, is.handValue.asInstanceOf[FourOfaKind].fourOfoKindCards(2).value.id)
    assertEquals(expected.fourOfoKindCards(3).value.id, is.handValue.asInstanceOf[FourOfaKind].fourOfoKindCards(3).value.id)
    assertEquals(expected.kicker.value.id, is.handValue.asInstanceOf[FourOfaKind].kicker.value.id)
  }

  def test_straight_flush(): Unit = {
    val expected = new StraightFlush(Seq(card("6h"), card("4h"), card("5h"), card("7h"), card("8h")))
    val is = new PokerHand(List("5h", "7h", "6h", "8h", "4h"))

    assertEquals(expected.straightFlushCards.head.value.id, is.handValue.asInstanceOf[StraightFlush].straightFlushCards.head.value.id)
    assertEquals(expected.straightFlushCards(1).value.id, is.handValue.asInstanceOf[StraightFlush].straightFlushCards(1).value.id)
    assertEquals(expected.straightFlushCards(2).value.id, is.handValue.asInstanceOf[StraightFlush].straightFlushCards(2).value.id)
    assertEquals(expected.straightFlushCards(3).value.id, is.handValue.asInstanceOf[StraightFlush].straightFlushCards(3).value.id)
    assertEquals(expected.straightFlushCards(4).value.id, is.handValue.asInstanceOf[StraightFlush].straightFlushCards(4).value.id)
  }

  def test_top_straight_flush(): Unit = {
    val expected = new StraightFlush(Seq(card("Ah"), card("Qh"), card("Jh"), card("Th"), card("Kh")))
    val is = new PokerHand(List("Jh", "Th", "Qh", "Kh", "Ah"))

    assertEquals(expected.straightFlushCards.head.value.id, is.handValue.asInstanceOf[StraightFlush].straightFlushCards.head.value.id)
    assertEquals(expected.straightFlushCards(1).value.id, is.handValue.asInstanceOf[StraightFlush].straightFlushCards(1).value.id)
    assertEquals(expected.straightFlushCards(2).value.id, is.handValue.asInstanceOf[StraightFlush].straightFlushCards(2).value.id)
    assertEquals(expected.straightFlushCards(3).value.id, is.handValue.asInstanceOf[StraightFlush].straightFlushCards(3).value.id)
    assertEquals(expected.straightFlushCards(4).value.id, is.handValue.asInstanceOf[StraightFlush].straightFlushCards(4).value.id)
  }

  def test_low_straight_flush(): Unit = {
    val expected = new StraightFlush(Seq(card("Ah"), card("2h"), card("5h"), card("3h"), card("4h")))
    val is = new PokerHand(List("5h", "3h", "2h", "4h", "Ah"))

    assertEquals(expected.straightFlushCards.head.value.id, is.handValue.asInstanceOf[StraightFlush].straightFlushCards.head.value.id)
    assertEquals(expected.straightFlushCards(1).value.id, is.handValue.asInstanceOf[StraightFlush].straightFlushCards(1).value.id)
    assertEquals(expected.straightFlushCards(2).value.id, is.handValue.asInstanceOf[StraightFlush].straightFlushCards(2).value.id)
    assertEquals(expected.straightFlushCards(3).value.id, is.handValue.asInstanceOf[StraightFlush].straightFlushCards(3).value.id)
    assertEquals(expected.straightFlushCards(4).value.id, is.handValue.asInstanceOf[StraightFlush].straightFlushCards(4).value.id)
  }

  def test_Poker_Hand_equals_method(): Unit = {
    val pk1 = new PokerHand(List("Ah", "Ac", "2d", "2h", "Ks"))
    val pk2 = new PokerHand(List("As", "Ad", "2s", "2c", "Kc"))
    assertTrue(pk1 == pk2)

    val pk3 = new PokerHand(List("Ah", "Ac", "2d", "2h", "Ks"))
    val pk4 = new PokerHand(List("Js", "Ad", "2s", "2c", "Kc"))
    assertTrue(pk3 != pk4)
  }

}
