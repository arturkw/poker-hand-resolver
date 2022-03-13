package com.evolution.bootcamp.assignment.poker

object Colors extends Enumeration {
  type Color = Value
  val Diamond, Heart, Club, Spade = Value
}

object CardValue extends Enumeration {
  type value = Value
  val Deuce, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace = Value
}

object LowStraight {
  val lowStraightCards: Seq[CardValue.Value] = Seq(CardValue.Ace, CardValue.Deuce, CardValue.Three, CardValue.Four, CardValue.Five)
}

case class Card(cardAsString: String) {
  val color: Colors.Value = cardAsString.last match {
    case 's' => Colors.Spade
    case 'd' => Colors.Diamond
    case 'h' => Colors.Heart
    case 'c' => Colors.Club
  }

  val value: CardValue.Value = cardAsString.charAt(0) match {
    case '2' => CardValue.Deuce
    case '3' => CardValue.Three
    case '4' => CardValue.Four
    case '5' => CardValue.Five
    case '6' => CardValue.Six
    case '7' => CardValue.Seven
    case '8' => CardValue.Eight
    case '9' => CardValue.Nine
    case 'T' => CardValue.Ten
    case 'J' => CardValue.Jack
    case 'Q' => CardValue.Queen
    case 'K' => CardValue.King
    case 'A' => CardValue.Ace
  }

  def valueAsString() : Char = {
    value.toString.charAt(0)
  }

  override def hashCode(): Int = value.hashCode()

  override def equals(obj: Any): Boolean = value.id.equals(obj.asInstanceOf[Card].value.id)
}

object Card {
  def card(i: String): Card = {
    new Card(i)
  }
}