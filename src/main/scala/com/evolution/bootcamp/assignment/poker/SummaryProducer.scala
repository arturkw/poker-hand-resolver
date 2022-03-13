package com.evolution.bootcamp.assignment.poker

object SummaryProducer {

  def omahaSummary(hands: Seq[(PokerHand, String)]): String = {
    val groupBy = hands.groupBy(e => e._1.handValue)
    findMessage("", hands.map(e => e._1), groupBy, null)
  }

  private def findMessage(message: String, bestPlayerHands: Seq[PokerHand], handsByGroups: Map[HandValue, Seq[(PokerHand, String)]], boardHand: PokerHand): String = {
    if (bestPlayerHands.nonEmpty) {
      val currentHand = bestPlayerHands.head
      val groupHands: Seq[PokerHand] = handsByGroups(currentHand.handValue).map(e => e._1)
      val (isBoardTypeHand, groupHandSize) = (if (boardHand != null) boardHand.handValue == currentHand.handValue else false, groupHands.size)
      val handSummaryMessagePart = (isBoardTypeHand, groupHandSize) match {
        case (true, size) => Iterator.fill(size)(boardHand.cards.sortWith((a, b) => a.value.id > b.value.id).take(2).map(e => e.cardAsString).mkString("")).mkString("=")
        case (false, _) => Iterator.fill(1)(handsByGroups(currentHand.handValue).map(e => e._2).sorted.mkString("=")).mkString("=")
      }
      val separator = if (message.isEmpty) "" else " "
      return findMessage(message + separator + handSummaryMessagePart, bestPlayerHands.filter(e => !groupHands.contains(e)), handsByGroups, boardHand)
    }
    message
  }

  def texasHoldemSummary(hands: Seq[(PokerHand, String)], boardHand: PokerHand): String = {
    val groupBy = hands.groupBy(e => e._1.handValue)
    findMessage("", hands.map(e => e._1), groupBy, boardHand)
  }

  def fiveCardDrawSummary(hands: List[PokerHand]): String = {
    val handGroups = hands.groupBy(h => h.handValue).map(e => (e._1, e._2.map(r => (r, r.cardsAsStrings.mkString("")))))
    findMessage("", hands.sorted, handGroups, null)
  }

}