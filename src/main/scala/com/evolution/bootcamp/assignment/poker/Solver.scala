package com.evolution.bootcamp.assignment.poker


object Solver {

  def best5CardDrawHand(handsAsString: List[String]): PokerHand = {
    to5CardDrawHand(handsAsString).max
  }

  def find5CardDrawHands(handsAsString: List[String]): String = {
    SummaryProducer.fiveCardDrawSummary(to5CardDrawHand(handsAsString))
  }

  private def to5CardDrawHand(handsAsString: List[String]): List[PokerHand] = {
    handsAsString.map(h => h.grouped(2).toList).map(l => new PokerHand(l))
  }

  def findTexasHoldemHands(board: String, startingHands: List[String]): String = {
    val allPossibleHands = for (startingHand <- startingHands) yield (startingHand,
      (startingHand + board).grouped(2).map(e => e.mkString("")).toSeq.combinations(5).toSeq.map(e => e.mkString(""))
    )
    val bestHandForStartingHand = allPossibleHands.map(e => (e._1, e._2.map(q => new PokerHand(q.grouped(2).toList))))
      .map(e => (e._1, e._2.max)).map(e => (e._2, e._1)).sorted
    val boardHand = new PokerHand(board.grouped(2).toList)
    SummaryProducer.texasHoldemSummary(bestHandForStartingHand, boardHand)
  }

  def findOmahaHands(board: String, hands: List[String]): String = {
    val possibleBoardCards = board.grouped(2).toSeq.combinations(3).map(e => e.mkString("")).toSeq
    val startingHandsMap = hands.map(hand => (hand.grouped(2).toSeq.combinations(2).toSeq.map(e => e.mkString("")), hand))

    val allHandValues = for (boardCards <- possibleBoardCards; startingHand <- startingHandsMap)
      yield (startingHand._2, for (q <- startingHand._1) yield q + boardCards)

    val value = allHandValues.map(e => (best5CardDrawHand(e._2.toList), e._1))
      .groupBy(e => e._2).map(e => (e._1, e._2.max))
      .values.toSeq.sortWith((a, b) => a._1 < b._1)

    SummaryProducer.omahaSummary(value)
  }

  def process(line: String): String = {
    val ErrorPrefix = "Error: "

    line.split("\\s+").toList match {
      case "texas-holdem" :: board :: hands => findTexasHoldemHands(board, hands)
      case "omaha-holdem" :: board :: hands => findOmahaHands(board, hands)
      case "five-card-draw" :: hands => find5CardDrawHands(hands)
      case x :: _ => ErrorPrefix + "Unrecognized game type"
      case _ => ErrorPrefix + "Invalid input"
    }
  }
}
