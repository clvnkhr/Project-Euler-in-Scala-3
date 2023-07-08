package projecteuler
package pokercards

enum Suit:
  case Diamond, Club, Heart, Spade
  override def toString: String = this match
    case Diamond => "♦"
    case Club    => "♣️"
    case Heart   => "♥️"
    case Spade   => "♠️"

type CardVal = Int
val (jack, queen, king, ace): (CardVal, CardVal, CardVal, CardVal) =
  (11, 12, 13, 14)

extension (v: CardVal)
  def pretty: String = v match
    case v if v == jack  => "J"
    case v if v == queen => "Q"
    case v if v == king  => "K"
    case v if v == ace   => "A"
    case _               => v.toString

case class Card(cardVal: CardVal, suit: Suit):
  override def toString: String =
    s"[${cardVal.pretty}$suit]"

given ordSuit: Ordering[Suit] with
  def compare(s1: Suit, s2: Suit): Int = s1.ordinal.compare(s2.ordinal)

import Suit._
import math.Ordered.orderingToOrdered

// assert(Diamond < Heart)

given ordCard: Ordering[Card] with
  def compare(c1: Card, c2: Card): Int = (c1, c2) match
    case (Card(v1, s1), Card(v2, s2)) =>
      if v1 != v2 then v1.compare(v2) else ordSuit.compare(s1, s2)

// assert(Card(jack, Diamond) > Card(5, Spade))

enum HandType:
  case HighCard, OnePair, TwoPairs, ThreeOfAKind, Straight, Flush,
    FullHouse, FourOfAKind, StraightFlush
import HandType._

// tiebreaks holds the cards needed to distinguish which hand is bigger,
// We assume that a single deck of 52 cards is used,
// so a card cannot appear twice
case class Hand(handType: HandType, tieBreaker: Card)

given ordHand: Ordering[Hand] with
  def compare(h1: Hand, h2: Hand): Int = (h1, h2) match
    case (Hand(type1, tie1), Hand(type2, tie2)) =>
      if type1 != type2 then type1.ordinal.compare(type2.ordinal)
      else ordCard.compare(tie1, tie2)

// assert(Hand(FullHouse, Card(6, Club)) > Hand(FullHouse, Card(4, Club)))
// assert(
// Hand(FourOfAKind, Card(4, Diamond)) > Hand(HighCard, Card(3, Diamond))
// )

extension (cards: Array[Card])
  def toHand: Hand =
    if cards.length != 5 then throw Exception("not 5 cards")
    val sortedCards = cards.sorted
    val values = sortedCards.map(_.cardVal)
    val suits = sortedCards.map(_.suit)
    require(values.forall(v => v >= 2 && v <= ace))

    val highCard = sortedCards.last
    if values.zipWithIndex.forall((v, i) => v == sortedCards(0).cardVal + i)
    then
      if suits.distinct.length == 1 then Hand(StraightFlush, highCard)
      else Hand(Straight, highCard)
    else if suits.distinct.length == 1 then Hand(Flush, highCard)
    else {
      val valOccs =
        values.distinct.map(v => (v, values.count(_ == v))).sortBy(_._2)
      val mostVal = valOccs.last._1
      val mostOccs = valOccs.last._2
      val bestMultiCard = sortedCards.filter(_.cardVal == mostVal).last

      if valOccs.length == 2 then
        if mostOccs == 4 then Hand(FourOfAKind, bestMultiCard)
        else Hand(FullHouse, bestMultiCard)
      else if mostOccs == 3 then Hand(ThreeOfAKind, bestMultiCard)
      else if mostOccs == 2 then
        if valOccs.dropRight(1).last._2 == 2 then Hand(TwoPairs, bestMultiCard)
        else Hand(OnePair, bestMultiCard)
      else Hand(HighCard, highCard)
    }

// input processing
extension (c: Char)
  def toSuit: Suit = c match
    case 'D' => Diamond
    case 'C' => Club
    case 'H' => Heart
    case 'S' => Spade
    case _   => throw Exception(s"$c is not a valid suit")

  def toValue: CardVal = c match
    case x if x.toInt >= '2'.toInt && x.toInt <= '9'.toInt =>
      x.toInt - '0'.toInt
    case x if x == 'T' => 10
    case x if x == 'J' => jack
    case x if x == 'Q' => queen
    case x if x == 'K' => king
    case x if x == 'A' => ace
    case _             => throw Exception(s"$c is not a valid card")

extension (str: String)
  def toCard: Card = str match
    case str if str.length == 2 =>
      Card(str(0).toValue, str(1).toSuit)
    case _ => throw new Exception(s"$str is not a pair of chars")
