module Cards where

import System.Random

data Suit = Hearts | Diamonds | Spades | Clubs deriving (Show, Eq)

data Rank = NumberCard Int | Jack | Queen | King | Ace deriving (Show, Eq)

data Card = Card Rank Suit deriving (Show, Eq)

--datatype pentru a face diferenta intre tipurile diferite de maini (e.g. un 21 format din Ace si 10 sau face card este blackjack, un 21 format din
--8, 10 si 3 nu este)
data HandVal = Value Int | Blackjack | Bust deriving (Eq, Ord, Show)

type Hand = [Card]
type Deck = [Card]


getCardSuit :: Card -> Suit
getCardSuit (Card _ s) = s

getCardRank :: Card -> Rank
getCardRank (Card r _) = r

--functie care determina recursiv cati asi sunt in mana data (folosita pentru calculul valorii mainii)
acesInHand :: Hand -> Int
acesInHand [] = 0
acesInHand (card:hand) = if getCardRank card == Ace then 1 + acesInHand hand else acesInHand hand

--functie care determina daca mana data e blackjack (as + 10 sau face card)
blackjackInHand :: Hand -> Bool
blackjackInHand [(Card rank1 suit1), (Card rank2 suit2)] = ((rank1 == Ace) && rank2 `elem` [NumberCard 10, Jack, Queen, King]) || ((rank2 == Ace) && rank1 `elem` [NumberCard 10, Jack, Queen, King])
blackjackInHand _ = False


--functie elementara care returneaza valoarea unei carti date, folosind Asul ca valoare 11 by default, valoarea 1 fiind luata in considerare doar in
--functia handValue
cardValue :: Card -> Int
cardValue (Card (NumberCard n) s) = n
cardValue (Card Ace s) = 11
cardValue _ = 10

--functie care calculeaza recursiv valoarea mainii folosindu-se de functia de mai sus, cu asii de valoare 11
rawHandValue :: Hand -> Int
rawHandValue [] = 0
rawHandValue (card:hand) = (cardValue card) + (rawHandValue hand)


--daca valoarea trece de 22 sau peste, vom considera asii existenti ca valoare 1. pentru aceasta, scadem 10 din valoarea mainii pentru fiecare as
--(transformand un as din 11 in 1, valoarea mainii se va reduce cu 10)
handValue :: Hand -> Int
handValue hand = if rawHandValue hand > 21 then rawHandValue hand - (10 * acesInHand hand) else rawHandValue hand

--functia care converteste valoarea mainii in tipurile diferite de outcome ale unei maini (bust, blackjack sau mana normala cu o valoare <= 21)
handVal :: Hand -> HandVal
handVal hand
    | (handValue hand > 21) = Bust
    | blackjackInHand hand = Blackjack
    | otherwise = Value (handValue hand)

--functie care returneaza un IO Hand gol pentru "initializari"
emptyHand :: IO Hand
emptyHand = return []



