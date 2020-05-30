module Blackjack where

import Cards
import MoneyOutcomes
import System.Random
import System.Random.Shuffle
import System.IO.Unsafe
import Math.Combinat.Permutations
import Data.Map
import Control.Concurrent

import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef


--"variabie" pentru testare
card1 :: Card
card1 = Card Ace Spades

card2 :: Card
card2 = Card Queen Hearts

card3 :: Card
card3 = Card (NumberCard 4) Diamonds

card4 :: Card
card4 = Card (NumberCard 7) Hearts

hand1 :: Hand
hand1 = [card1, card2]

hand2 :: Hand
hand2 = [card2, card3, card4]



--functia produce o lista cu toate cartile de o anumita suita
suitCards :: Suit -> [Card]
suitCards s = [Card Ace s] ++ [Card King s] ++ [Card Queen s] ++ [Card Jack s] ++ [Card (NumberCard 10) s] ++ [Card (NumberCard 9) s] ++ [Card (NumberCard 8) s] ++ [Card (NumberCard 7) s] ++ [Card (NumberCard 6) s] ++ [Card (NumberCard 5) s] ++ [Card (NumberCard 4) s] ++ [Card (NumberCard 3) s] ++ [Card (NumberCard 2) s]

--fullDeck concateneaza listele obtinute apeland suitCards pentru fiecare suita
fullDeck :: Deck
fullDeck = (suitCards Hearts) ++ (suitCards Spades) ++ (suitCards Diamonds) ++ (suitCards Clubs)

plsshuffle :: [a] -> IO [a]
plsshuffle xs = do
    let n = length xs
    perm <- getStdRandom (randomPermutation n)
    return $ permuteList perm xs


--ia o carte de pe top-ul pachetului si o pune in mana data
draw :: Deck -> Hand -> (Deck, Hand)
draw (card:deck) hand = (deck, card:hand)


--primeste mana curenta a dealer-ului si aplica algoritmul de joc al dealerilor de blackjack din marea majoritate a cazino-urilor si returneaza mana obtinuta:
--daca valoarea mainii dealer-ului este 16 sau mai putin, el va trebui sa traga o carte, daca are 17 sau mai mult va trebui sa se opreasca
--daca dealer-ul are un as, care, numarat ca 11 aduce valoarea mainii la cel putin 17 (dar pana in 21), va trebui sa se opreasca. 
--cum programul nostru calculeaza asii ca fiind 11 by default pana valoarea mainii trece de 22, conditia va fi mentinuta
playDealerHand :: Deck -> Hand -> Hand
playDealerHand deck dealerHand | handValue dealerHand <= 16 = playDealerHand deck' dealerHand' where (deck', dealerHand') = draw deck dealerHand
playDealerHand deck dealerHand = dealerHand


--functia main a jocului. arata balance-ul curent al jucatorului si cere un pariu mai mic decat balance-ul
--cand pariul este plasat, pachetul se amesteca, se impart cate 2 carti la jucator si la dealer (jucatorul stiind una dintre cartile din mana dealer-ului
--si se apeleaza functia care se ocupa de gameplay-ul efectiv din runda. functia roundLoop va apela gameLoop cu noul balance al jucatorului dupa fiecare runda
--dat ca parametru
gameLoop :: Money -> IO ()
gameLoop money = do
    putStrLn $ "Your balance is " ++ show money
    
    putStrLn "Place your bet: "
    betstring <- getLine
    let bet = (read betstring :: Integer)
    if bet > money then do
        putStrLn "You cannot bet more money than you already have"
        gameLoop money
    else do
        shuffledDeck <- plsshuffle fullDeck
        playerHand <- emptyHand
        dealerHand <- emptyHand
        let (shuffledDeck', playerHand') = (draw shuffledDeck playerHand)
        let (shuffledDeck2', playerHand2') = (draw shuffledDeck' playerHand')
        
        
        let (shuffledDeck3', dealerHand') = (draw shuffledDeck2' dealerHand)
        putStrLn $ "Dealer's face up card is: " ++ show dealerHand'
        let (shuffledDeck4', dealerHand2') = (draw shuffledDeck3' dealerHand')
        --roundLoop money bet shuffledDeck2' [(Card Ace Hearts), (Card King Spades)] dealerHand2'
        roundLoop money bet shuffledDeck4' playerHand2' dealerHand2'
    
exit :: IO ()
exit = putStrLn "exiting the game..."


--functia care se ocupa de gameplay-ul din timpul rundei, primeste ca parametri balance-ul jucatorului, pariul, pachetul si cele doua maini
--prima conditie de iesire din loop verifica daca jucatorul a trecut peste 21, caz in care se apeleaza functia din moneyOutcomes care determina
--noul balance al jucatorului direct cu outcome-ul de loss

--daca mana jucatorului este blackjack(A + 10 sau face card), se va opri automat si se va determina mana dealer-ului
--outcome-ul (WinByBlackjack sau Draw va fi determinat prin functia handsOutcome si se va apela gameLoop in acelasi fel, calculand noul balance cu functia
--moneyOutcome in functie de rezultatul efectiv al rundei

--daca mana jucatorului nu este Bust si nu este Blackjack, acesta va avea optiunea sa mai traga o carte
--daca o face, va primi o carte prin functia draw si se va apela recursiv functia
--daca se opreste, se va repeta procesul descris la pasul anterior
roundLoop :: Money -> Money -> Deck -> Hand -> Hand -> IO ()
roundLoop money bet deck playerHand dealerHand = do
    putStrLn $ "Your current hand: " ++ show playerHand
    putStrLn $ "Hand value: " ++ show (handValue playerHand)
    
    if handVal playerHand == Bust 
    then do
        putStrLn "You went bust! You have lost this round"
        threadDelay 2000000
        gameLoop (moneyOutcome money bet Loss)
    else do
        if handVal playerHand == Blackjack
        then do
            putStrLn "You have blackjack!"
            threadDelay 2000000
            let dealerHand' = playDealerHand deck dealerHand
            putStrLn $ "Dealer has: " ++ show dealerHand'
            putStrLn $ "Dealer hand value: " ++ show (handValue dealerHand')
            threadDelay 2000000
            let dealerHandVal = handVal dealerHand'
            let outcome = (handsOutcome Blackjack dealerHandVal)
            putStrLn $ show outcome
            threadDelay 2000000
            gameLoop (moneyOutcome money bet outcome)
        else do
            putStrLn "Draw card? [y/n]"
            answer <- getLine
            if Prelude.null answer || not (answer == "n") then do
                let (deck', playerHand') = draw deck playerHand
                roundLoop money bet deck' playerHand' dealerHand
            else do
                let dealerHand' = playDealerHand deck dealerHand
                putStrLn $ "Dealer has: " ++ show dealerHand'
                putStrLn $ "Dealer hand value: " ++ show (handValue dealerHand')
                threadDelay 2000000
                let dealerHandVal = handVal dealerHand'
                let outcome = (handsOutcome (handVal playerHand) dealerHandVal)
                putStrLn $ show outcome
                threadDelay 2000000
                gameLoop (moneyOutcome money bet outcome)
            
        
    
