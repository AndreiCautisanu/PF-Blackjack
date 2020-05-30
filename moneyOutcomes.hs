module MoneyOutcomes where

import Cards

type Money = Integer

data Outcome = Loss | Draw | Win | WinByBlackJack deriving (Eq, Show)


--functie care determina rezultatul unei runde pentru jucator in functie de valorile mainilor jucatorului si dealerului
--daca jucatorul merge bust in orice fel, jucatorul pierde, daca dealer-ul merge bust in orice fel, jucatorul castiga
--daca jucatorul face blackjack, iar dealer-ul nu, jucatorul castiga prin blackjack (relevant pentru prize money)
--daca ambii au blackjack, jocul este egalitate
--in orice alt caz, comparam valorile mainilor
handsOutcome :: HandVal -> HandVal -> Outcome
handsOutcome _ Bust = Win
handsOutcome Bust _ = Loss
handsOutcome Blackjack Blackjack = Draw
handsOutcome Blackjack _ = WinByBlackJack
handsOutcome playerHandVal bankHandVal
    | playerHandVal > bankHandVal = Win
    | playerHandVal == bankHandVal = Draw
    | playerHandVal < bankHandVal = Loss

--functia primeste balance-ul, pariul si outcome-ul rundei ale jucatorului si determina noul balance
--daca jucatorul pierde, va pierde o suma egala cu suma pariata
--daca jocul este egal, balance-ul jucatorului va ramane la fel
--o victorie obisnuita ii creste balance-ul jucatorului cu o suma egala cu suma pariata
--o victorie prin blackjack ii creste balance-ul jucatorului cu o suma egala cu 150% din suma pariata
moneyOutcome :: Money -> Money -> Outcome -> Money
moneyOutcome money bet Loss = money - bet
moneyOutcome money _ Draw = money
moneyOutcome money bet Win = money + bet
moneyOutcome money bet WinByBlackJack = money + (ceiling $ (1.5 :: Double) * fromIntegral bet)

    
    
    