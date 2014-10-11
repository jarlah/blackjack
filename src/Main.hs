{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
--
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Monad (unless)
import Test.QuickCheck.All (quickCheckAll)
import System.Random
import System.Exit
 
type Deck = [Card]

data Card = Card {cSuit :: CSuit, cValue :: CValue} deriving Show

data CSuit = Spades | Hearts | Diamonds | Clubs deriving (Show, Enum, Eq, Bounded)

data CValue = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Enum, Eq, Bounded)

type Hand = [Card]

defaultDeck :: Deck
defaultDeck = [Card suit value | suit <- [(minBound :: CSuit) ..], value <- [(minBound :: CValue) ..]]

dealHand :: Deck -> IO undefined
dealHand inDeck = do
  -- make random generator
   rng <- newStdGen

   -- shufflee
   let deck = shuffleDeck rng inDeck
 
   -- dealers hand
   -- get first card
   let card1 = head deck 
   let deck1 = tail deck -- new deck
   -- get second card
   let card2 = head deck1
   let deck2 = tail deck1 -- new deck
   -- and the dealers hand is
   let dealers_hand = [card1, card2]
 
   print ("Dealers faced up card is: " ++ show (cValue card1))
   
   -- get first cards
   let card3 = head deck2
   let deck3 = tail deck2 -- new deck
   -- get second card
   let card4 = head deck3
   let deck4 = tail deck3 -- new deck
   let base = [card3, card4]

   -- Enter draw loop
   drawLoop base dealers_hand deck4

drawLoop :: Hand -> Hand -> Deck -> IO undefined
drawLoop hand dealers_hand inDeck = do
   let inSum = sumOfHand hand
   -- Pre draw next card
   let card = head inDeck
   let deck = tail inDeck
   let status = showStatus hand
   
   if inSum > 21 
      then do
         print ("You lost: " ++ status) 
         playAgain deck
      else do
         print ("Current hand: " ++ status)
         print "Draw another card?"
         continue <- getLine
         case continue of
            "y"       -> drawLoop (hand ++ [card]) dealers_hand deck
            otherwise -> dealerLoop inSum dealers_hand deck 
 
dealerLoop :: Int -> Hand -> Deck -> IO undefined
dealerLoop inSum dealers_hand deck =
  -- dealer take another until
  if sumOfHand dealers_hand < inSum then do
    let card1 = head deck
    let deck1 = tail deck
    let dealers_newHand = dealers_hand ++ [card1]
    let dealers_sum = sumOfHand dealers_newHand
    print ("Dealers new hand: " ++ show dealers_sum)
    if inSum <= dealers_sum && dealers_sum <= 21
      then do
      print ("You loose. Dealer had "  ++ showStatus dealers_newHand)
      playAgain deck1
      else
      dealerLoop inSum dealers_newHand deck1
    else do
    let dealers_sum = sumOfHand dealers_hand 
    if dealers_sum == inSum 
      then do
      print ("You loose! Dealer had " ++ showStatus dealers_hand)
      playAgain deck 
      else do
      print "You won!"
      playAgain deck;

playAgain :: Deck -> IO undefined
playAgain deck = do
  print "Play again?"
  continue <- getLine
  case continue of
     "y"        -> dealHand deck
     otherwise  -> undefined

showStatus :: Hand -> String 
showStatus hand = show (map cValue hand) ++ " " ++ show (sumOfHand hand)

shuffleDeck :: StdGen -> Deck -> Deck
shuffleDeck _ [] = []
shuffleDeck gen xs = let (n,newGen) = randomR (0,length xs -1) gen
                         front = xs !! n
                     in  front : shuffleDeck newGen (take n xs ++ drop (n+1) xs)
 
sumOfHand :: Hand -> Int
sumOfHand hand   = if total == 11 && hasAce then 21 else total
   where  total  = sum $ map getValue hand
          hasAce = 0 < length (filter (\card -> cValue card == Ace) hand)

getValue :: Card -> Int
getValue (Card _ King) = 10
getValue (Card _ Queen) = 10
getValue (Card _ Jack) = 10
getValue (Card _ Ten) = 10
getValue (Card _ Nine) = 9
getValue (Card _ Eight) = 8
getValue (Card _ Seven) = 7
getValue (Card _ Six) = 6
getValue (Card _ Five) = 5
getValue (Card _ Four) = 4
getValue (Card _ Three) = 3
getValue (Card _ Two) = 2
getValue (Card _ Ace) = 1

-- Hello World
exeMain = dealHand defaultDeck

-- Entry point for unit tests.
testMain = do
    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    unless allPass exitFailure

-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION

