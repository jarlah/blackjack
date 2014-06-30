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
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Monad (unless)
import Data.List (stripPrefix, length)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)
import System.Random
import Control.Applicative
import System.Exit

type Deck = [Card]

data Card = Card {cSuit :: CSuit
                 ,cType :: CType 
                 }
                 deriving Show

data CSuit = Spades | Hearts | Diamonds | Clubs deriving (Show, Enum, Eq)

data CType = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Enum, Eq)

type Hand = [Card]

dealHand :: Deck -> IO ()
dealHand inDeck = do
   -- make random generator
   rng <- newStdGen

   -- shuffle
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

   print ("Dealers faced up card is: " ++ show (cType card1))

   -- get first cards
   let card3 = head deck2
   let deck3 = tail deck2 -- new deck
   -- get second card
   let card4 = head deck3
   let deck4 = tail deck3 -- new deck
   let base = [card1, card2]

   -- Enter draw loop
   drawLoop base dealers_hand deck2

drawLoop :: Hand -> Hand -> Deck -> IO (undefined)
drawLoop hand dealers_hand inDeck = do
   let inSum = sumOfHand hand
   -- Pre draw next card
   let card = head inDeck
   let deck = tail inDeck
   let status = show (map cType hand) ++ " " ++ show inSum
   if inSum > 21 
      then do
         print ("You lost: " ++ status) 
         exitFailure
      else 
         print ("Current hand: " ++ status)
   print "Draw another card?"
   continue <- getLine
   case continue of
      "y"       -> drawLoop (hand ++ [card]) dealers_hand deck
      otherwise -> dealerLoop inSum dealers_hand deck

dealerLoop :: Int -> Hand -> Deck -> IO (undefined)
dealerLoop inSum dealers_hand deck = do
   -- dealer take another until
   if sumOfHand dealers_hand < inSum then do
      let card1 = head deck
      let deck1 = tail deck
      let dealers_newHand = dealers_hand ++ [card1]
      print ("Dealers new hand: " ++ show (sumOfHand dealers_newHand))
      let dealers_sum = sumOfHand dealers_newHand
      if inSum <= dealers_sum && dealers_sum <= 21 then do
         print "You lost"
         exitFailure
         else
            dealerLoop inSum dealers_newHand deck1
      else do
         let dealers_sum = sumOfHand dealers_hand
         if dealers_sum == inSum
            then do
               print ("You loose! Dealer had " ++ show (map cType dealers_hand) ++ show dealers_sum)
               exitFailure
               else do
                  print "You won!"
                  exitSuccess

shuffleDeck :: StdGen -> Deck -> Deck
shuffleDeck _ [] = []
shuffleDeck gen xs = let (n,newGen) = randomR (0,length xs -1) gen
                         front = xs !! n
                     in  front : shuffleDeck newGen (take n xs ++ drop (n+1) xs)

sumOfHand :: Hand -> Int
sumOfHand hand   = if total == 11 && hasAce then 21 else total
   where  total  = sum $ map getValue hand
          hasAce = 0 < (length $ filter (\card -> (cType card) == Ace) hand)

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


defaultDeck :: Deck
defaultDeck = [Card Spades King
         ,Card Spades Queen
         ,Card Spades Jack
         ,Card Spades Ten
         ,Card Spades Nine
         ,Card Spades Eight
         ,Card Spades Seven
         ,Card Spades Six
         ,Card Spades Five
         ,Card Spades Four
         ,Card Spades Three
         ,Card Spades Two
         ,Card Spades Ace
         ,Card Hearts King
         ,Card Hearts Queen
         ,Card Hearts Jack
         ,Card Hearts Ten
         ,Card Hearts Nine
         ,Card Hearts Eight
         ,Card Hearts Seven
         ,Card Hearts Six
         ,Card Hearts Five
         ,Card Hearts Four
         ,Card Hearts Three
         ,Card Hearts Two
         ,Card Hearts Ace
         ,Card Clubs King
         ,Card Clubs Queen
         ,Card Clubs Jack
         ,Card Clubs Ten
         ,Card Clubs Nine
         ,Card Clubs Eight
         ,Card Clubs Seven
         ,Card Clubs Six
         ,Card Clubs Five
         ,Card Clubs Four
         ,Card Clubs Three
         ,Card Clubs Two
         ,Card Clubs Ace
         ,Card Diamonds King
         ,Card Diamonds Queen
         ,Card Diamonds Jack
         ,Card Diamonds Ten
         ,Card Diamonds Nine
         ,Card Diamonds Eight
         ,Card Diamonds Seven
         ,Card Diamonds Six
         ,Card Diamonds Five
         ,Card Diamonds Four
         ,Card Diamonds Three
         ,Card Diamonds Two
         ,Card Diamonds Ace
         ]


-- Simple function to create a hello message.
hello s = "Hello " ++ s

-- Tell QuickCheck that if you strip "Hello " from the start of
-- hello s you will be left with s (for any s).
prop_hello s = stripPrefix "Hello " (hello s) == Just s

prop_deck_size = length defaultDeck == 52

-- Hello World
exeMain = do
    dealHand defaultDeck

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

