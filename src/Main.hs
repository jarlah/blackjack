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
   let deal_dealer1 = dealCard deck
   let card_dealer1 = fst deal_dealer1
   let deck_dealer1 = snd deal_dealer1 -- new deck
   -- get second card
   let deal_dealer2 = dealCard deck_dealer1
   let card_dealer2 = fst deal_dealer2
   let deck_dealer2 = snd deal_dealer2 -- new deck
   -- and the dealers hand is
   let dealers_hand = [card_dealer1, card_dealer2]
   print ("Dealers first card is: " ++ show card_dealer1)
   -- get first cards
   let deal1 = dealCard deck_dealer2
   let card1 = fst deal1
   let deck1 = snd deal1 -- new deck
   -- get second card
   let deal2 = dealCard deck1
   let card2 = fst deal2
   let deck2 = snd deal2 -- new deck
   let base = [card1, card2]
   -- Enter draw loop
   drawLoop base dealers_hand deck2
   print "Finished"

drawLoop :: Hand -> Hand -> Deck -> IO (Int, Deck)
drawLoop hand dealers_hand inDeck = do
   let sum = sumOfHand hand
   -- Pre draw next card
   let deal = dealCard inDeck
   let card = fst deal
   let deck = snd deal
   if sum == 21 
      then do
         print "Blackjack!"
         exitSuccess
      else 
         if sum > 21 
            then do
               print ("You lost: " ++ show sum) 
               exitFailure
            else 
               print ("Current hand: " ++ show sum)
   print "Draw another card?"
   continue <- getLine
   case continue of
      "y"       -> drawLoop (hand ++ [card]) dealers_hand deck
      otherwise -> do
         -- dealer take another
         if sumOfHand dealers_hand < 18 then do
            let deal_dealer3 = dealCard deck
            let card_dealer3 = fst deal_dealer3
            let deck_dealer3 = snd deal_dealer3
            let dealers_newHand = dealers_hand ++ [card_dealer3]
            print ("Dealers new hand: " ++ show (sumOfHand dealers_newHand))
            let dealers_sum = sumOfHand dealers_newHand
            if sum <= dealers_sum && dealers_sum <=21 then do
               print "You lost"
               exitFailure
               else do
                  print "You won!"
                  exitSuccess
            else do
               print "You won!"
               exitSuccess

shuffleDeck :: StdGen -> Deck -> Deck
shuffleDeck _ [] = []
shuffleDeck gen xs = let (n,newGen) = randomR (0,length xs -1) gen
                         front = xs !! n
                     in  front : shuffleDeck newGen (take n xs ++ drop (n+1) xs)

dealCard :: Deck -> (Card, Deck)
dealCard []     = error "Can't pop from an empty stack!"
dealCard (card:deck) = (card, deck)

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
    putStrLn (hello "World")

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

