module Main where

import Data.List
import Data.IORef

import Control.Monad
import Control.Exception
import Control.Monad.Loops

import System.IO
import System.IO.Unsafe
import System.Random.Shuffle
import System.Console.Readline

import Debug.Trace

import PokerRush.Util
import PokerRush.Poker

-- 3 * 13 + 2 = 41

testDeck = sort (concat (replicate 3 [ Card3 .. Card2 ])) ++ [ JokerB, JokerA ]

totalCount = length testDeck
nPlayer = 3
perPlayer = totalCount `div` nPlayer

-- testMyDeck = unsafePerformIO (take perPlayer <$> shuffleM testDeck) -- [ Card3, Card4, Card7, Card7, Card9, Card10, CardQ, CardQ, CardK, CardA, Card2, Card2, JokerA ]
-- testOpn0Deck = unsafePerformIO (take perPlayer <$> shuffleM (deductList testDeck testMyDeck))
-- testOpn1Deck = testDeck `deductList` testMyDeck `deductList` testOpn0Deck

testOpnDecks :: [[Card]]
testOpnDecks =
    map sort $
    foldl (\lst n -> take perPlayer (drop (n * perPlayer) opn_cards) : lst)
    [] [ 0 .. nPlayer - 2 ]
    where opn_cards = unsafePerformIO (shuffleM testDeck)

testMyDeck = testDeck `deductList` concat testOpnDecks

testState = def {
        my_cards = testMyDeck,
        opn_cards = concat testOpnDecks,
        opponents = replicate (nPlayer - 1) perPlayer
    }

main :: IO ()
main = iterateM_ proc (0, testState, testOpnDecks)
    where
        proc (player, state, opn_decks) =
            if player >= nPlayer - 1 then do
                -- traceM ("your deck: " ++ show (sort (opn_cards state)))

                let (best, nstate) = bestMove def state
            
                traceM ("my move: " ++ show best)
                traceM ("remaining: " ++ show (sort (my_cards nstate)))
                traceM (show nstate)

                return (0, nstate, opn_decks)
            else do
                let opn_deck = opn_decks !! player

                if not (null opn_deck) then do
                    traceM ("opponent " ++ show player ++ ": " ++ show opn_deck)

                    Just str <- readline "take a move: "

                    let mmove = read str
                        (new_decks, nstate) =
                            case mmove of
                                Just move ->
                                    if not (opn_deck `contains` cardTypeToDeck move) ||
                                       not (isValidMove state move) then
                                        error "you can't make that move"
                                    else
                                        (replaceApp player (\deck -> deck `deductList` cardTypeToDeck move) opn_decks,
                                         makeMove (Just player) mmove state)
                                Nothing ->
                                    (opn_decks, makeMove (Just player) Nothing state)

                    (nstate `seq` return (player + 1, nstate, new_decks)) `catch`
                        \e -> do
                            traceM ("move failed: " ++ show (e :: SomeException))
                            return (player, state, opn_decks)
                else
                    return (player + 1, state, opn_decks)
