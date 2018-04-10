-- basic utilities for poker(PvL)

module PokerRush.Poker where

import Data.List

import Debug.Trace

import PokerRush.Util

data Card
    = Card3
    | Card4
    | Card5
    | Card6
    | Card7
    | Card8
    | Card9
    | Card10
    | CardJ
    | CardQ
    | CardK
    | CardA
    | Card2
    | JokerB
    | JokerA
    deriving (Show, Read, Eq, Ord, Enum)

data CardType
    = Rocket -- Two Jokers
    | Bomb Card
    | Single Card
    | Double Card
    | Triple Card
    | TripleWithOne Card Card
    | TripleWithTwo Card Card
    | SingleStraight Card Card -- begin end
    | DoubleStraight Card Card -- begin end
    | TripleStraight Card Card -- begin end
    | Plane Card Card -- begin end -- a a a b b b c d e + where e > d > c > b > a
    | BombWithTwo Card Card Card -- card of bomb
    deriving (Show, Read)

instance Eq CardType where
    Rocket == Rocket = True
    Bomb _ == Bomb _ = True
    Single _ == Single _ = True
    Double _ == Double _ = True
    Triple _ == Triple _ = True
    TripleWithOne _ _ == TripleWithOne _ _ = True
    TripleWithTwo _ _ == TripleWithTwo _ _ = True

    SingleStraight a1 b1 == SingleStraight a2 b2 = fromEnum b1 - fromEnum a1 == fromEnum b2 - fromEnum a2
    DoubleStraight a1 b1 == DoubleStraight a2 b2 = fromEnum b1 - fromEnum a1 == fromEnum b2 - fromEnum a2
    TripleStraight a1 b1 == TripleStraight a2 b2 = fromEnum b1 - fromEnum a1 == fromEnum b2 - fromEnum a2
    Plane a1 b1 == Plane a2 b2 = fromEnum b1 - fromEnum a1 == fromEnum b2 - fromEnum a2

    BombWithTwo _ _ _ == BombWithTwo _ _ _ = True
    _ == _ = False

instance Ord CardType where
    compare Rocket Rocket = EQ
    compare (Bomb a) (Bomb b) = compare a b
    compare (Single a) (Single b) = compare a b
    compare (Double a) (Double b) = compare a b
    compare (Triple a) (Triple b) = compare a b
    compare (TripleWithOne a _) (TripleWithOne b _) = compare a b
    compare (TripleWithTwo a _) (TripleWithTwo b _) = compare a b
    compare (SingleStraight a _) (SingleStraight b _) = compare a b
    compare (DoubleStraight a _) (DoubleStraight b _) = compare a b
    compare (TripleStraight a _) (TripleStraight b _) = compare a b
    compare (Plane a _) (Plane b _) = compare a b
    compare (BombWithTwo a _ _) (BombWithTwo b _ _) = compare a b
    compare _ _ = error "not the same type"

cardTypeGreaterThan :: CardType -> CardType -> Bool
cardTypeGreaterThan _ Rocket = False
cardTypeGreaterThan Rocket _ = True
cardTypeGreaterThan (Bomb a) (Bomb b) = a > b -- bombs are greater than everything except rockets
cardTypeGreaterThan (Bomb a) _ = True
cardTypeGreaterThan t1 t2 = t1 == t2 && t1 > t2

cardTypeToDeck :: CardType -> [Card]
cardTypeToDeck Rocket = [ JokerA, JokerB ]
cardTypeToDeck (Bomb c) = replicate 4 c
cardTypeToDeck (Single c) = [c]
cardTypeToDeck (Double c) = [ c, c ]
cardTypeToDeck (Triple c) = [ c, c, c ]
cardTypeToDeck (TripleWithOne c1 c2) = [ c1, c1, c1, c2 ]
cardTypeToDeck (TripleWithTwo c1 c2) = [ c1, c1, c1, c2, c2 ]
cardTypeToDeck (SingleStraight from to) = [ from .. to ]
cardTypeToDeck (DoubleStraight from to) = [ from .. to ] ++ [ from .. to ]
cardTypeToDeck (TripleStraight from to) = [ from .. to ] ++ [ from .. to ] ++ [ from .. to ]
cardTypeToDeck (Plane from to) = [ from, from, succ from, succ from ] ++ [ from .. to ]
cardTypeToDeck (BombWithTwo c1 c2 c3) = [ c1, c1, c1, c1, c2, c3 ]

data GameState =
    GameState {
        opn_cards     :: [Card], -- cards of opponents
        my_cards      :: [Card],
        opponents     :: [Int], -- how many cards each opponent has
        no_move_count :: Int,
        cur_type      :: Maybe CardType -- current card type played
    } deriving (Show)

instance Default GameState where
    def = GameState {
        opn_cards = undefined,
        my_cards = undefined,
        opponents = undefined,
        no_move_count = 0,
        cur_type = Nothing
    }

{-

deck -> [Card]
move/card type -> CardType

-}

-- if the 2nd argument is Nothing, it's main player's move
-- otherwise the number indicates the opponent
makeMove :: Maybe Int -> Maybe CardType -> GameState -> GameState
makeMove mplayer Nothing state =
    -- trace (show $ no_move_count nstate) nstate
    -- where nstate =
    -- trace (show $ no_move_count state) $
    if no_move_count state + 1 >= length (opponents state) then
        trace "clear current type" $ state {
            no_move_count = 0,
            cur_type = Nothing
        }
    else
        state {
            no_move_count = no_move_count state + 1
        }

makeMove mplayer (Just move) state =
    case mplayer of
        Nothing ->
            state {
                my_cards = deductList (my_cards state) deck,
                no_move_count = 0,
                cur_type = Just move
            }

        Just i ->
            state {
                opn_cards = deductList (opn_cards state) deck,
                opponents =
                    take i opns ++
                    [ (opns !! i) - length deck ] ++
                    drop (i + 1) opns,

                no_move_count = 0,
                cur_type = Just move
            }

    where opns = opponents state
          deck = cardTypeToDeck move

-- sort a deck to (card, number) pairs
groupDeck = groupList

isStraight :: [Card] -> Bool
isStraight [] = True
isStraight [c] = c < Card2
isStraight (a:b:rst) = b < Card2 && (succ a == b) && isStraight (b:rst)

-- all possible moves from a deck
possibleMovesOfType :: CardType -> [Card] -> [CardType]
possibleMovesOfType ctype deck =
    case ctype of
        Single _ -> map (Single . fst) sorted
        Double _ -> map (Double . fst) (repeat 2)
        Triple _ -> map (Triple . fst) (repeat 3)
        Bomb _ -> bombs
        Rocket -> if JokerB `elem` deck && JokerA `elem` deck then [Rocket] else []

        TripleWithOne _ _ -> do
            (c1, _) <- repeat 3
            (c2, _) <- filter ((/= c1) . fst) sorted
            return (TripleWithOne c1 c2)

        TripleWithTwo _ _ -> do
            (c1, _) <- repeat 3
            (c2, _) <- filter ((/= c1) . fst) (repeat 2)
            return (TripleWithTwo c1 c2)
        
        SingleStraight _ _ -> single_straights
        DoubleStraight _ _ -> straights (map fst (repeat 2)) DoubleStraight
        TripleStraight _ _ -> straights (map fst (repeat 3)) TripleStraight

        Plane _ _ -> do
            (SingleStraight from to) <- single_straights
            let is_plane = Just True == do
                    a <- lookup from sorted
                    b <- lookup (succ from) sorted
                    return (a >= 3 && b >= 3)

            if is_plane then
                return (Plane from to)
            else
                fail "not a plane"

        BombWithTwo _ _ _ -> do
            (Bomb c0) <- bombs
            (c1, n) <- filter ((/= c0) . fst) sorted
            (c2, _) <- filter (\(c, _) -> if n == 1 then c /= c0 && c /= c1 else c /= c0) sorted
            return (BombWithTwo c0 c1 c2)

    where
        sorted = groupDeck deck
        repeat n = (filter ((>= n) . snd) sorted)

        bombs = map (Bomb . fst) (repeat 4)
        single_straights = straights (map fst sorted) SingleStraight

        straights :: [Card] -> (Card -> Card -> CardType) -> [CardType]
        straights [] cons = []
        straights sorted cons =
            -- at least 5 elements
            map (\lst -> cons (head lst) (last lst))
                (takeWhile isStraight (map (`take` sorted) [ 5 .. length sorted ])) ++
            straights (tail sorted) cons

possibleMoves :: [Card] -> [CardType]
possibleMoves deck =
    concatMap (`possibleMovesOfType` deck)
    [
        Single u, Double u, Triple u,
        Bomb u, Rocket, TripleWithOne u u,
        TripleWithTwo u u, SingleStraight u u,
        DoubleStraight u u, TripleStraight u u,
        Plane u u, BombWithTwo u u u
    ]

-- possibility of having the deck

-- n cards in total
-- player A has m cards
-- he wish to have b CardK's, for instance
-- while there are k CardK's in the deck
-- p = mCb * k / n * (k - 1) / (n - 1) * (k - 2) / (n - 2) * ... * (k - b + 1) / (n - b + 1)
possibilityHavingDeck' :: [(Card, Int)] -> [(Card, Int)] -> Int -> Int -> Double
possibilityHavingDeck' _ [] _ _ = 1
possibilityHavingDeck' _ _ _ 0 = 0
possibilityHavingDeck' tot_deck ((c, b):rest) n m =
    case lookup c tot_deck of
        Nothing -> 0
        Just k ->
            if b > m || b > k then 0
            -- else if m >= n then 1
            else let
                -- p = fi k / fi n
                has_n b' =
                    if m >= n then 1 -- if m >= n, then the result is either 1 or 0(we have all cards)
                    else -- fi (choose m b') * (p ** fi b') * ((1 - p) ** fi (m - b'))
                        p (fi n) (fi k) (fi m) (fi b')
            in
                sum $ map
                (\b' -> has_n b' * possibilityHavingDeck' tot_deck rest (n - k) (m - b'))
                [ b .. min m k ]
                -- fromIntegral (choose m b) *
                -- (fromIntegral (product [ k .. k - b + 1 ])) /
                -- (fromIntegral (product [ n .. n - b + 1 ]))

    where p :: Double -> Double -> Double -> Double -> Double
          p n k 0 0 = 1
          p _ _ 0 _ = 0
          p n k m 0 = ((n - k) / n) * p (n - 1) k (m - 1) 0
          p n k m b = (k / n) * p (n - 1) (k - 1) (m - 1) (b - 1) +
                      ((n - k) / n) * p (n - 1) k (m - 1) b

possibilityHavingDeck :: [Card] -> [Card] -> Int -> Double
possibilityHavingDeck tot_deck deck m =
    min 1 $
    possibilityHavingDeck' (groupDeck tot_deck) (groupDeck deck) (length tot_deck) m

-- possibilityHavingDeckSpec :: [Card] -> [Card] -> Int -> Double
-- possibilityHavingDeckSpec tot_deck deck m =
--     trace (show (length match, length comb)) $ fi (length match) / fi (length comb)
--     where comb = combinations m tot_deck
--           match = filter (`contains` deck) comb

isValidMove :: GameState -> CardType -> Bool
isValidMove state move =
    case cur_type state of
        Just ctype -> move `cardTypeGreaterThan` ctype
        Nothing -> True

currentPossibleMoves :: GameState -> Maybe Int -> [CardType]
currentPossibleMoves state mopn =
    let tot_cards = case mopn of
            Just opn -> opn_cards state
            Nothing -> my_cards state
    in filter (isValidMove state) (possibleMoves tot_cards)

-- the greater the card the better
sizeCoefficient card = 1.2 ** fi (fromEnum card)

sizeDeterminant Rocket = JokerA
sizeDeterminant (Bomb a) = a
sizeDeterminant (Single a) = a
sizeDeterminant (Double a) = a
sizeDeterminant (Triple a) = a
sizeDeterminant (TripleWithOne a _) = a
sizeDeterminant (TripleWithTwo a _) = a
sizeDeterminant (SingleStraight a _) = a
sizeDeterminant (DoubleStraight a _) = a
sizeDeterminant (TripleStraight a _) = a
sizeDeterminant (Plane a _) = a
sizeDeterminant (BombWithTwo a _ _) = a

scoreCardType :: CardType -> Double
scoreCardType Rocket = scoreCardType (Bomb JokerA)
scoreCardType (Bomb a) = 16 * sizeCoefficient a
-- default score
scoreCardType ctype = 1.3 ** fi (length (cardTypeToDeck ctype) - 1) * sizeCoefficient (sizeDeterminant ctype)

-- evaluate a score for the current state
-- higher the score, better the state for the main player
evalScore :: GameState -> Double
evalScore state =
    if null (my_cards state) then trace "nearly won" $ 512 -- win
    else if any (== 0) (opponents state) then -512 -- lose
    else
        -- the fewer cards I have, the better
        -- the more cards the opponents have, the better
        -- the more complicated combinations, the better

        -- trace (show (map scoreCardType (currentPossibleMoves state Nothing))) $
        sum (map scoreCardType (possibleMoves (my_cards state)))
        * (0.7 ** fi (length (my_cards state)))
        * (1.1 ** fi (length (opn_cards state)))

        -- product (map (\(c, n) -> 1.005 ** fi (fromEnum c) * 1.05 ** fi n) (groupDeck (my_cards state))) *
        -- 0.7 ** fi (length (my_cards state)) *
        -- 1.05 ** fi (length (opn_cards state))

data SearchState =
    SearchState {
        cur_round :: Int,
        max_round :: Int
    }

instance Default SearchState where
    def = SearchState 0 3

nextRound search = search { cur_round = cur_round search + 1 }

-- -- best move for the current state
bestMove :: SearchState -> GameState -> (Maybe [Card], GameState)
bestMove search state =
    -- [(CardType, score, state)]
    let no_move_case =
            if cur_type state /= Nothing {- you can't skip if you are the leader -} then
                let nstate = makeMove Nothing Nothing state in
                [(Nothing, evalScore nstate, nstate)]
            else []

        take_move_cases = do
            move <- currentPossibleMoves state Nothing

            let nstate = makeMove Nothing (Just move) state
                score = bestOpponentMove 0 search nstate
                -- evalScore nstate

            return (Just move, score, nstate)

        all_cases = no_move_case ++ take_move_cases
    in
        if cur_round search >= max_round search ||
           null all_cases then
            (Nothing, makeMove Nothing Nothing state)
        else
            let (mdeck, _, state) =
                    maximumBy (\(_, s1, _) (_, s2, _) -> compare s1 s2) all_cases
            in  trace (show (map (\(a, b, _) -> (a, b)) all_cases)) $
                (cardTypeToDeck <$> mdeck, state)

-- guess the best move for the opponent
-- return the average score of the final state
bestOpponentMove :: Int -> SearchState -> GameState -> Double
bestOpponentMove opn search state =
    let -- [(Double, CardType)]
        possible_moves = filter ((>= 0.0001) . fst) $ do
            let tot_cards = opn_cards state
                opn_card_count = opponents state !! opn
                moves = currentPossibleMoves state (Just opn)

            move <- moves

            let deck = cardTypeToDeck move
                p = possibilityHavingDeck tot_cards deck opn_card_count
                -- nstate = makeMove (Just opn) move state

            -- recursively deduce the next opponent's move
            return (p, move)

        -- take_move_p = combinePossOr (map fst possible_moves)
        -- no_move_p = 1 - take_move_p

        no_move_score =
            bestOpponentMove (opn + 1) search (makeMove (Just opn) Nothing state)

        take_move_scores =
            flip map possible_moves $ \(p, move) ->
                p * bestOpponentMove (opn + 1) search (makeMove (Just opn) (Just move) state)

        -- all_scores = no_move_score : take_move_scores
        opn_count = length (opponents state)
    in
        if opn < opn_count then
            -- trace (concat (replicate (opn + cur_round * 3) "  ") ++ show (zip possible_moves take_move_scores)) $
            if null take_move_scores then no_move_score
            else -- foldl min inf take_move_scores
                sum take_move_scores / sum (map fst possible_moves)
        else
            let (_, nstate) = bestMove (nextRound search) state
            in evalScore nstate

stdDeck = sort (concat (replicate 4 [ Card3 .. Card2 ])) ++ [ JokerB, JokerA ]
    
{-

my card:
[Card3,Card4,Card4,Card4,Card5,Card5,Card7,Card7,Card8,Card8,Card8,Card9,Card10,Card10,CardJ,CardJ,CardQ,CardK,CardK,CardA,Card2,Card2,JokerA]

my_deck = [Card3,Card4,Card4,Card4,Card5,Card5,Card7,Card7,Card8,Card8,Card8,Card9,Card10,Card10,CardJ,CardJ,CardQ,CardQ,CardK,CardK,CardA,Card2,Card2,JokerA]

GameState { my_cards = my_deck, opn_cards = deductList stdDeck my_deck, opponents = [ 16, 16 ], cur_type = Nothing }

-}
