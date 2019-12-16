-- | Write a report describing your design and strategy here.
module Player (
    playCard,
    makeBid
)
where
{-
Organized structure:
    Simpler functions such as transformCard, getRank, getSuit called by the composite functions are placed
    at the beginning. There are also some functions such as headRankCard and lastRankCard used to avoid 
    repetitions in the composite functions. The naming convention is very simple to make it easy to understand.

Strategy:
    The memory applied in this is to get all the cards which have been played previously, then check for 
    whether any Heart card has been played. This is used to decide to play the suitable cards later on.

    If there is a card in the current trick, get the suit of the first card played. Get the highest-ranking card 
    of this suit (leadCard) then check 
        i)  If the suit is Spade and leadCard > Queen of Spade card and the Queen of Space is on hand, play it.
        ii) Filter all the cards based on the leading suit. After filtering, divide into 2 lists with the leading 
        card of the trick as the mid value thus there are a list of cards with value less than the mid value (list1) 
        and another list with cards higher than the mid value (list2). 
            a) If there is at least card in list1, take the card with the highest rank to play. 
            b) Take the card with the lowest rank in list2. 
            c) If both lists are empty, then
	            1)  If the opponent plays the Heart card, play the highest-ranking Heart card.
                2)  Otherwise, play the highest-ranking card starting with Club. If there is no Club, move to Diamond. 
                If there is no Diamond, move to Spade. However, Queen Spade will be played last.
                3)	If Heart cards are the remaining on the player's hand, play the highest-ranking Heart card.

Reference:
    Oh Hell Game - https://github.com/harsilspatel/ohHell/blob/master/staticgame/Player.hs
-}

-- You can add more imports as you need them.
import Hearts.Types
import Hearts.Rules
import Cards

-- | Transform the card to (card, show card)
transformCard :: Card -> (Card, String)
transformCard card = (card, show card)

-- | Get the rank of the Card
getRank :: Card -> Rank
getRank (Card _ rank) = rank

-- | Get the suit of the Card
getSuit :: Card -> Suit
getSuit (Card suit _) = suit

-- | Return True if list is not empty, otherwise return False
notNull :: [Card] -> Bool
notNull ls = not $ null ls

-- | Sort the list of cards from lowest to highest
-- >> Use the function from https://github.com/harsilspatel/ohHell/blob/master/staticgame/Player.hs
sortByRank :: [Card] -> [Card]
sortByRank [] = []
sortByRank (card:cards) = (sortByRank lower) ++ [card] ++ (sortByRank higher)
    where
        lower = filter((< getRank card) . getRank) cards
        higher = filter((>= getRank card). getRank) cards
        
-- | Get the lowest-ranking card
headRankCard :: [Card] -> Card
headRankCard cards = head $ sortByRank cards

-- | Get the highest-ranking card
lastRankCard :: [Card] -> Card
lastRankCard cards = last $ sortByRank cards

-- | From cards in hand, filter those matching the given suit
getCardsOfSuit :: [Card] -> Suit -> [Card]
getCardsOfSuit cards suit = filter ((suit==) . getSuit) cards

-- | Get the suit of the current trick
getPlayedSuitOnTrick :: [(Card, PlayerId)] -> Suit
getPlayedSuitOnTrick cardsTrick = getSuit firstCard
    where
        firstCard = last [card | (card, _) <- cardsTrick]

-- | Get the lead card on the current trick
-- >> return (Spade Queen) if is present
-- >> else, return the highest-ranking Heart card
-- >> otherwise return random card with the highest rank
getLeadCardOnTrick :: [(Card, PlayerId)] -> Suit -> Card
getLeadCardOnTrick trick suit
    | notNull suitCards = lastRankCard suitCards
    | notNull heartCards = lastRankCard heartCards
    | otherwise = lastRankCard playedCards
    where
        playedCards = [fst x | x <- trick]        
        suitCards = getCardsOfSuit playedCards suit
        heartCards = getCardsOfSuit playedCards Heart

-- | Get lowest-ranking card from your hand to play first
getFirstPlayingCard :: [Card] -> Card
getFirstPlayingCard handCards
    | notNull clubCards = headRankCard clubCards
    | notNull diamondCards = headRankCard diamondCards
    | notNull spadeCards = headRankCard spadeCards
    | otherwise = headRankCard heartCards
    where
        clubCards = getCardsOfSuit handCards Club
        diamondCards = getCardsOfSuit handCards Diamond
        spadeCards = getCardsOfSuit handCards Spade
        heartCards = getCardsOfSuit handCards Heart

-- | Extract previous tricks from memory
extractMemoryTricks :: Maybe ([(Card, PlayerId)], String) -> [(Card, PlayerId)]
extractMemoryTricks Nothing = []
extractMemoryTricks (Just (xs, _)) = xs

-- | Using memory to check if the Heart card has been played
checkHeartBroken :: PlayerId -> Maybe ([(Card, PlayerId)], String) -> Bool
checkHeartBroken playerId memory
    | (memory == Nothing) = False
    | length heartCards > 0 = True
    | otherwise = length otherHeartCards > 0
    where
        tricks = extractMemoryTricks memory   -- ^ extract previous tricks from memory
        heartCards = filter (\(card, pid) -> pid == playerId && getSuit card == Heart) tricks
        otherHeartCards = filter (\(card, _) -> getSuit card == Heart) tricks

-- | Check if there is a card on your hand
checkCardInHand :: Card -> [Card] -> Bool
checkCardInHand _ [] = False
checkCardInHand card (c : handCards)
    | card == c = True
    | otherwise = checkCardInHand card handCards

-- | When there is no matching card with the suit in the trick, get another card to play
-- 1. Based on memory, if Heart is played then can play highest-ranking Heart card
-- 2. Otherwise, pick Club/Diamond/Spade card (exclude Queen of Spade) to play
-- 3. Then, check if your hand has Queen of Spade, play Queen of Spade
-- 4. Finally, choose highest-ranking Heart card to play
getOtherSuitCardForTrick :: [Card] -> Bool -> Card
getOtherSuitCardForTrick handCards heartBroken
    | length heartCards > 0 && heartBroken == True = lastRankCard heartCards
    | notNull clubCards = lastRankCard clubCards
    | notNull diamondCards = lastRankCard diamondCards
    | notNull spadeCards = lastRankCard spadeCards
    | sqInHand == True = sq
    | otherwise = lastRankCard heartCards
    where
        sq = Hearts.Rules.queen_spades
        sqInHand = checkCardInHand sq handCards
        clubCards = getCardsOfSuit handCards Club
        diamondCards = getCardsOfSuit handCards Diamond
        spadeCards = filter (\c -> getSuit c == Spade && c /= sq) handCards
        heartCards = getCardsOfSuit handCards Heart

-- | The simple AI that is based on the played-cards in the current trick trying to WIN
-- WIN means try to get the point as fewest as possible for each trick
tryToWinTheTrick :: [Card] -> [(Card, PlayerId)] -> Bool -> Card
tryToWinTheTrick handCards trickCards heartBroken
    | trickSuit == Spade && sqInHand == True && leadCard > queenSpade = queenSpade
    | notNull lesserSuitCards = lastRankCard lesserSuitCards
    | notNull greaterSuitCards = headRankCard greaterSuitCards
    | otherwise = otherSuiteCard
    where
        queenSpade = Hearts.Rules.queen_spades
        sqInHand = checkCardInHand queenSpade handCards
        trickSuit = getPlayedSuitOnTrick trickCards
        leadCard = getLeadCardOnTrick trickCards trickSuit
        lesserSuitCards = filter (\c -> getSuit c == trickSuit && c < leadCard) handCards
        greaterSuitCards = filter (\c -> getSuit c == trickSuit && c >= leadCard) handCards
        otherSuiteCard = getOtherSuitCardForTrick handCards heartBroken

-- | If no card is in trick, you play first. Else there is at least 1 card, you follow suit
playCard :: PlayFunc
playCard playerId handCards currentTrick memory
    | null currentTrick = transformCard firstCard
    | otherwise = transformCard choseCard
    where
        firstCard = getFirstPlayingCard handCards
        heartBroken = checkHeartBroken playerId memory
        choseCard = tryToWinTheTrick handCards currentTrick heartBroken

-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined
