# Hearts
## Rule
* The goal is to score as few points as possible. 
* The game you will play will follow most of the classic rules of Hearts.
## Tournament
* Compete with 3 AIs from easy to hard. Most of the times my hand would win the AIs.
* Compete with other players to see which hand is the strongest. After the tournament is over, my overall ranking is 75 out of 250 contestants.
## Files modified:
* Player.hs
## Strategy:
The memory applied in this is to get all the cards which have been played previously, then check for whether any Heart card has been played. This is used to decide to play the suitable cards later on. If there is a card in the current trick, get the suit of the first card played. Get the highest-ranking card of this suit (leadCard) then check:
* If the suit is Spade and leadCard > Queen of Spade card and the Queen of Space is on hand, play it.
* Filter all the cards based on the leading suit. After filtering, divide into 2 lists with the leading card of the trick as the mid value thus there are a list of cards with value less than the mid value (list1) and another list with cards higher than the mid value (list2). 
     * If there is at least card in list1, take the card with the highest rank to play. 
     * Take the card with the lowest rank in list2. 
     * If both lists are empty, then
	          * If the opponent plays the Heart card, play the highest-ranking Heart card.
            * Otherwise, play the highest-ranking card starting with Club. If there is no Club, move to Diamond. If there is no Diamond, move to Spade. However, Queen Spade will be played last.
            * If Heart cards are the remaining on the player's hand, play the highest-ranking Heart card.
