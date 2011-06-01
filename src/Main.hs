import Data.Maybe (catMaybes, isNothing)
import Data.List (intersect, sortBy, (\\))
import Data.Ord (comparing)

main :: IO ()
main = do putStrLn "Top Matches from Pearson Simularity with Toby"
          putStrLn $ show $ topMatches critics "Toby" 3 simPearson
          putStrLn "\nEuclidian Distance between Lisa Rose and Gene Seymour"
          putStrLn $ show $ simDistance critics "Lisa Rose" "Gene Seymour"
          putStrLn "\nPearson Distance between Lisa Rose and Gene Seymour"
          putStrLn $ show $ simPearson critics "Lisa Rose" "Gene Seymour"


data Critic = Critic String [(String, Float)]
              deriving (Show, Eq)

critics :: [Critic]
critics = [Critic "Jack Matthews" [("Lady in the Water", 3.0), ("Snakes on a Plane", 4.0), ("You, Me and Dupree", 3.5), ("Superman Returns", 5.0), ("The Night Listener", 3.0)]
         , Critic "Mick LaSalle" [("Lady in the Water", 3.0), ("Snakes on a Plane", 4.0), ("Just My Luck", 2.0), ("Superman Returns", 3.0), ("You, Me and Dupree", 2.0), ("The Night Listener", 3.0)]
         , Critic "Claudia Puig" [("Snakes on a Plane", 3.5), ("Just My Luck", 3.0), ("You, Me and Dupree", 2.5), ("Superman Returns", 4.0), ("The Night Listener", 4.5)]
         , Critic "Lisa Rose" [("Lady in the Water", 2.5), ("Snakes on a Plane", 3.5), ("Just My Luck", 3.0), ("Superman Returns", 3.5), ("The Night Listener", 3.0), ("You, Me and Dupree", 2.5)]
         , Critic "Toby" [("Snakes on a Plane", 4.5), ("Superman Returns", 4.0), ("You, Me and Dupree", 1.0)]
         , Critic "Gene Seymour" [("Lady in the Water", 3.0), ("Snakes on a Plane", 3.5), ("Just My Luck", 1.5), ("Superman Returns", 5.0), ("You, Me and Dupree", 3.5), ("The Night Listener", 3.0)]
         , Critic "Michael Phillips" [("Lady in the Water", 2.5), ("Snakes on a Plane", 3.0), ("Superman Returns", 3.5), ("The Night Listener", 4.0)]]

-- Returns a distance-based similarity score for person1 and person2
simDistance :: [Critic] -> String -> String -> Maybe Float
simDistance prefs person1 person2 =
  case (getCritic prefs person1, getCritic prefs person2) of
       (Just c1, Just c2) ->
         if length inters /= 0
            then Just $ 1 / (1 + (sum sqrDiff))
            else Nothing
            where
                inters = intersectCritics c1 c2
                sqrDiff = sumSquares c1 c2 inters
       _ -> Nothing

getCritic :: [Critic] -> String -> Maybe Critic
getCritic (c:cs) name = if cName == name
                           then Just c
                           else getCritic cs name
                        where Critic cName _ = c

getCritic [] _ = Nothing


sumSquares :: Critic -> Critic -> [String] -> [Float]
sumSquares c1@(Critic _ r1) c2@(Critic _ r2) (i:is) =
  case (rating1, rating2) of
       (Just a, Just b) -> (a - b) ** 2 : sumSquares c1 c2 is
       _ -> sumSquares c1 c2 is
  where rating1 = lookup i r1
        rating2 = lookup i r2
sumSquares _ _ [] = []

intersectCritics :: Critic -> Critic -> [String]
intersectCritics (Critic _ r1) (Critic _ r2) =
  (map fst r1) `intersect` (map fst r2)


-- Returns the Pearson correlation coefficient for p1 and p2
simPearson :: [Critic] -> String -> String -> Maybe Float
simPearson prefs person1 person2 =
  case (getCritic prefs person1, getCritic prefs person2) of
       (Just c1@(Critic _ r1), Just c2@(Critic _ r2)) ->
         if and [length inters /= 0, den /=0.0]
            then Just $ num/den
            else Nothing
         where
             inters = intersectCritics c1 c2
             n = fromIntegral $ length inters :: Float

             -- get ratings from each critic
             ratings1 = catMaybes $ map ($ r1) (map lookup inters)
             ratings2 = catMaybes $ map ($ r2) (map lookup inters)

             -- Add up all the preferences
             sum1 = sum ratings1
             sum2 = sum ratings2

             -- Sum up the squares
             sum1Sq = sum $ map (** 2) ratings1
             sum2Sq = sum $ map (** 2) ratings2

             -- Sum up the products
             pSum = sum $ zipWith (*) ratings1 ratings2

             -- Calculate Pearson score
             num = pSum-(sum1*sum2/n)
             den = ((sum1Sq-(sum1**2)/n)*(sum2Sq-(sum2**2)/n))**0.5

       _ -> Nothing


-- Returns the best matches for person from the prefs dictionary.
-- Number of results and similarity function are optional params.
topMatches :: [Critic] -> String -> Int -> ([Critic] -> String -> String -> Maybe Float) -> [(Float, String)]
topMatches prefs person n simFunc =
  take n $ reverse $ sortBy (comparing fst) matches

  where
    matches = catMaybes $ map compMatch prefs

    compMatch :: Critic -> Maybe (Float, String)
    compMatch (Critic other _) =
      if other /= person
         then case simFunc prefs person other of
                   Just rating -> Just (rating, other)
                   Nothing -> Nothing
         else Nothing

-- Gets recommendations for a person by using a weighted average
-- of every other user's rankings
getRecommendations :: [Critic] -> String -> ([Critic] -> String -> String -> Maybe Float) -> [(Float, String)]
getRecommendations prefs person simFunc =
  reverse $ sortBy (comparing fst) matches
  where
    matches = catMaybes $ map compRec prefs

    -- compute totals and sum of simularities
    compRec :: Critic -> Maybe [(String, Float, Float)]
    compRec (Critic other _) =
      if other /= person
         then case simFunc prefs person other of
                   Just rating -> if rating <= 0.0
                                 -- ignore scores of zero or lower
                                 then Nothing
                                 else zipWith initDict ((map foo unscoredItems
                   Nothing -> Nothing
         else Nothing
      where otherPrefs = maybe [] (\(Critic _ x) -> x) $ getCritic prefs other
            personPrefs = maybe [] (\(Critic _ x) -> x) $ getCritic prefs person
            -- drop items scored 0
            personPrefs' = filter (\(_, x) -> if x == 0.0 then False else True) personPrefs
            -- items that person hasn't scored yet
            unscoredItems = otherPrefs \\ personPrefs'
            foo :: (String, Float) -> (String, Float, Float)

{-
def getRecommendations(prefs,person,similarity=sim_pearson):
  totals={}
  simSums={}
  for other in prefs:
    # don't compare me to myself
    if other==person: continue

    sim=similarity(prefs,person,other)
    # ignore scores of zero or lower
    if sim<=0: continue

    for item in prefs[other]:
      # only score movies I haven't seen yet
      if item not in prefs[person] or prefs[person][item]==0:
        # Similarity * Score
        totals.setdefault(item,0)
        totals[item]+=prefs[other][item]*sim
        # Sum of similarities
        simSums.setdefault(item,0)
        simSums[item]+=sim

  # Create the normalized list
  rankings=[(total/simSums[item],item) for item,total in totals.items( )]
-}
