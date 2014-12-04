{-# LANGUAGE RankNTypes #-}

-- Fast Food Restaurant
module Main where
  data Food =
      Hamburger
    | Hotdog
    | Salad
    deriving (Eq, Show)

  data Drink =
      Coke
    | Pepsi
    | Water
    deriving (Eq, Show)

  data Order =
      FoodOrder Food
    | DrinkOrder Drink
    deriving (Show)

  data Meal =
    Meal Food Drink
    deriving (Show)

  -- Parenthesis
{-
  a -> b -> c
  a -> (b -> c)
  (a -> (b -> c))
  (a -> b -> c)

  (a -> b) -> c
  ((a -> b) -> c)

  opposite for function application
  f :: (a -> b) -> c -> d
  g :: a -> b
  f g c
  (f g) c
  ((f g) c)
  (f g c)

  f (g c)
  (f (g c))
-}

  -- No polymorphism or Rank 0
  pairing :: Food -> Drink
  pairing Hamburger = Coke
  pairing Hotdog = Pepsi
  pairing Salad = Water

  -- Polymorphism or Rank 1
  id' :: a -> a
  id' a = a

  length' :: [a] -> Int
  length' [] = 0
  length' (_:xs) = 1 + length' xs


  -- Bounded (Still Rank 1) Polymorphism
  type Pricer = Show a => a -> Int
  
  always2Price :: Pricer
  always2Price item = 2

  lengthPrice :: Pricer
  lengthPrice item = length (show item)

  twiceLengthPrice :: Pricer
  twiceLengthPrice item = 2 * lengthPrice item

  -- forall quantifier
  id'' :: forall a . a -> a
  id'' a = a

  -- Rank 2 Polymorphism
  type Pricer' = (forall a . Show a => a -> Int)

  priceMeal :: Pricer' -> Meal -> Int
  priceMeal pricer (Meal f d) = pricer f + pricer d

  priceMealProduct :: Pricer' -> Meal -> Int
  priceMealProduct pricer (Meal f d) = pricer f * pricer d

  -- Rank 3...
  type PriceMeal = Pricer' -> Meal -> Int
  superSizer :: PriceMeal -> PriceMeal
  superSizer pm = \ p m -> (pm p m) * 2

  compareModifiedSumAndProductPrices :: (PriceMeal -> PriceMeal) -> Meal -> (Int, Int)
  compareModifiedSumAndProductPrices modifier meal = ((modifier priceMeal) lengthPrice meal, (modifier priceMealProduct) always2Price meal)

  main = do
    print (pairing Hamburger)
    print (pairing Hotdog)
    print (always2Price Hotdog)
    print (priceMeal always2Price (Meal Hotdog Coke))
    print (priceMeal lengthPrice (Meal Hotdog Coke))
    print (superSizer priceMeal lengthPrice (Meal Hotdog Coke))
    print (compareModifiedSumAndProductPrices superSizer (Meal Hotdog Coke))


