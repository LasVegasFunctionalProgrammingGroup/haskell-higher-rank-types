{-# LANGUAGE RankNTypes #-}
module Main (main) where
-- g1: identity function
-- GHC implicitely quantifies it to: forall a. a -> a
g1 :: a -> a
g1 x = x
-- g2: Rank2Type
g2 :: (forall a. a -> a) -> (Bool, Char)
g2 f = (f True, f 'a')
-- g3: Rank3Type
g3 :: ((forall a. a -> a) -> (Bool, Char)) -> (Char, Bool)
g3 f = (\x -> (snd x, fst x)) (f g1)
-- g4: Rank4Type
g4 :: (((forall a. a -> a) -> (Bool, Char)) -> (Char, Bool)) -> (Bool, Char)
g4 f = (\x -> (snd x, fst x)) (f g2)

main :: IO ()
main = do
  putStrLn "Rank-2 Example:" 
  putStrLn . show . fst $ (g2 g1)
  putStrLn . show . snd $ (g2 g1)
  putStrLn "Rank-3 Example:" 
  putStrLn . show . fst . g3 $ g2
  putStrLn . show . snd . g3 $ g2
  putStrLn "Rank-4 Example:" 
  putStrLn . show . fst . g4 $ g3
  putStrLn . show . snd . g4 $ g3
