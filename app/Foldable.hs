-- module Foldable where
--     import Data.Foldable
--     import Data.Monoid
    
--     a = fold [Sum 2398]
--     b = fold [1,2,23] :: Sum Int
--     c = fold ["hello", "julie"]
--     d = foldMap First [Just 3, Just 4, Just 2, Nothing]
--     e = foldMap (* 5) [1,2,3,4 :: Sum Int]
--     data Identity a = Identity a
--     instance Foldable (Identity) where
--         foldMap f (Identity x) = f x

--     az = foldr (+) 1 Nothing
--     ab = foldr (+) 1 (Just 3)
--     data Optional a = Nada | Yep a
--     instance Foldable Optional where  
--         foldr f seed (Yep a)= a `f` seed
--         foldr f seed Nada = seed
--         foldMap f (Yep a) = f a 
--         foldMap f Nada = mempty
--         fold (Yep a) = a
--         fold Nada = mempty
 
--     someList = fmap sum [Just 1, Just 2, Nothing, Just 3, Nothing]
--     ay = sequenceA $ fmap Just [1,2,3]
    