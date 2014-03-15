{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}

import Control.Monad.Trans.Iter
import qualified Data.DList as DL
import Criterion.Main
import Data.Either
import Data.Monoid
import Control.Arrow
import Control.Monad
import Control.Monad.Identity
import Data.List
import Data.Function

delay_ :: (MonadFree f m, Monad f) => m ()
delay_ = delay $ return ()

countdown :: Int -> Iter Int
countdown = countdown' 0
            where
              countdown' acc 0 = return acc
              countdown' acc n | n > 0 = delay $ countdown' (acc + 1) (n - 1)
  

interleave1 :: Monad m => [IterT m a] -> IterT m [a]
interleave1 ms = IterT $ do
  xs <- mapM runIterT ms
  if null (rights xs)
     then return . Left $ lefts xs
     else return . Right . interleave1 $ map (either return id) xs

interleave2 :: Monad m => [IterT m a] -> IterT m [a]
interleave2 = fmap DL.toList . interleave2' . map (Right . fmap DL.singleton)
  where
  interleave2' :: (Monad m, Monoid a) => [Either a (IterT m a)] -> IterT m a
  interleave2' ms = IterT $ do
    xs <- mapM (either (return . Left) runIterT) ms
    case compact xs of
      [l@(Left _)] -> return l
      xs'          -> return . Right $ interleave2' xs'

  compact :: (Monoid a) => [Either a b] -> [Either a b]
  compact []               = []
  compact (r@(Right _):xs) = r:(compact xs)
  compact (l@(Left a) :xs)  = compact' a xs

  compact' a []               = [Left a] 
  compact' a (r@(Right _):xs) = (Left a):(r:(compact xs))
  compact' a (  (Left a'):xs) = compact' (a <> a') xs

interleave3 :: Monad m => [IterT m a] -> IterT m [a]
interleave3 ms = IterT $ do
  xs <- mapM runIterT ms
  let (ls, rs) = lefts &&& rights $ xs
  return $ if null rs
             then Left ls
             else Right . fmap (fillin xs) $ interleave3 rs


-- | Replace Right's with values from second list.
-- > fillin [Left 1, Left 2, Right "a", Left 4, Right "b"] [3, 5]
-- [1, 2, 3, 4, 5]
fillin :: [Either a b] -> [a] -> [a]
fillin [] _ = []
fillin (Right _ : xs) (y:ys) = y : fillin xs ys
fillin (Left  x : xs) ys     = x : fillin xs ys
                               
interleave4 :: Monad m => [IterT m a] -> IterT m [a]
interleave4 = interleave4' . map (Right . fmap (:[]))
  where
  interleave4' :: (Monad m, Monoid a) => [Either a (IterT m a)] -> IterT m a
  interleave4' ms = IterT $ do
    xs <- mapM (either (return . Left) runIterT) ms
    case compact xs of
      [l@(Left _)] -> return l
      xs'          -> return . Right $ interleave4' xs'

  compact :: (Monoid a) => [Either a b] -> [Either a b]
  compact []               = []
  compact (r@(Right _):xs) = r:(compact xs)
  compact (l@(Left a) :xs)  = compact' a xs

  compact' a []               = [Left a] 
  compact' a (r@(Right _):xs) = (Left a):(r:(compact xs))
  compact' a (  (Left a'):xs) = compact' (a <> a') xs


interleavedCount :: (forall a m. (Monad m) => [IterT m a] -> IterT m [a]) -> [Int] -> [Int]
interleavedCount interleave l = runIdentity . retract . interleave $ map countdown l

fs :: [(String, forall m a. (Monad m) => [IterT m a] -> IterT m [a])]
fs = [("interleave1",interleave1),
      ("interleave2",interleave2),
      ("interleave3",interleave3),
      ("interleave4",interleave4)]

divExact :: (Integral a) => a -> a -> a
divExact a b | a `rem` b == 0 = a `div` b
divExact a b                  = error $ show (fromIntegral a) ++ " is not a multiple of " ++ show (fromIntegral b)


data Tag = Tag { version   :: String,
                 stepCount :: String,
                 listSize  :: String,
                 dist      :: String,
                 order     :: String }

data TestCase = TestCase {
  tag :: Tag,
  execute :: [Int] -> [Int],
  input :: [Int]
 }
  

inputs :: [TestCase]
inputs = do
  (functionName, function) <- fs
  (stepCountName, (stepCount, listSizes)) <- map ((("10^" ++) . show . fst) &&& id) [(5,[2,3,4]),
                                                                                     (6,[3,4,5])]
                                                                                     
  (listSizeName,  listSize)  <- map ((("10^" ++) . show) &&& id) listSizes
  (distName, dist)           <- [("uniform",[1]),
                                 ("half"   ,[1,3]),
                                 ("10%"    ,[1, 1, 1, 1, 1, 1, 1, 1, 1,11]),
                                 ("linear" ,[1, 3, 5, 7, 9,11,13,15,17,19])]
  (orderName, order)     <- [("sorted", sort),("non-sorted", id)]
  
  let tag = Tag { version = functionName,
                  stepCount = stepCountName,
                  listSize  = listSizeName,
                  dist = distName,
                  order = orderName }
  -- Adjust the distribution so that the number of total steps stays the same
  let distFactor = (sum dist) `divExact` (length dist)
  let list' = take (10 ^ listSize) $
             zipWith (*)
               (cycle dist)
               (repeat $ (10^(stepCount - listSize)) `divExact` distFactor)
  let list = order list'

  when (sum list /= (10 ^ stepCount)) $ error "distribution massaging done incorrectly"
  return $ TestCase tag (interleavedCount function) list
  

groupByKey :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupByKey f = map (fst . head &&& map snd)
                   . groupBy ((==) `on` fst)
                   . sortBy (compare `on` fst)
                   . map (f &&& id)
               
main = defaultMain $ do
  (group, testcases) <- groupByKey ((stepCount &&& listSize &&& order) . tag) $
                            filter (not . (== ("10^5","interleave4")) . (listSize &&& version) . tag) $
                            filter ((\t -> dist t      == "10%") . tag)
                            inputs

  return $ bgroup (show group) $ do
    testcase <- testcases
    return $ bench (version . tag $ testcase)
      $ nf (execute testcase) (input testcase)
    
    
  
