import Data.Bits
import Data.Word

-- LCG: https://en.wikipedia.org/wiki/Linear_congruential_generator
newRandom :: Word -> (Int, Word) 
newRandom seed = (rnd, seed')
  where a = 1103515245
        m = 2^31
        c = 12345
        seed' = (a * seed + c) `mod` m
        rnd = fromIntegral $ seed' .&. 0X7FFFFFFF


rndNumbers :: Word -> ([Int], Word)
rndNumbers seed0 =
  let (r1, seed1) = newRandom seed0
      (r2, seed2) = newRandom seed1
  in ([r1,r2], seed2)

-------------------------------------------------------------------------------
-- State Transformer
-------------------------------------------------------------------------------

newtype ST s a = S (s -> (a, s))

runState :: ST s a -> s -> (a, s)
runState (S f) s0 = f s0 


instance Functor (ST s) where
  fmap :: (a -> b) -> ST s a -> ST s b
  fmap f sta = S (\s0 -> 
    case (runState sta s0) of
      (a, s1) -> (f a, s1))


instance Applicative (ST s) where
  pure :: a -> ST s a 
  pure a = S (\s -> (a, s))
 
  (<*>) :: ST s (a -> b) -> ST s a -> ST s b
  {- S f' <*> S a' = S $ \s0 -> 
    case f' s0 of 
      (f, s1) -> case a' s1 of
        (a, s2) -> (f a, s2) -}

  {- stf <*> sta = do
    f <- stf
    a <- sta
    return (f a) -}

  stf <*> sta = stf >>= \f -> sta >>= \a -> return (f a)


instance Monad (ST s) where
  (>>=) :: ST s a -> (a -> ST s b) -> ST s b
  (S a') >>= f = S (\s0 ->
       case a' s0 of 
         (a, s1) -> runState (f a) s1)


get :: ST s s
get = S (\s -> (s,s))

put :: s -> ST s ()
put w = S (\_ -> ((), w))

modify :: (s -> s) -> ST s ()
modify f = do
  w <- get
  put (f w)


-------------------------------------------------------------------------------
-- Random Example
-------------------------------------------------------------------------------

rndNumbers' :: ST Word [Int]
rndNumbers' = do
  r1 <- newRandom'
  r2 <- newRandom'
  return [r1,r2]

newRandom' :: ST Word Int 
newRandom' = do
 seed <- get
 let seed' = (a * seed + c) `mod` m
     rnd = fromIntegral $ seed' .&. 0X7FFFFFFF
 put seed'
 return rnd
 where a = 1103515245
       m = 2^31
       c = 12345



-------------------------------------------------------------------------------
-- Counter Example
-------------------------------------------------------------------------------

type Counter = ST Int Int

increment :: Counter
increment = do
  count <- get
  put (count + 1)
  return (count + 1)

program :: Counter
program = do
  increment
  increment
  increment

main :: IO ()
main = do
  let initialState = 0
      (result, finalState) = runState program initialState
  print finalState  -- Output should be 3