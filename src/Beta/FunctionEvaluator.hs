-- Recurrence relations
-- http://www.codewars.com/kata/550756a881b8bdba99000348/

module FunctionEvaluator where

import qualified Data.Map as M
import Control.Arrow (second, (&&&))

evaluateFunction :: Ord a => (a -> Either b ([a], [b] -> b)) -> a -> b
evaluateFunction f a = snd $ eval (M.empty, a)
    where eval (m, a) = case M.lookup a m of
              Nothing -> eval (M.insert a (f a) m, a)
              Just (Left b) -> (m, b)
              Just (Right (as, g)) -> (\(m', b) -> (M.insert a (Left b) m', b)) . second g . foldr (\v (m, bs) ->  second (:bs) . eval $ (m, v)) (m, []) $ as
