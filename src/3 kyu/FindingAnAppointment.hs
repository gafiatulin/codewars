-- Finding an appointment
-- http://www.codewars.com/kata/525f277c7103571f47000147/
-- Note: Tests fail due to ambiguous occurrence of `shuffle'

module FindingAnAppointment where

import Data.Maybe (listToMaybe)
import Control.Arrow ((***), (&&&))
import Data.List.Split (splitOn)
import Text.Printf (printf)

getStartTime :: [[(String, String)]] -> Int -> Maybe String
getStartTime schedules d = listToMaybe . map (writeT . fst) 
                         . filter (\(s1, e1) -> (s0 <= s1) && (e1 < e0) && ((>=d) . f e1 $ s1)) 
                         . uncurry (flip zip) . ((++[e0]) *** (s0:)) 
                         . unzip . foldr (g . map (readT *** readT)) [] $ schedules
                           where (s0, e0) = ((9, 0), (19, 0))
                                 f (h1, m1) (h2, m2) = 60*(h1-h2)+(m1-m2)
                                 readT = (read . head &&& read . last) . splitOn ":"
                                 writeT = uncurry (printf "%02d:%02d")
                                 g [] s2 = s2
                                 g s1 [] = s1
                                 g ((s1, e1):xs) ((s2, e2):ys) | e1 < s2 = (s1, e1) : (s2, e2) : g xs ys
                                                               | e2 < s1 = (s2, e2) : (s1, e1) : g xs ys
                                                               | otherwise =  (min s1 s2, max e1 e2) : g xs ys
