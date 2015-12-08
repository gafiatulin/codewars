-- What's up next?
-- http://www.codewars.com/kata/542ebbdb494db239f8000046

module LazyNext where

next :: Eq a => a -> [a] -> Maybe a
next _ [] = Nothing
next item [x] = Nothing
next item (x:xs) = if x == item then Just(head xs) else next item xs
