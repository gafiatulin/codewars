-- Product Groups
-- http://www.codewars.com/kata/5413819730b07c4c6b000067/

module CodeWars.Group.Product where
import CodeWars.Group
import Data.Monoid

instance (Group a, Group b) => Group (a, b) where
    invert (a, b) = (invert a, invert b)

embedLeft :: (Group a, Group b) => a -> (a,b)
embedLeft a = (a, mempty) 

embedRight :: (Group a, Group b) => b -> (a,b)
embedRight b = (mempty, b)