-- Convert boolean values to strings 'Yes' or 'No'.
-- http://www.codewars.com/kata/53369039d7ab3ac506000467

module YesNo where

boolToWord :: Bool -> String
boolToWord True = "Yes"
boolToWord _ = "No"
