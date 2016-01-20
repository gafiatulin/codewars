-- A simple English to Japanese transliterator
-- http://www.codewars.com/kata/54c3ff09f3883230ac000689/

module JpnTranslit where

import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromJust, listToMaybe)

translit :: String -> String
translit [] = []
translit xs = case listToMaybe . filter ((`isPrefixOf` xs) . fst) $ patterns of 
    Nothing -> (head xs :) . translit . tail $ xs
    Just x  -> (snd x ++) . translit . fromJust . stripPrefix (fst x) $ xs

patterns :: [(String, String)]
patterns =
    [("a","あ")   ,("i","い")   ,("u","う")   ,("e","え")
    ,("o","お")   ,("ka","か")  ,("ki","き")  ,("ku","く")
    ,("ke","け")  ,("ko","こ")  ,("ga","が")  ,("gi","ぎ")
    ,("gu","ぐ")  ,("ge","げ")  ,("go","ご")  ,("sa","さ")
    ,("shi","し") ,("su","す")  ,("se","せ")  ,("so","そ")
    ,("za","ざ")  ,("ji","じ")  ,("zu","ず")  ,("ze","ぜ")
    ,("zo","ぞ")  ,("ta","た")  ,("chi","ち") ,("tsu","つ")
    ,("te","て")  ,("to","と")  ,("da","だ")  ,("de","で")
    ,("do","ど")  ,("na","な")  ,("ni","に")  ,("nu","ぬ")
    ,("ne","ね")  ,("no","の")  ,("ha","は")  ,("hi","ひ")
    ,("fu","ふ")  ,("he","へ")  ,("ho","ほ")  ,("ba","ば")
    ,("bi","び")  ,("bu","ぶ")  ,("be","べ")  ,("bo","ぼ")
    ,("pa","ぱ")  ,("pi","ぴ")  ,("pu","ぷ")  ,("pe","ぺ")
    ,("po","ぽ")  ,("ma","ま")  ,("mi","み")  ,("mu","む")
    ,("me","め")  ,("mo","も")  ,("ya","や")  ,("yu","ゆ")
    ,("yo","よ")  ,("ra","ら")  ,("ri","り")  ,("ru","る")
    ,("re","れ")  ,("ro","ろ")  ,("wa","わ")  ,("wo","を")
    ,("n","ん")   ,("m","ん")   ,(".","。")]
