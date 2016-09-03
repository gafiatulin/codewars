-- Translate DNA to protein in 6 frames
-- https://www.codewars.com/kata/5708ef48fe2d018413000776

module Codewars.Kata.DNA(translateWithFrame) where
import Codewars.Kata.DNA.Types
import Data.List (unfoldr)

reverseComplement :: Nucleotide -> Nucleotide
reverseComplement A = T
reverseComplement G = C
reverseComplement T = A
reverseComplement C = G

aa :: DNA -> [[AminoAcid]]
aa dna = map ((map codon . unfoldr split) . (`drop` dna)) [0, 1, 2]

split :: DNA -> Maybe (Codon, DNA)
split (a:b:c:xs) = Just ((a, b, c), xs)
split _ = Nothing

translateWithFrame :: DNA -> [Int] -> [[AminoAcid]]
translateWithFrame dna fs = map (\i -> aas !! i ) is
    where aas = (aa dna) ++ (aa rcdna)
          rcdna = reverse . map reverseComplement $ dna
          is = map (\n -> if n < 0 then 2 + abs n else pred n) fs
