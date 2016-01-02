-- Determining if a graph has a solution
-- http://www.codewars.com/kata/53223653a191940f2b000877/

module Graph where

import Data.Graph (buildG, path)
import Control.Arrow ((&&&), (***))
import Data.Char (ord)

type Node = Char
type Arc  = (Node, Node)

solveGraph :: Node -> Node -> [Arc] -> Bool
solveGraph s e arcs | s == e = True
                    | null arcs = False
                    | ord s `within` bounds && ord e `within` bounds = path graph (ord s) (ord e)
                    | otherwise = False
                    where graph = buildG bounds edges
                          edges = map (ord *** ord) arcs
                          bounds = (minimum &&& maximum) . uncurry (++) . unzip $ edges
                          within x (a, b) = a <= x && x <= b
