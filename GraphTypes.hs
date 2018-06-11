module GraphTypes (
    Graph
  ) where

fst' :: (a,b,c) -> a
fst' (a,_,_) = a

snd' :: (a,b,c) -> b
snd' (_,b,_) = b

third' :: (a,b,c) -> c
third' (_,_,c) = c

class Eq v => Graph v e g where
  edgesFrom :: Integral n => g -> v -> [(v, n, e)]

  edgesBetween :: Integral n => g -> v -> v -> [(n,e)]
  edgesBetween g v v' =
    fmap (\(_,n,e) -> (n,e)) .
    filter ((==v') . fst') $
    edgesFrom g v

  minCostEdge :: Integral n => g -> v -> v -> Maybe (n,e)
  minCostEdge g v v' =
    case edges of
      [] -> Nothing
      _  -> Just $ minimumBy (comparing fst) edges
    where edges = edgesBetween g v v'
