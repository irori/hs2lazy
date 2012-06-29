module SCC (scc) where
import List (elemIndex)
import Monad (when)
import Array

data SM s a = SM (s -> (a, s))

instance Monad (SM s) where
    SM c1 >>= fc2 = SM (\s0 -> let (r,s1) = c1 s0; SM c2 = fc2 r in c2 s1)
    return k = SM (\s -> (k,s))

readSM :: (s -> a) -> SM s a
readSM f = SM (\s -> (f s, s))

updateSM :: (s -> s) -> SM s ()
updateSM f =  SM (\s -> ((), f s))

changeSM :: (s -> (a, s)) -> SM s a
changeSM f = SM (\s -> f s)

runSM :: s -> SM s a -> (a,s)
runSM s0 (SM c) = c s0

type SCC i a = SM (Int, Array i Int, [i], [[i]]) a

newId :: SCC i Int
newId = changeSM (\(i,m,s,r) -> (i+1, (i+1,m,s,r)))

setId :: Ix i => i -> Int -> SCC i ()
setId v i = updateSM (\(i',m,s,r) -> (i', m // [(v, i)], s, r))

idof :: Ix i => i -> SCC i Int
idof v = readSM (\(i,m,s,r) -> m!v)

push :: i -> SCC i ()
push v = updateSM (\(i,m,s,r) -> (i,m,v:s,r))

putComponents :: Ix i => i -> SCC i ()
putComponents v = updateSM $ \(i,m,s,r) ->
		  let (c, _:s') = break (== v) s
		      r' = (v:c) : r
		      m' = m // [(u, rangeSize (bounds m)) | u <- (v:c)]
		  in (i,m',s',r')

adjId :: Ix i => Array i [i] -> i -> SCC i Int
adjId adj v = do i <- idof v
		 case i of 0 -> visit adj v
			   n -> return n

visit :: Ix i => Array i [i] -> i -> SCC i Int
visit adj v = do nodeId <- newId
		 setId v nodeId
		 push v
		 ids <- mapM (adjId adj) (adj!v)
		 let minId = foldl min nodeId ids
		 when (nodeId == minId) (putComponents v)
		 return minId

scc :: Eq a => [(a, [a])] -> [[a]]
scc adj = map (map (vs !!)) (scc' adj')
    where adj' = array (0, length adj - 1) (map toIndex adj)
	  toIndex (v, adjs) = (index v, map index adjs)
	  vs = map fst adj
	  index v = case elemIndex v vs of
		    Just i -> i
		    Nothing -> error "Illegal adjacency list"

scc' :: Ix i => Array i [i] -> [[i]]
scc' adj = r
    where (_, (_,_,_,r)) = runSM (0, idMap, [], []) sm
	  idMap = array (bounds adj) [(i, 0) | i <- range (bounds adj)]
	  sm = mapM (adjId adj) (range (bounds adj))

{-
adj = [('a', "fbg"), ('b', ""), ('c', "a"), ('d', "f"),
       ('e', "d"), ('f', "e"), ('g', "cej"), ('h', "gi"),
       ('i', "h"), ('j', "kml"), ('k', ""), ('l', "gm"), ('m', "l")]
result = ["hi","almjcg","k","b","fde"]

test = scc' (array ('a','m') adj) == result
test2 = scc adj == result
-}
