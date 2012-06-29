import HUnit
import Syntax
import qualified Parser as P
import qualified Static as S

tcList = Tycon "List" (Kfun Star Star) 2
tyList = TAp (TCon tcList) (TVar (Tyvar "a" Star))

tests =
 ["test_anDataLhs" ~: -- data List a = Cons a (List a) | Nil
    let pList = (P.TyAp (P.TyCon "List") (P.TyVar "a"))
    in assertEqual "1" (tyList, tcList) (S.anDataLhs pList 2),
  "test_anConstr" ~:
    let constr = P.TyAp (P.TyAp (P.TyCon "Cons") (P.TyVar "a"))
                        (P.TyAp (P.TyCon "List") (P.TyVar "a"))
        typeArgs = [TVar (Tyvar "a" Star), tyList]
    in assertEqual "1" ("Cons", typeArgs, tyList)
                       (S.anConstr [tcList] tyList constr)
 ]

main = runTestTT $ test tests
