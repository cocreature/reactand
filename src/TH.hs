{-# LANGUAGE TemplateHaskell #-}

module TH
  ( makeGEqInstance
  , makeGCompareInstance
  ) where

import Language.Haskell.TH

makeGEqInstance :: Name -> Q [Dec]
makeGEqInstance name =
  do info <- reify name
     case info of
       TyConI (DataD _ _name _tyvarbndrs cons _names) ->
         instanceD (cxt [])
                   (appT (conT (mkName "GEq"))
                         (conT name))
                   [funD (mkName "geq")
                         ((makeGEqClause <$> getConstNames cons) ++
                          [(clause [wildP,wildP]
                                   (normalB (conE (mkName "Nothing")))
                                   [])])] >>=
         \inst -> return [inst]
       _ -> error "not a data constructor"

makeGEqClause :: Name -> ClauseQ
makeGEqClause name =
  clause [conP name [],conP name []]
         (normalB (appE (conE (mkName "Just"))
                        (conE (mkName "Refl"))))
         []

getConstNames :: [Con] -> [Name]
getConstNames cons = map (\(ForallC _tyvarbndrs _cxt (NormalC name _stricTypes)) -> name) cons

makeGCompareInstance :: Name -> Q [Dec]
makeGCompareInstance name =
  do info <- reify name
     case info of
       TyConI (DataD _ _name _tyvarbndrs cons _names) ->
         instanceD (cxt [])
                   (appT (conT (mkName "GCompare"))
                         (conT name))
                   [funD (mkName "gcompare")
                         (makeGCompareClauses (getConstNames cons))] >>=
         \inst -> return [inst]
       _ -> error "not a data constructor"

makeGCompareClauses :: [Name] -> [ClauseQ]
makeGCompareClauses names = clauses =<< orderedNames
  where orderedNames = zip names [0::Int ..]
        orderingToGOrdering order =
          mkName ('G' : show order)
        clauses (name,order) =
          map (\(name',order') ->
                 clause [conP name [],conP name' []]
                        (normalB (conE (orderingToGOrdering (compare order order'))))
                        [])
              orderedNames
