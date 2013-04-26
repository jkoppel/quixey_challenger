{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies, ImplicitParams #-}

module Boilerplate (
      kureYourBoilerplate,
      gCon1,
      gCon2,
      gCon3,
      gCon4,
      gCon5,
      gCon6,
      gCon7,
      gCon8,
      gCon9,
      gCon10,
    ) where



import Language.KURE ( Node, Generic, Walker, MonadCatch, AbsolutePath, numChildren, childL, lens, translate )
import Language.KURE.Injection ( Injection, inject, retract)
import Language.KURE.Utilities ( childLgeneric )

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Control.Applicative ( Applicative )

import KureCong ( gCon, childLr, size )

$(return $ concat $ map gCon (take 10 [1..]))

kureYourBoilerplate :: (?excl :: [Name]) => Name -> Q [Dec]
kureYourBoilerplate gname = do
  info <- reify gname
  let (gcons, gtypes) = extractConstructorsUnary info
  
  ast_types <- sequence $ map (reify . typeName) gtypes

  return $ (concatMap (generateConBoilerplate gname) (zip3 gcons gtypes ast_types)) ++
           [generateGenericNode gname gcons,
            generateGenericWalker gname gcons
            ]

generateGenericNode :: Name -> [Name] -> Dec
generateGenericNode gname gcons = 
  InstanceD [] (AppT (ConT ''Node) (ConT gname)) $ [
            TySynInstD ''Generic [ConT gname] (ConT gname)

            ] ++ [FunD 'numChildren (map makeNumChildren gcons)]
  where
    nm = mkName "c"
    makeNumChildren con = Clause [ConP con [VarP nm]] (NormalB (AppE (VarE 'numChildren) (VarE nm))) []

generateGenericWalker :: Name -> [Name] -> Dec
generateGenericWalker gname gcons =
  InstanceD [ClassP ''MonadCatch [VarT m], ClassP ''Applicative [VarT m]] (foldl AppT (ConT ''Walker) [ConT ''AbsolutePath, VarT m, ConT gname]) $ [
    FunD 'childL [Clause [VarP n]
                         (NormalB ((AppE (VarE 'lens) (AppE (VarE 'translate)
                                      (LamE [(VarP ctx), (VarP g)] $ CaseE (VarE g) (map makeCase gcons))))))
                         [] ]]
  where
    n = mkName "n"
    m = mkName "m"
    ctx = mkName "ctx"
    g = mkName "g"
    c = mkName "c"

    makeCase con = Match (ConP con [VarP c])
                         (NormalB (foldl AppE (VarE 'childLgeneric) [VarE n, VarE ctx, VarE c]))
                         []

generateConBoilerplate :: (?excl :: [Name]) => Name -> (Name, Type, Info) -> [Dec]
generateConBoilerplate gname (con, typ, inf) = [generateInjection gname con typ,
                                                generateNode gname typ inf,
                                                generateWalker typ inf]

generateInjection :: Name -> Name -> Type -> Dec
generateInjection gname con typ =
  InstanceD [] (AppT (AppT (ConT ''Injection) (ConT tname)) (ConT gname)) $ [
    ValD (VarP 'inject) (NormalB (ConE con)) [],

    FunD 'retract [Clause [ConP con [VarP c]] (NormalB (AppE (ConE 'Just) (VarE c))) [],
                   Clause [WildP] (NormalB (ConE 'Nothing)) []]
    ]

  where
    tname = typeName typ
    c = mkName "c"


generateNode :: (?excl :: [Name]) => Name -> Type -> Info -> Dec
generateNode gname typ inf =
  InstanceD [] (AppT (ConT ''Node) (ConT tname)) $ [
            TySynInstD ''Generic [ConT tname] (ConT gname)

            ] ++ [FunD 'numChildren $ map makeNumChildren (zip cons targs)]
  where
    nms = genVars "c"
    tname = typeName typ
    (cons, targs) = extractConstructors inf

    makeNumChildren (con, ts) = Clause [ConP con (take (length ts) $ map VarP nms)] (NormalB $ genNumChildren con ts) []
    genNumChildren con ts | elem con ?excl = LitE (IntegerL 0)
    genNumChildren con ts | otherwise = foldl (\s e -> AppE (AppE (VarE '(+)) s) e) (LitE (IntegerL 0)) (map mkTerm (zip ts nms))
    mkTerm (t, n) = AppE (VarE 'size) (VarE n)

generateWalker :: (?excl :: [Name]) => Type -> Info -> Dec
generateWalker typ inf =
  InstanceD [ClassP ''MonadCatch [VarT m], ClassP ''Applicative [VarT m]] (AppT (AppT (AppT (ConT ''Walker) (ConT ''AbsolutePath)) (VarT m)) (ConT tname)) $ [
                    FunD 'childL [Clause [VarP n]
                                         (NormalB
                                           (AppE (VarE 'lens) (AppE (VarE 'translate)
                                              (LamE [VarP ctx, VarP e]
                                                 (CondE (InfixE (Just (VarE n)) (VarE '(>=)) (Just (AppE (VarE 'numChildren) (VarE e))))
                                                          (AppE (VarE 'fail) (LitE (StringL failMsg)))
                                                          (CaseE (VarE e)
                                                              ((map makeMatch (filter (not.null.snd) (zip cons targs))) ++
                                                               [Match WildP (NormalB $ AppE (VarE 'fail) (LitE (StringL $ "childL for "++(show tname)++ " hit undefined")))  []])))))))
                                  []]
      ]
  where
    [m,n,ctx,e] = map mkName ["m","n","ctx","e"]
    tname = typeName typ
    failMsg = "childL for " ++ (show tname) ++ ": n out of bounds"
    cts = uncurry zip $ extractConstructors inf
    (cons, targs) = unzip $ filter (not . flip elem ?excl . fst) $ cts

    makeMatch (con, ts) = Match (ConP con (map VarP tvs)) (NormalB
                                (foldl AppE (VarE 'childLr) [VarE n,
                                                             VarE ctx,
                                                             ListE (map (AppE (VarE 'inject) . VarE) tvs),
                                                             (AppE (VarE gc) (ConE con))]))
                                []
                          where
                            tvs = take (length ts) $ genVars "x"
                            gc = mkName ("gCon" ++ (show $ length ts))

typeName :: Type -> Name
typeName typ = case typ of
                 ConT nm -> nm

extractConstructorsUnary :: Info -> ([Name], [Type])
extractConstructorsUnary info =
  let (ns, ts) = extractConstructors info in
  (ns, map head ts)

extractConstructors :: Info -> ([Name], [[Type]])
extractConstructors info =
  case info of
    TyConI (DataD _ _ _ cons _) -> unzip $
                                      [ (con, map snd strictTys) | (NormalC con strictTys) <- cons] ++
                                      [ (con, map (\(_,_,x) -> x) vsts) | (RecC con vsts) <- cons]


genVars :: String -> [Name]
genVars pre = map (mkName . (pre++) . show) $ [0..]
