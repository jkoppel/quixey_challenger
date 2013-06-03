--Move context stuff

{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances, UndecidableInstances, IncoherentInstances, FlexibleContexts, TemplateHaskell #-}

module KureCong (
       GenericSubexp,
       childLr,
       gCon,
       Context,
       initialContext,
       Sizable,
       size
 ) where

import Prelude

import Control.Applicative (Applicative, (<*>))

import Language.Haskell.TH ( mkName )
import Language.Haskell.TH.Syntax

import Language.KURE (AbsolutePath, extendAbsPath, Generic, MonadCatch, Node, rootAbsPath)
import Language.KURE.Injection



--------------------------------------------------------------------------------

type Context = AbsolutePath

(@@) :: Context -> Int -> Context
(@@) = flip extendAbsPath

initialContext :: Context
initialContext = rootAbsPath
--------------------------------------------------------------------------------

data GenericSubexp g = GenericMaybe (Maybe g)
                     | GenericList [g]
                     | GenericSubexp g
                     | GenericString String
                     | GenericChar Char
                     | GenericBool Bool 
                     | GenericInt Int
                     | GenericDouble Double
                     | GenericInteger Integer

--This should already exist. Data.Cardinality seems it should fit the bill, but
--is overall poorly designed, and also generalized to support lazy cardinalities
class Sizable a where
  size :: a -> Int

instance Sizable g => Sizable (GenericSubexp g) where
  size (GenericMaybe (Just g)) = size g
  size (GenericMaybe Nothing)  = 0
  size (GenericList gs)        = sum $ map size gs
  size (GenericSubexp g)       = size g
  size (GenericString s)       = 0
  size (GenericChar c)         = 0
  size (GenericBool b)         = 0
  size (GenericInt n)          = 0
  size (GenericDouble n)       = 0
  size (GenericInteger n)      = 0

instance Node a => Sizable a where
  size _ = 1

instance Sizable (Maybe a) where
  size Nothing = 0
  size (Just _) = 1

instance Sizable [a] where
  size = length

instance Sizable Int where size _ = 0
instance Sizable Integer where size _ = 0
instance Sizable Char where size _ = 0
instance Sizable Double where size _ = 0
instance Sizable String where size _ = 0
instance Sizable Bool where size _ = 0

instance Injection a g => Injection a (GenericSubexp g) where
  inject = GenericSubexp . inject

  retract (GenericSubexp x) = retract x
  retract _                 = Nothing


instance Injection a g => Injection (Maybe a) (GenericSubexp g) where
  inject Nothing  = GenericMaybe Nothing
  inject (Just x) = GenericMaybe $ Just $ inject x

  retract (GenericMaybe Nothing)  = Just Nothing
  retract (GenericMaybe (Just x)) = case retract x of
                                      Just y  -> Just $ Just y
                                      Nothing -> Nothing
  retract _                       = Nothing

instance Injection a g => Injection [a] (GenericSubexp g) where
  inject = GenericList . (map inject)

  retract (GenericList xs) = mapM retract xs
  retract _                = Nothing

instance Injection Bool (GenericSubexp g) where
  inject = GenericBool

  retract (GenericBool b) = Just b
  retract _               = Nothing

instance Injection Int (GenericSubexp g) where
  inject = GenericInt

  retract (GenericInt n)  = Just n
  retract _               = Nothing

instance Injection Char (GenericSubexp g) where
  inject = GenericChar

  retract (GenericChar c) = Just c
  retract _               = Nothing

instance Injection String (GenericSubexp g) where
  inject = GenericString

  retract (GenericString s) = Just s
  retract _                 = Nothing

instance Injection Double (GenericSubexp g) where
  inject = GenericDouble

  retract (GenericDouble s) = Just s
  retract _                 = Nothing

instance Injection Integer (GenericSubexp g) where
  inject = GenericInteger

  retract (GenericInteger n) = Just n
  retract _                  = Nothing

-- Turns a n-ary constructor into a "generic" constructor, with the proprety
--    (gConN f) [inject x1,... , inject xN] == f x1 ... xN
--
-- For example, $(gCon 2) outputs approximately the following:
--
--gCon2 :: (Injection a1 (GenericSubexp g), Injection a2 (GenericSubexp g), MonadCatch m, Applicative m) =>
--                    (a1 -> a2 -> b) -> [GenericSubexp g] -> m b
--gCon2 f = \xs -> case xs of
--                      [x1, x2] -> return f <*> (retractM x1) <*> (retractM x2)
--                      _         -> fail "gCon2 failed"
gCon :: Int -> [Dec]
gCon n = [SigD nfnam t,
          ValD (VarP nfnam) (NormalB e) []]
     where
        nf = mkName "f"
        nxs = mkName "xs"
        nams = take n $ map mkName $ zipWith (++) (repeat "c") (map show $ iterate (+1) 0)
        inner = foldl (\e nam -> InfixE (Just e) (VarE '(<*>)) (Just (AppE (VarE 'retractM) (VarE nam))))
                      (AppE (VarE 'return) (VarE nf))
                      nams
        fnam = "gCon" ++ (show n)
        nfnam = mkName fnam
        e = LamE [VarP nf] $
              LamE [VarP nxs] $ CaseE (VarE nxs) [
                         Match (ListP $ map VarP nams) (NormalB inner) [],
                         Match WildP (NormalB (AppE (VarE 'fail) (LitE $ StringL $ fnam ++ " failed"))) []
                   ]
        nas = take n $ map mkName $ zipWith (++) (repeat "a") (map show $ iterate (+1) 0)
        nb = mkName "b"
        ng = mkName "g"
        nm = mkName "m"
        t = ForallT ([PlainTV n | n <- nas] ++ [PlainTV ng, PlainTV nm, PlainTV nb])
                    ([ClassP ''Injection [VarT n, AppT (ConT ''GenericSubexp) (VarT ng)] | n <- nas] ++ [ClassP ''MonadCatch [VarT nm], ClassP ''Applicative [VarT nm]])
                    (AppT (AppT ArrowT (foldr (\a b -> AppT (AppT ArrowT (VarT a)) b) (VarT nb) nas)) $
                      AppT (AppT ArrowT (AppT ListT (AppT (ConT ''GenericSubexp) (VarT ng)))) $
                        AppT (VarT nm) (VarT nb))


replace :: [a] -> Int -> a -> [a]
replace [] _ _ = error "replace: n out of bound"
replace (x:xs) 0 x' = x' : xs
replace (x:xs) n x' = x : replace xs (n-1) x'

childLr_rec :: (Monad m, Sizable g) => Int -> [GenericSubexp g] -> ([GenericSubexp g] -> m a) -> [GenericSubexp g] -> m (g, g -> m a)
childLr_rec n [] r as = fail "childLr_rec: Indexing into empty list"
childLr_rec n (x:xs) r as = if n < size x then
                               case x of
                                    GenericSubexp x'       -> return (x', \e -> r (as ++ [GenericSubexp e] ++ xs))
                                    GenericList x'         -> return (x' !! n, \e -> r (as ++ [GenericList $ replace x' n e] ++ xs))
                                    GenericMaybe (Just x') -> return (x', \e -> r (as ++ [GenericMaybe $ Just e] ++ xs))
                                    GenericMaybe Nothing   -> error "childLr_rec: Saw 'GenericMaybe Nothing' for size in bounds" --impossible
                            else
                                childLr_rec (n - (size x)) xs r (as ++ [x])

childLr :: (MonadCatch m, Node a, Node g, g ~ Generic a) => Int -> Context -> [GenericSubexp g] -> ([GenericSubexp g] -> m a) -> m ((Context,g), g -> m a)
childLr n c es r = do (b, f) <- childLr_rec n es r []
                      return ((c @@ n, b), f)
