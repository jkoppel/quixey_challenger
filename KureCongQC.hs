{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances, TemplateHaskell, FlexibleContexts, FunctionalDependencies, UndecidableInstances #-}

module KureCongQC (
      ) where

import Language.KURE.Injection ( inject )
import Language.KURE.Utilities ( KureMonad (..) )

import Test.QuickCheck.Function ( apply, Fun (..))

import KureCong ( gCon )

$(return $ gCon 1)
$(return $ gCon 2)
$(return $ gCon 3)

class Applicable a b | b -> a where
  applyN :: a -> b

instance Applicable String String where
  applyN = id

instance Applicable a b => Applicable (Fun c a) (c -> b) where
  applyN f x = applyN (apply f x)

prop_gCon1 f n = ((return $ applyN f (n :: Int)) :: KureMonad String) ==
                   (gCon1 (apply f) [inject n])

prop_gCon2 f n1 n2 = ((return $ (applyN f (n1 :: Int)) (n2 :: Int)) :: KureMonad String) ==
                       (gCon2 (applyN f) [inject n1, inject n2])

prop_gCon3 f n1 n2 n3 = ((return $ applyN f (n1 :: Int) (n2 :: Int) (n3 :: Int)) :: KureMonad String) ==
                          (gCon3 (applyN f) [inject n1, inject n2, inject n3])