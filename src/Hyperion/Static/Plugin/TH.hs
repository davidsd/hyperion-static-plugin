{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Hyperion.Static.Plugin.TH
  ( deriveStatic
  ) where

import Hyperion.Static (Static (closureDict))
import Hyperion.Static.TH (mkInstance)
import Language.Haskell.TH
  ( Dec
  , Name
  , Q
  , Type (..)
  , reifyInstances
  )

deriveStatic :: Q Type -> Q [Dec]
deriveStatic typeQ = do
  target <- typeQ
  let targetType = dropForall target
  (className, args) <- classApplication targetType
  instances <- reifyInstances className args
  case instances of
    [instanceDec] -> pure [mkInstance 'closureDict ''Static instanceDec]
    [] -> fail $ "deriveStatic: no instance found for " <> show targetType
    _ -> fail $ "deriveStatic: multiple instances found for " <> show targetType

dropForall :: Type -> Type
dropForall = \case
  ForallT _ _ body -> body
  ForallVisT _ body -> body
  other -> other

classApplication :: Type -> Q (Name, [Type])
classApplication =
  go []
  where
    go args = \case
      AppT f x -> go (x : args) f
      ConT name -> pure (name, args)
      PromotedT name -> pure (name, args)
      SigT ty _ -> go args ty
      ParensT ty -> go args ty
      other -> fail $ "deriveStatic: expected a class application, got " <> show other
