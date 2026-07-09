{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Hyperion.Static.Plugin.TH
  ( deriveStatic
  ) where

import Data.Constraint (Dict (Dict))
import Data.List (nub)
import Data.Monoid (Endo (Endo, appEndo))
import Data.Typeable (Typeable)
import GHC.TypeNats (KnownNat, Nat)
import Hyperion.Static (Closure, Static (closureDict), cAp)
import Language.Haskell.TH
  ( Body (NormalB)
  , Clause (Clause)
  , Dec (FunD, InstanceD)
  , Exp (ConE, InfixE, LamE, SigE, StaticE, VarE)
  , InstanceDec
  , Name
  , Pat (ConP)
  , Q
  , Type (..)
  , TyVarBndr (KindedTV)
  , nameBase
  , reifyInstances
  )

deriveStatic :: Q Type -> Q [Dec]
deriveStatic typeQ = do
  target <- typeQ
  let typeableExclusions = natKindVariables target
  let targetType = dropForall target
  (className, args) <- classApplication targetType
  instances <- reifyInstances className args
  case instances of
    [instanceDec] -> pure [mkInstanceWithTypeableExclusions typeableExclusions 'closureDict ''Static instanceDec]
    [] -> fail $ "deriveStatic: no instance found for " <> show targetType
    _ -> fail $ "deriveStatic: multiple instances found for " <> show targetType

mkInstanceWithTypeableExclusions :: [String] -> Name -> Name -> InstanceDec -> InstanceDec
mkInstanceWithTypeableExclusions typeableExclusions getClosureDictName staticClassName instanceDec = case instanceDec of
  InstanceD maybeOverlap oldCxt oldType _ ->
    let
      dictTypeName = ''Dict
      dictType = ConT dictTypeName
      dictValueName = 'Dict
      dictValue = ConE dictValueName
      dictPat = ConP dictValueName [] []
      closureClassName = ''Closure
      closureClass = ConT closureClassName
      capName = 'cAp
      capValue = VarE capName
      getClosureDict = VarE getClosureDictName
      addClassF = AppT (ConT staticClassName)
      addTypeableF = AppT (ConT ''Typeable)
      newType = addClassF oldType
      newStaticCxt = addClassF <$> oldCxt
      oldTypeVars = nub ((oldType : oldCxt) >>= findAllTypeVars)
      newTypeableCxt =
        (addTypeableF . VarT)
          <$> filter
            ((`notElem` typeableExclusions) . nameBase)
            oldTypeVars
      newKnownNatCxt =
        AppT (ConT ''KnownNat) . VarT
          <$> filter
            ((`elem` typeableExclusions) . nameBase)
            oldTypeVars
      newCxt = applyConstraintReplacements staticClassName (newKnownNatCxt ++ newTypeableCxt ++ newStaticCxt)
      mkTypeSig cxt = AppT closureClass (AppT dictType cxt)
      mkArgExp cxt = SigE getClosureDict (mkTypeSig cxt)
      addArg x cxt = InfixE (Just x) capValue (Just (mkArgExp cxt))
      funcPart = case length oldCxt of
        0 -> dictValue
        n -> LamE (replicate n dictPat) dictValue
      body = NormalB (foldl addArg (StaticE funcPart) oldCxt)
      clause = Clause [] body []
      funClause = FunD getClosureDictName [clause]
    in
      InstanceD maybeOverlap newCxt newType [funClause]
  _ -> error "mkInstance: Not an instance"

natKindVariables :: Type -> [String]
natKindVariables = \case
  ForallT tyVarBndrs _ _ -> concatMap natKindVariable tyVarBndrs
  ForallVisT tyVarBndrs _ -> concatMap natKindVariable tyVarBndrs
  _ -> []

natKindVariable :: TyVarBndr flag -> [String]
natKindVariable = \case
  KindedTV name _ kind
    | isNatKind kind -> [nameBase name]
  _ -> []

isNatKind :: Type -> Bool
isNatKind = \case
  ConT name -> name == ''Nat || nameBase name == "Nat"
  SigT ty _ -> isNatKind ty
  ParensT ty -> isNatKind ty
  _ -> False

applyConstraintReplacements :: Name -> [Type] -> [Type]
applyConstraintReplacements staticClassName =
  applyAll
    [ replaceStaticKnownNatConstraints staticClassName
    ]

applyAll :: [a -> a] -> a -> a
applyAll replacements value =
  foldl (flip ($)) value replacements

replaceStaticKnownNatConstraints :: Name -> [Type] -> [Type]
replaceStaticKnownNatConstraints staticClassName constraints =
  filter (not . isTypeableKnownNatVariable knownNatVariables) $
    replaceStaticKnownNatConstraint staticClassName <$> constraints
  where
    knownNatVariables =
      [ name
      | AppT (ConT knownNatName) (VarT name) <- replaceStaticKnownNatConstraint staticClassName <$> constraints
      , knownNatName == ''KnownNat
      ]

replaceStaticKnownNatConstraint :: Name -> Type -> Type
replaceStaticKnownNatConstraint staticClassName = \case
  AppT (ConT staticName) knownNatConstraint@(AppT (ConT knownNatName) _)
    | staticName == staticClassName
    , knownNatName == ''KnownNat ->
        knownNatConstraint
  constraint -> constraint

isTypeableKnownNatVariable :: [Name] -> Type -> Bool
isTypeableKnownNatVariable knownNatVariables = \case
  AppT (ConT typeableName) (VarT name) ->
    typeableName == ''Typeable && name `elem` knownNatVariables
  _ -> False

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

findAllTypeVars :: Type -> [Name]
findAllTypeVars x = appEndo (go x) [] where
  go = \case
    ForallT{} -> error "Don't know how do deal with Foralls in types"
    AppT t1 t2 -> go t1 <> go t2
    SigT t _ -> go t
    VarT name -> Endo (name :)
    InfixT t1 _ t2 -> go t1 <> go t2
    UInfixT t1 _ t2 -> go t1 <> go t2
    ParensT t -> go t
    _ -> mempty
