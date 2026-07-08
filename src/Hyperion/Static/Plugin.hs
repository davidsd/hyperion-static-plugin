module Hyperion.Static.Plugin
  ( plugin
  ) where

import Data.Char (isAlphaNum, isLower)
import Data.List (intercalate, nub)
import GHC.Data.FastString (fsLit)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.DynFlags
  ( DynFlags
  , xopt_set
  )
import GHC.Driver.Env.Types
  ( Hsc
  , HscEnv (..)
  )
import GHC.Driver.Plugins
  ( CommandLineOption
  , ParsedResult (..)
  , Plugin (..)
  , defaultPlugin
  , purePlugin
  )
import GHC.Hs (HsParsedModule (..))
import GHC.Hs.Extension (GhcPs)
import GHC.LanguageExtensions.Type qualified as LangExt
import GHC.Parser
  ( parseDeclaration
  , parseImport
  )
import GHC.Parser.Lexer
  ( ParseResult (..)
  , initParserState
  , unP
  )
import GHC.Types.SrcLoc
  ( GenLocated (..)
  , mkRealSrcLoc
  )
import GHC.Unit.Module.ModSummary (ModSummary (..))
import GHC.Utils.Outputable
  ( Outputable
  , ppr
  , showSDocUnsafe
  )
import GHC.Utils.Panic (panic)
import Language.Haskell.Syntax (HsModule (..))
import Language.Haskell.Syntax.Decls
  ( DerivDecl (..)
  , DerivClauseTys (..)
  , HsDecl (..)
  , HsDataDefn (..)
  , HsDerivingClause (..)
  , LHsDecl
  , TyClDecl (..)
  )
import Language.Haskell.Syntax.ImpExp (LImportDecl)
import Language.Haskell.Syntax.Type
  ( HsBndrVar (..)
  , HsTyVarBndr (..)
  , LHsQTyVars (..)
  )

plugin :: Plugin
plugin =
  defaultPlugin
    { driverPlugin = enableGeneratedSplices
    , parsedResultAction = rewriteStaticDeriving
    , pluginRecompile = purePlugin
    }

enableGeneratedSplices :: [CommandLineOption] -> HscEnv -> IO HscEnv
enableGeneratedSplices _opts hscEnv =
  pure hscEnv {hsc_dflags = enableTH (hsc_dflags hscEnv)}

rewriteStaticDeriving
  :: [CommandLineOption]
  -> ModSummary
  -> ParsedResult
  -> Hsc ParsedResult
rewriteStaticDeriving _opts summary result = do
  let summary' = enableTemplateHaskellInSummary summary
  parsedModule <- rewriteParsedModule summary' (parsedResultModule result)
  pure result {parsedResultModule = parsedModule}

enableTemplateHaskellInSummary :: ModSummary -> ModSummary
enableTemplateHaskellInSummary summary =
  summary {ms_hspp_opts = enableTH (ms_hspp_opts summary)}

enableTH :: DynFlags -> DynFlags
enableTH =
  (`xopt_set` LangExt.TemplateHaskellQuotes)
    . (`xopt_set` LangExt.TemplateHaskell)

rewriteParsedModule :: ModSummary -> HsParsedModule -> Hsc HsParsedModule
rewriteParsedModule summary hpm@HsParsedModule {hpm_module = L loc hsModule} = do
  hsModule' <- rewriteModule summary hsModule
  pure hpm {hpm_module = L loc hsModule'}

rewriteModule :: ModSummary -> HsModule GhcPs -> Hsc (HsModule GhcPs)
rewriteModule summary hsModule@HsModule {hsmodImports = imports, hsmodDecls = decls} = do
  rewrites <- traverse (rewriteDecl summary) decls
  let needsTH = any rrNeedsTH rewrites
  imports' <-
    generatedImports summary needsTH imports
  pure hsModule {hsmodImports = imports', hsmodDecls = concatMap rrDecls rewrites}

data RewriteResult = RewriteResult
  { rrNeedsTH :: Bool
  , rrDecls :: [LHsDecl GhcPs]
  }

rewriteDecl :: ModSummary -> LHsDecl GhcPs -> Hsc RewriteResult
rewriteDecl summary original@(L _ (DerivD _ derivDecl))
  | Just staticInstance <- parseStaticDeriving derivDecl =
      rewriteStaticInstance summary staticInstance
  | otherwise =
      pure (keepOriginal original)
rewriteDecl summary (L loc (TyClD ext tyClDecl))
  | (tyClDecl', staticInstances@(_ : _)) <- staticInstancesFromTyClDecl tyClDecl = do
      rewritten <- traverse (rewriteStaticInstance summary) staticInstances
      pure
        RewriteResult
          { rrNeedsTH = any rrNeedsTH rewritten
          , rrDecls = L loc (TyClD ext tyClDecl') : concatMap rrDecls rewritten
          }
rewriteDecl _ original = pure (keepOriginal original)

keepOriginal :: LHsDecl GhcPs -> RewriteResult
keepOriginal original =
  RewriteResult
    { rrNeedsTH = False
    , rrDecls = [original]
    }

data StaticInstance = StaticInstance
  { siContext :: [String]
  , siHead :: String
  }

data ClassifiedContext = ClassifiedContext
  { ccExplicitStaticConstraints :: [String]
  , ccExplicitOtherConstraints :: [String]
  , ccDictionaryConstraints :: [String]
  , ccImplicitStaticPayloads :: [String]
  }

parseStaticDeriving :: DerivDecl GhcPs -> Maybe StaticInstance
parseStaticDeriving derivDecl =
  let instanceType = compactSpaces (render (deriv_type derivDecl))
      (context, headType) = splitContext instanceType
   in if isStaticInstanceType headType
        then
          Just
            StaticInstance
              { siContext = context
              , siHead = headType
              }
        else Nothing

isStaticInstanceType :: String -> Bool
isStaticInstanceType instanceType =
  case dropOuterParens instanceType of
    'S' : 't' : 'a' : 't' : 'i' : 'c' : rest -> startsArg rest
    qualified
      | Just rest <- stripPrefixString "Hyperion.Static.Static" qualified ->
          startsArg rest
    _ -> False
  where
    startsArg (' ' : _) = True
    startsArg ('(' : _) = True
    startsArg _ = False

parseGeneratedInstance :: ModSummary -> StaticInstance -> Hsc (LHsDecl GhcPs)
parseGeneratedInstance summary staticInstance =
  case unP parseDeclaration parserState of
    POk _ declaration -> pure declaration
    PFailed _ ->
      panic $
        "hyperion-static-plugin: failed to parse generated instance:\n"
          <> generatedInstance
  where
    generatedInstance =
      unlines
        [ "instance " <> instanceContext <> siHead staticInstance <> " where"
        , "  closureDict = " <> closureDictBody (ccDictionaryConstraints classifiedContext)
        ]
    classifiedContext = classifyContext (siContext staticInstance)
    instanceContext =
      case generatedContext staticInstance classifiedContext of
        [] -> ""
        constraints -> "(" <> intercalate ", " constraints <> ") => "
    parserState =
      initParserState
        (initParserOpts (ms_hspp_opts summary))
        (stringToStringBuffer generatedInstance)
        (mkRealSrcLoc (fsLit "<hyperion-static-plugin>") 1 1)

rewriteStaticInstance :: ModSummary -> StaticInstance -> Hsc RewriteResult
rewriteStaticInstance summary staticInstance
  | null (siContext staticInstance)
  , not (null (typeVariables [staticConstraintPayload (siHead staticInstance)])) = do
      declaration <- parseGeneratedTHSplice summary staticInstance
      pure
        RewriteResult
          { rrNeedsTH = True
          , rrDecls = [declaration]
          }
  | otherwise = do
      declaration <- parseGeneratedInstance summary staticInstance
      pure
        RewriteResult
          { rrNeedsTH = False
          , rrDecls = [declaration]
          }

parseGeneratedTHSplice :: ModSummary -> StaticInstance -> Hsc (LHsDecl GhcPs)
parseGeneratedTHSplice summary staticInstance =
  case unP parseDeclaration parserState of
    POk _ declaration -> pure declaration
    PFailed _ ->
      panic $
        "hyperion-static-plugin: failed to parse generated TH splice:\n"
          <> generatedSplice
  where
    target = staticConstraintPayload (siHead staticInstance)
    quantifiedTarget =
      case typeVariables [target] of
        [] -> target
        variables -> "forall " <> unwords variables <> ". " <> target
    generatedSplice =
      "$(deriveStatic [t| " <> quantifiedTarget <> " |])"
    parserState =
      initParserState
        (initParserOpts (ms_hspp_opts summary))
        (stringToStringBuffer generatedSplice)
        (mkRealSrcLoc (fsLit "<hyperion-static-plugin>") 1 1)

generatedImports :: ModSummary -> Bool -> [LImportDecl GhcPs] -> Hsc [LImportDecl GhcPs]
generatedImports summary needsTH imports = do
  thImports <-
    if needsTH
      then (: []) <$> parseGeneratedImport summary "import Hyperion.Static.Plugin.TH (deriveStatic)"
      else pure []
  pure (thImports <> imports)

parseGeneratedImport :: ModSummary -> String -> Hsc (LImportDecl GhcPs)
parseGeneratedImport summary generatedImport =
  case unP parseImport parserState of
    POk _ importDecl -> pure importDecl
    PFailed _ ->
      panic $
        "hyperion-static-plugin: failed to parse generated import:\n"
          <> generatedImport
  where
    parserState =
      initParserState
        (initParserOpts (ms_hspp_opts summary))
        (stringToStringBuffer generatedImport)
        (mkRealSrcLoc (fsLit "<hyperion-static-plugin>") 1 1)

staticInstancesFromTyClDecl :: TyClDecl GhcPs -> (TyClDecl GhcPs, [StaticInstance])
staticInstancesFromTyClDecl tyClDecl@DataDecl {tcdLName = typeName, tcdTyVars = tyVars, tcdDataDefn = dataDefn} =
  ( tyClDecl {tcdDataDefn = dataDefn'}
  , [ StaticInstance
        { siContext = []
        , siHead = "Static (" <> markerClass <> " " <> appliedType <> ")"
        }
    | markerClass <- markers
    ]
  )
  where
    (dataDefn', markers) = removeStaticMarkers dataDefn
    appliedType =
      case tyVarNames tyVars of
        [] -> render typeName
        vars -> "(" <> unwords (render typeName : vars) <> ")"
staticInstancesFromTyClDecl tyClDecl = (tyClDecl, [])

removeStaticMarkers :: HsDataDefn GhcPs -> (HsDataDefn GhcPs, [String])
removeStaticMarkers dataDefn@HsDataDefn {dd_derivs = derivingClauses} =
  (dataDefn {dd_derivs = clauses'}, markers)
  where
    rewrites = rewriteDerivingClause <$> derivingClauses
    clauses' = [clause | (Just clause, _) <- rewrites]
    markers = concat [clauseMarkers | (_, clauseMarkers) <- rewrites]

rewriteDerivingClause
  :: GenLocated l (HsDerivingClause GhcPs)
  -> (Maybe (GenLocated l (HsDerivingClause GhcPs)), [String])
rewriteDerivingClause (L loc clause@HsDerivingClause {deriv_clause_tys = L tysLoc clauseTys}) =
  case rewriteDerivClauseTys clauseTys of
    (Nothing, markers) -> (Nothing, markers)
    (Just clauseTys', markers) ->
      (Just (L loc clause {deriv_clause_tys = L tysLoc clauseTys'}), markers)

rewriteDerivClauseTys :: DerivClauseTys GhcPs -> (Maybe (DerivClauseTys GhcPs), [String])
rewriteDerivClauseTys original@(DctSingle _ sigType) =
  case staticMarker sigType of
    Just markerClass -> (Nothing, [markerClass])
    Nothing -> (Just original, [])
rewriteDerivClauseTys (DctMulti ext sigTypes) =
  (DctMulti ext <$> nonMarkerTypes, markers)
  where
    classified = classifyDerivType <$> sigTypes
    nonMarkerTypes =
      case [sigType | (sigType, Nothing) <- classified] of
        [] -> Nothing
        sigTypes' -> Just sigTypes'
    markers = [markerClass | (_, Just markerClass) <- classified]

classifyDerivType
  :: Outputable sigType
  => sigType
  -> (sigType, Maybe String)
classifyDerivType sigType =
  (sigType, staticMarker sigType)

staticMarker :: Outputable sigType => sigType -> Maybe String
staticMarker sigType =
  case dropOuterParens (compactSpaces (render sigType)) of
    'S' : 't' : 'a' : 't' : 'i' : 'c' : rest
      | startsArg rest -> Just (dropOuterParens (compactSpaces rest))
    qualified
      | Just rest <- stripPrefixString "Hyperion.Static.Static" qualified
      , startsArg rest ->
          Just (dropOuterParens (compactSpaces rest))
    _ -> Nothing
  where
    startsArg (' ' : _) = True
    startsArg ('(' : _) = True
    startsArg _ = False

tyVarNames :: LHsQTyVars GhcPs -> [String]
tyVarNames (HsQTvs {hsq_explicit = tyVars}) =
  concatMap tyVarName tyVars

tyVarName :: GenLocated l (HsTyVarBndr flag GhcPs) -> [String]
tyVarName (L _ HsTvb {tvb_var = HsBndrVar _ name}) = [render name]
tyVarName _ = []

render :: Outputable a => a -> String
render = showSDocUnsafe . ppr

compactSpaces :: String -> String
compactSpaces =
  unwords . words

splitContext :: String -> ([String], String)
splitContext instanceType =
  case splitTopLevelArrow (dropForall instanceType) of
    Nothing -> ([], instanceType)
    Just (context, headType) -> (splitConstraintTuple context, headType)

dropForall :: String -> String
dropForall input =
  case stripPrefixString "forall " input of
    Just rest ->
      case break (== '.') rest of
        (_, '.' : afterDot) -> compactSpaces afterDot
        _ -> input
    Nothing -> input

splitTopLevelArrow :: String -> Maybe (String, String)
splitTopLevelArrow = go 0 ""
  where
    go _ _ [] = Nothing
    go depth acc ('=' : '>' : rest)
      | depth == 0 = Just (compactSpaces (reverse acc), compactSpaces rest)
    go depth acc (char : rest) =
      go (updateDepth char depth) (char : acc) rest

splitConstraintTuple :: String -> [String]
splitConstraintTuple =
  filter (not . null)
    . fmap (compactSpaces . dropOuterParens)
    . splitTopLevelCommas
    . dropOuterParens

splitTopLevelCommas :: String -> [String]
splitTopLevelCommas = go 0 "" []
  where
    go _ acc chunks [] = reverse (reverse acc : chunks)
    go depth acc chunks (',' : rest)
      | depth == 0 = go depth "" (reverse acc : chunks) rest
    go depth acc chunks (char : rest) =
      go (updateDepth char depth) (char : acc) chunks rest

updateDepth :: Char -> Int -> Int
updateDepth '(' depth = depth + 1
updateDepth '[' depth = depth + 1
updateDepth '{' depth = depth + 1
updateDepth ')' depth = max 0 (depth - 1)
updateDepth ']' depth = max 0 (depth - 1)
updateDepth '}' depth = max 0 (depth - 1)
updateDepth _ depth = depth

classifyContext :: [String] -> ClassifiedContext
classifyContext =
  foldr addConstraint (ClassifiedContext [] [] [] [])
  where
    addConstraint constraint classifiedContext
      | isStaticInstanceType constraint =
          classifiedContext
            { ccExplicitStaticConstraints = constraint : ccExplicitStaticConstraints classifiedContext
            , ccDictionaryConstraints = staticConstraintPayload constraint : ccDictionaryConstraints classifiedContext
            }
      | isTypeableConstraint constraint =
          classifiedContext
            { ccExplicitOtherConstraints = constraint : ccExplicitOtherConstraints classifiedContext
            }
      | otherwise =
          classifiedContext
            { ccDictionaryConstraints = constraint : ccDictionaryConstraints classifiedContext
            , ccImplicitStaticPayloads = constraint : ccImplicitStaticPayloads classifiedContext
            }

generatedContext :: StaticInstance -> ClassifiedContext -> [String]
generatedContext staticInstance classifiedContext =
  typeableConstraints
    <> ccExplicitOtherConstraints classifiedContext
    <> ccExplicitStaticConstraints classifiedContext
    <> implicitStaticConstraints
  where
    explicitConstraints =
      ccExplicitOtherConstraints classifiedContext
        <> ccExplicitStaticConstraints classifiedContext
    typeableConstraints =
      ("Typeable " <>)
        <$> filter
          (`notElem` explicitTypeableVariables explicitConstraints)
          (typeVariables [siHead staticInstance])
    implicitStaticConstraints =
      ("Static (" <>)
        . (<> ")")
        <$> ccImplicitStaticPayloads classifiedContext

isTypeableConstraint :: String -> Bool
isTypeableConstraint constraint =
  case dropOuterParens constraint of
    'T' : 'y' : 'p' : 'e' : 'a' : 'b' : 'l' : 'e' : rest -> startsArg rest
    qualified
      | Just rest <- stripPrefixString "Type.Reflection.Typeable" qualified ->
          startsArg rest
      | Just rest <- stripPrefixString "Data.Typeable.Typeable" qualified ->
          startsArg rest
    _ -> False
  where
    startsArg (' ' : _) = True
    startsArg ('(' : _) = True
    startsArg _ = False

staticConstraintPayload :: String -> String
staticConstraintPayload constraint =
  dropOuterParens $
    case dropOuterParens constraint of
      'S' : 't' : 'a' : 't' : 'i' : 'c' : rest -> dropOuterParens (compactSpaces rest)
      qualified
        | Just rest <- stripPrefixString "Hyperion.Static.Static" qualified ->
            dropOuterParens (compactSpaces rest)
      other -> other

explicitTypeableVariables :: [String] -> [String]
explicitTypeableVariables =
  concatMap typeableVariable

typeableVariable :: String -> [String]
typeableVariable constraint =
  case dropOuterParens constraint of
    'T' : 'y' : 'p' : 'e' : 'a' : 'b' : 'l' : 'e' : rest -> typeVariables [rest]
    qualified
      | Just rest <- stripPrefixString "Type.Reflection.Typeable" qualified ->
          typeVariables [rest]
      | Just rest <- stripPrefixString "Data.Typeable.Typeable" qualified ->
          typeVariables [rest]
    _ -> []

typeVariables :: [String] -> [String]
typeVariables =
  nub . concatMap (filter isTypeVariable . tokens)

isTypeVariable :: String -> Bool
isTypeVariable token =
  case token of
    first : _ -> isLower first && token `notElem` reservedTypeNames
    [] -> False

reservedTypeNames :: [String]
reservedTypeNames =
  [ "forall"
  , "family"
  , "role"
  , "type"
  , "data"
  , "newtype"
  , "class"
  , "instance"
  ]

tokens :: String -> [String]
tokens [] = []
tokens (char : rest)
  | isIdentChar char =
      let (tokenRest, afterToken) = span isIdentChar rest
       in (char : tokenRest) : tokens afterToken
  | otherwise = tokens rest

isIdentChar :: Char -> Bool
isIdentChar char =
  isAlphaNum char || char == '_' || char == '\''

closureDictBody :: [String] -> String
closureDictBody [] = "static Dict"
closureDictBody context =
  "static (\\"
    <> unwords (replicate (length context) "Dict")
    <> " -> Dict)"
    <> concatMap closureDictArg context

closureDictArg :: String -> String
closureDictArg context =
  " `cAp` (closureDict :: Closure (Dict (" <> context <> ")))"

dropOuterParens :: String -> String
dropOuterParens input =
  case input of
    '(' : rest
      | Just inner <- stripLastParen rest -> compactSpaces inner
    _ -> input

stripLastParen :: String -> Maybe String
stripLastParen input =
  case reverse input of
    ')' : rest -> Just (reverse rest)
    _ -> Nothing

stripPrefixString :: String -> String -> Maybe String
stripPrefixString [] ys = Just ys
stripPrefixString (_ : _) [] = Nothing
stripPrefixString (x : xs) (y : ys)
  | x == y = stripPrefixString xs ys
  | otherwise = Nothing
