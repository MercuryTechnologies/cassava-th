{-# language TemplateHaskell #-}

-- | This module provides @TemplateHaskell@ support for the @cassava@
-- library, so you can avoid deriving 'Generic' and the compilation
-- overhead that brings.
module Data.Csv.TH
  ( deriveToNamedRecord,
    deriveToRecord,
    deriveDefaultOrdered,
    deriveNamedRecord,
    deriveFromNamedRecord,
    deriveFromRecord,
    deriveToAndFromRecord,
    deriveToNamedRecordAndDefaultOrdered,
    deriveToAndFromNamedRecordAndDefaultOrdered,
    CsvOptions,
    csvDefaultOptions,
  )
where

import Data.Traversable
import Control.Monad
import Control.Monad.Fail
import Data.Csv qualified as Csv
import Data.Vector qualified as V
import Language.Haskell.TH

-- | Cassava uses the same name as Aeson, which is used much more frequently.
-- This alias makes it a bit easier to refer to it.
type CsvOptions = Csv.Options

-- | Cassava uses the same name as Aeson, which is used more frequently. To
-- avoid qualified names and lots of clashes, we export an alias here.
csvDefaultOptions :: Csv.Options
csvDefaultOptions = Csv.defaultOptions

-- | A helper for the common case of deriving both 'Csv.ToNamedRecord' and
-- 'Csv.DefaultOrdered', sharing the same options.
deriveToNamedRecordAndDefaultOrdered :: Csv.Options -> Name -> DecsQ
deriveToNamedRecordAndDefaultOrdered opts name =
  deriveToNamedRecord opts name <> deriveDefaultOrdered opts name

-- | A helper for the common case of deriving 'Csv.ToNamedRecord',
-- 'Csv.FromNamedRecord', and 'Csv.DefaultOrdered', sharing the same options.
deriveToAndFromNamedRecordAndDefaultOrdered :: Csv.Options -> Name -> DecsQ
deriveToAndFromNamedRecordAndDefaultOrdered opts name =
  deriveToNamedRecord opts name
    <> deriveDefaultOrdered opts name
    <> deriveFromNamedRecord opts name

-- | Derives a 'Csv.ToNamedRecord' instance for a given type.
deriveToNamedRecord :: Csv.Options -> Name -> DecsQ
deriveToNamedRecord opts typName = do
  info <- reify typName
  con <-
    case info of
      TyConI dec ->
        case dec of
          NewtypeD _cxt _name _tyvars _mkind con _derivs ->
            pure con
          DataD _cxt _name _tyvars _mkind cons _derivs ->
            case cons of
              [con] ->
                pure con
              _ ->
                fail $
                  concat
                    [ "Expected "
                    , show typName
                    , " to be a record with a single constructor."
                    ]
          _ ->
            fail $
              concat
                [ "Expected "
                , show typName
                , " to be a record with a single constructor."
                ]
      _ ->
        fail $
          concat
            [ "Expected"
            , show typName
            , " to be a type name of a single record constructor."
            ]
  (constrPatternMatch, matchedVariables) <-
    case con of
      RecC constrName fields -> do
        namesWithPatterns <- for fields \(fieldName, _, _) -> do
          FieldName fieldName <$> newName (nameBase fieldName)

        pure (ConP constrName [] (map (VarP . fieldPatternVariable) namesWithPatterns), namesWithPatterns)
      _ ->
        fail $
          concat
            [ "Expected "
            , show typName
            , "to be a record with a single constructor."
            ]

  let fieldExpr fieldName = do
        let modifiedFieldName =
              Csv.fieldLabelModifier opts (nameBase $ fieldOriginalName fieldName)

        [e|
          ( $(litE $ StringL modifiedFieldName)
          , Csv.toField $(varE (fieldPatternVariable fieldName))
          )
          |]

  listExpr <-
    ListE <$> for matchedVariables \fieldName ->
      fieldExpr fieldName
  [d|
    instance Csv.ToNamedRecord $(conT typName) where
      toNamedRecord val =
        case val of
          $(pure constrPatternMatch) ->
            Csv.namedRecord $(pure listExpr)
    |]

data FieldName = FieldName
  { fieldOriginalName :: Name
  , fieldPatternVariable :: Name
  }

-- | Derive an instance of the 'Csv.ToRecord' type class. This only works for
-- non-record types.
deriveToRecord :: Name -> DecsQ
deriveToRecord typName = do
  info <- reify typName
  cons <-
    case info of
      TyConI dec ->
        case dec of
          NewtypeD _cxt _name _tyvars _mkind con _derivs ->
            pure [con]
          DataD _cxt _name _tyvars _mkind cons _derivs ->
            pure cons
          _ ->
            fail $
              concat
                [ "Expected "
                , show typName
                , " to be a datatype."
                ]
      _ ->
        fail $
          concat
            [ "Expected"
            , show typName
            , " to be a type name of a datatype."
            ]

  cases <-
    for cons \case
      NormalC constrName bangTypes -> do
        names <- for bangTypes \_ -> newName "p"
        exprBody <-
          AppE (VarE 'V.fromList) . ListE <$> do
            for names \name ->
              [e|Csv.toField $(varE name)|]
        let constrPattern =
              ConP constrName [] (map VarP names)
        pure $ Match constrPattern (NormalB exprBody) []
      RecC constrName varBangTypes -> do
        names <- for varBangTypes \(name, _, _) -> newName (nameBase name)
        exprBody <-
          AppE (VarE 'V.fromList) . ListE <$> do
            for names \name ->
              [e|Csv.toField $(varE name)|]
        let constrPattern =
              ConP constrName [] (map VarP names)
        pure $ Match constrPattern (NormalB exprBody) []
      _ ->
        fail $
          concat
            [ "Expected "
            , show typName
            , " to have regular constructors"
            ]

  [d|
    instance Csv.ToRecord $(conT typName) where
      toRecord =
        $(pure $ LamCaseE cases)
    |]

-- | Derive an instance of 'Csv.DefaultOrdered' for the type.
deriveDefaultOrdered :: CsvOptions -> Name -> DecsQ
deriveDefaultOrdered opts typName = do
  info <- reify typName
  con <-
    case info of
      TyConI dec ->
        case dec of
          NewtypeD _cxt _name _tyvars _mkind con _derivs ->
            pure con
          DataD _cxt _name _tyvars _mkind cons _derivs ->
            case cons of
              [con] ->
                pure con
              _ ->
                fail $
                  concat
                    [ "Expected "
                    , show typName
                    , " to be a record with a single constructor."
                    ]
          _ ->
            fail $
              concat
                [ "Expected "
                , show typName
                , " to be a record with a single constructor."
                ]
      _ ->
        fail $
          concat
            [ "Expected"
            , show typName
            , " to be a type name of a single record constructor."
            ]
  body <-
    case con of
      RecC _constrName varBangTypes -> do
        ListE <$> for varBangTypes \(fieldName, _, _) -> do
          let modifiedFieldName =
                Csv.fieldLabelModifier opts (nameBase fieldName)
          [e|$(litE $ StringL modifiedFieldName)|]
      _ ->
        fail $
          concat
            [ "Expected "
            , show typName
            , "to be a record with a single constructor."
            ]

  [d|
    instance Csv.DefaultOrdered $(conT typName) where
      headerOrder _ = V.fromList $(pure body)
    |]

-- | Derive an instance of both 'Csv.ToNamedRecord' and 'Csv.FromNamedRecord'
-- for a record type.
deriveNamedRecord :: CsvOptions -> Name -> DecsQ
deriveNamedRecord opts typName =
  deriveToNamedRecord opts typName <> deriveFromNamedRecord opts typName

-- | Derive an instance of 'Csv.FromNamedRecord' for record types.
deriveFromNamedRecord :: CsvOptions -> Name -> DecsQ
deriveFromNamedRecord opts typName = do
  info <- reify typName
  con <-
    case info of
      TyConI dec ->
        case dec of
          NewtypeD _cxt _name _tyvars _mkind con _derivs ->
            pure con
          DataD _cxt _name _tyvars _mkind cons _derivs ->
            case cons of
              [con] ->
                pure con
              _ ->
                fail $
                  concat
                    [ "Expected "
                    , show typName
                    , " to be a record with a single constructor."
                    ]
          _ ->
            fail $
              concat
                [ "Expected "
                , show typName
                , " to be a record with a single constructor."
                ]
      _ ->
        fail $
          concat
            [ "Expected"
            , show typName
            , " to be a type name of a single record constructor."
            ]
  (constrName, fieldNames) <-
    case con of
      RecC constrName fields -> do
        namesWithPatterns <- for fields \(fieldName, _, _) -> do
          pure fieldName

        pure (constrName, namesWithPatterns)
      _ ->
        fail $
          concat
            [ "Expected "
            , show typName
            , "to be a record with a single constructor."
            ]

  lamExpr <- do
    arg <- newName "namedRecord"

    let getFieldExpr fieldName = do
          let modifiedFieldName =
                Csv.fieldLabelModifier opts (nameBase fieldName)

          [e|$(varE arg) Csv..: $(litE $ StringL modifiedFieldName)|]
    body <- do
      foldM
        (\acc fieldName -> [e|$(pure acc) <*> $(getFieldExpr fieldName)|])
        (VarE 'pure `AppE` ConE constrName)
        fieldNames
    pure $ LamE [VarP arg] body

  [d|
    instance Csv.FromNamedRecord $(conT typName) where
      parseNamedRecord = $(pure lamExpr)
    |]

-- | Derive 'Csv.FromRecord' for a type. This ignores the record fields and uses
-- the position of the fields to determine how to parse the record.
deriveFromRecord :: Name -> DecsQ
deriveFromRecord typName = do
  info <- reify typName
  con <-
    case info of
      TyConI dec ->
        case dec of
          NewtypeD _cxt _name _tyvars _mkind con _derivs ->
            pure con
          DataD _cxt _name _tyvars _mkind cons _derivs ->
            case cons of
              [con] ->
                pure con
              _ ->
                fail $
                  concat
                    [ "Expected "
                    , show typName
                    , " to be a record with a single constructor."
                    ]
          _ ->
            fail $
              concat
                [ "Expected "
                , show typName
                , " to be a record with a single constructor."
                ]
      _ ->
        fail $
          concat
            [ "Expected"
            , show typName
            , " to be a type name of a single record constructor."
            ]
  (constrName, fieldCount) <-
    case con of
      RecC constrName fields -> do
        pure (constrName, length fields)
      NormalC constrName fields ->
        pure (constrName, length fields)
      _ ->
        fail $
          concat
            [ "Expected "
            , show typName
            , "to be a record with a single constructor."
            ]

  lamExpr <- do
    arg <- newName "record"

    let getFieldExpr idx = do
          [e|$(varE arg) Csv..! idx|]

    body <- do
      foldM
        (\acc fieldIndex -> [e|$(pure acc) <*> $(getFieldExpr fieldIndex)|])
        (VarE 'pure `AppE` ConE constrName)
        [0 .. fieldCount - 1]
    pure $ LamE [VarP arg] body

  [d|
    instance Csv.FromRecord $(conT typName) where
      parseRecord = $(pure lamExpr)
    |]

deriveToAndFromRecord :: Name -> DecsQ
deriveToAndFromRecord name =
  deriveToRecord name <> deriveFromRecord name

