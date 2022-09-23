{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module LDAP.Schema.Util where

import Control.Lens
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Data.String
import Data.Text (Text)
import GHC.Generics (Generic)
import LDAP.Schema.DistinguishedName
import Language.Haskell.TH

instance IsString Name where
    fromString = mkName

instance Semigroup Name where
    a <> b = fromString $ m <> (nameBase a ++ nameBase b)
      where
        m = maybe "" (++ ".") $ nameModule b

makeMissingInstances :: [Name] -> Name -> DecsQ
makeMissingInstances classNames name = do
    let mkInstance className = do
            exists <- isInstance className [ConT name]
            return $
                if exists
                    then Nothing
                    else Just $ InstanceD Nothing [] (AppT (ConT className) (ConT name)) []
    insts <- mapM mkInstance classNames
    return $ catMaybes insts

class AttributeType a where
    attributeName :: Text
    attributeValue :: a -> Text
    toDN :: a -> DistinguishedName
    toDN a = DistinguishedName [DistinguishedNamePart (attributeName @a) (attributeValue a)]

makeAttributeType :: Name -> String -> DecsQ
makeAttributeType name str =
    return
        [ NewtypeD
            []
            name
            []
            Nothing
            (NormalC name [(Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Text)])
            [DerivClause Nothing $ ConT <$> [''Generic, ''Show, ''Eq]]
        , InstanceD
            Nothing
            []
            (AppT (ConT ''Wrapped) (ConT name))
            []
        , InstanceD
            Nothing
            []
            (AppT (ConT ''AttributeType) (ConT name))
            [ FunD
                'attributeName
                [ Clause
                    []
                    (NormalB $ LitE $ StringL str)
                    []
                ]
            , FunD
                'attributeValue
                [ Clause
                    [ConP name [VarP a]]
                    (NormalB $ VarE a)
                    []
                ]
            ]
        , ClassD
            []
            ("MustHave" <> name)
            [PlainTV a]
            []
            [SigD lowerName $ lensT (VarT a) (ConT name)]
        , ClassD
            []
            ("MayHave" <> name)
            [PlainTV a]
            []
            [ SigD maybeName $ lensT (VarT a) (AppT (ConT ''Maybe) (ConT name))
            , FunD maybeName [Clause [] (NormalB $ lensE (AppE constE $ ConE 'Nothing) constE) []]
            ]
        ]
  where
    a = mkName "a"
    maybeName = "maybe" <> name
    lensT s = AppT (AppT (ConT ''Lens') s)
    lensE g = AppE (AppE (VarE 'lens) g)
    constE = VarE 'const
    lowerName = fromString $ let (n : ns) = show name in toLower n : ns

class ObjectClass a

makeObjectClass :: Name -> [Name] -> DecsQ
makeObjectClass name typeclasses =
    return
        [ ClassD
            ((\t -> AppT (ConT t) (VarT a)) <$> typeclasses)
            ("Is" <> name)
            [PlainTV a]
            []
            []
        , SigD makeName (AppT (AppT ArrowT (ConT ''Name)) (ConT ''DecsQ))
        , FunD
            makeName
            [ Clause [] (NormalB $ AppE (VarE 'makeMissingInstances) (ListE $ LitE . StringL . nameBase <$> implTypeclasses)) []
            ]
        ]
  where
    a = mkName "a"
    makeName = "make" <> name
    implTypeclasses = ("Is" <> name) : filter (\t -> not ("MustHave" `isPrefixOf` nameBase t)) typeclasses
