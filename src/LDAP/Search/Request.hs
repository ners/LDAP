module LDAP.Search.Request where

import LDAP.Schema.DistinguishedName
import LDAP.Search.Filter

data SearchRequest = SearchRequest
    { baseObject :: DistinguishedName
    , scope :: Scope
    , derefAliases :: DerefAliases
    , sizeLimit :: Int
    , timeLimit :: Int
    , typesOnly :: Bool
    , filter :: Filter
    , attributes :: AttributeSelector
    }
    deriving (Show)

data Scope
    = BaseObjectScope
    | SingleLevelScope
    | WholeSubtreeScope
    deriving (Show)

data DerefAliases
    = NeverDerefAliases
    | DerefInSearching
    | DerefFindingBaseObj
    | DerefAlways
    deriving (Show)

data AttributeSelector
    = Attrs [AttributeDescription]
    | NoAttrs
    | AllUserAttrs
    deriving (Show)
