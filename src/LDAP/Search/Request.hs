module LDAP.Search.Request where

import LDAP.Schema.DistinguishedName
import LDAP.Search.Filter

-- https://datatracker.ietf.org/doc/html/rfc4511#section-4.5.1
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

-- https://datatracker.ietf.org/doc/html/rfc4511#section-4.5.1
data Scope
    = BaseObjectScope
    | SingleLevelScope
    | WholeSubtreeScope
    deriving (Show)

-- https://datatracker.ietf.org/doc/html/rfc4511#section-4.5.1
data DerefAliases
    = NeverDerefAliases
    | DerefInSearching
    | DerefFindingBaseObj
    | DerefAlways
    deriving (Show)

-- https://datatracker.ietf.org/doc/html/rfc4511#section-4.5.1.8
-- TODO: should we allow multiple attrs / noattrs / allattrs?
data AttributeSelector
    = Attrs [AttributeDescription]
    | NoAttrs
    | AllUserAttrs
    deriving (Show)
