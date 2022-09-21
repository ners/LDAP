module LDAP.Search.Filter where

import Data.Text (Text)

type LdapString = Text

type AssertionValue = Text
type AttributeValue = Text
type MatchingRuleId = LdapString

data Filter
    = And [Filter]
    | Or [Filter]
    | Not Filter
    | EqualityMatch AttributeValueAssertion
    | Substrings SubstringFilter
    | GreaterOrEqual AttributeValueAssertion
    | LessOrEqual AttributeValueAssertion
    | Present AttributeDescription
    | ApproxMatch AttributeValueAssertion
    | ExtensibleMatch MatchingRuleAssertion
    deriving (Show)

data AttributeValueAssertion = AttributeValueAssertion
    { attributeDesc :: AttributeDescription
    , assertionValue :: AssertionValue
    }
    deriving (Show)

data AttributeDescription = AttributeDescription
    { attributeType :: ObjectIdentifier
    , attributeOptions :: [Text]
    }
    deriving (Show)

data ObjectIdentifier = DescrOid Text | NumericOid Int Int
    deriving (Show)

data SubstringFilter = SubstringFilter
    { substringFilterType :: AttributeDescription
    , substringFilterInitial :: Maybe AssertionValue
    , substringFilterAny :: [AssertionValue]
    , substringFilterFinal :: Maybe AssertionValue
    }
    deriving (Show)

data MatchingRuleAssertion = MatchingRuleAssertion
    { matchingRule :: Maybe MatchingRuleId
    , matchingRuleType :: Maybe AttributeDescription
    , matchingRuleValue :: AssertionValue
    , matchingRuleDnAttributes :: Bool
    }
    deriving (Show)
