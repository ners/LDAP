module LDAP.Search.Filter where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import Numeric.Natural (Natural)

type LdapString = Text

type AssertionValue = Text
type AttributeValue = Text
type MatchingRuleId = LdapString

-- https://datatracker.ietf.org/doc/html/rfc4511#section-4.5.1
data Filter
    = AndFilter (NonEmpty Filter)
    | OrFilter (NonEmpty Filter)
    | NotFilter Filter
    | SimpleFilter FilterType AttributeValueAssertion
    | Substrings SubstringFilter
    | Present AttributeDescription
    | ExtensibleMatch MatchingRuleAssertion
    deriving (Eq, Show)

data FilterType
    = Equal
    | GreaterOrEqual
    | LessOrEqual
    | ApproxEqual
    deriving (Eq, Show, Enum, Bounded)

-- https://datatracker.ietf.org/doc/html/rfc4511#appendix-B
data AttributeValueAssertion = AttributeValueAssertion
    { attributeDesc :: AttributeDescription
    , assertionValue :: AssertionValue
    }
    deriving (Eq, Show)

-- https://datatracker.ietf.org/doc/html/rfc4512#section-2.5
data AttributeDescription = AttributeDescription
    { attributeType :: ObjectIdentifier
    , attributeOptions :: [Text]
    }
    deriving (Eq, Show)

-- https://datatracker.ietf.org/doc/html/rfc4512#section-1.4
data ObjectIdentifier
    = DescrOid Text
    | NumericOid Natural (NonEmpty Natural)
    deriving (Eq, Show)

-- https://datatracker.ietf.org/doc/html/rfc4511#section-4.5.1
data SubstringFilter = SubstringFilter
    { substringFilterType :: AttributeDescription
    , substringFilterInitial :: Maybe AssertionValue
    , substringFilterAny :: [AssertionValue]
    , substringFilterFinal :: Maybe AssertionValue
    }
    deriving (Eq, Show)

-- https://datatracker.ietf.org/doc/html/rfc4511#section-4.5.1
data MatchingRuleAssertion = MatchingRuleAssertion
    { matchingRule :: Maybe MatchingRuleId
    , matchingRuleType :: Maybe AttributeDescription
    , matchingRuleValue :: AssertionValue
    , matchingRuleDnAttributes :: Bool
    }
    deriving (Eq, Show)
