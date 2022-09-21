module LDAP.Schema.Country where

import LDAP.Schema.AttributeTypes
import LDAP.Schema.Top

class
    ( IsTop a
    , MustHaveCountryName a
    , MayHaveSearchGuide a
    , MayHaveDescription a
    ) =>
    IsCountry a
