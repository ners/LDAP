module LDAP.Schema.Locality where

import LDAP.Schema.AttributeTypes
import LDAP.Schema.Top

class
    ( IsTop a
    , MayHaveStreet a
    , MayHaveSeeAlso a
    , MayHaveSearchGuide a
    , MayHaveState a
    , MayHaveLocalityName a
    , MayHaveDescription a
    ) =>
    IsLocality a
