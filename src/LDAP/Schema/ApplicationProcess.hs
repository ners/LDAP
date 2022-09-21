module LDAP.Schema.ApplicationProcess where

import LDAP.Schema.AttributeTypes
import LDAP.Schema.Top

class
    ( IsTop a
    , MustHaveCommonName a
    , MayHaveSeeAlso a
    , MayHaveOrganizationalUnitName a
    , MayHaveLocalityName a
    , MayHaveDescription a
    ) =>
    IsApplicationProcess a
