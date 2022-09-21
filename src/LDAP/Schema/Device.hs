module LDAP.Schema.Device where

import LDAP.Schema.AttributeTypes
import LDAP.Schema.Top

class
    ( IsTop a
    , MustHaveCommonName a
    , MayHaveSerialNumber a
    , MayHaveSeeAlso a
    , MayHaveOwner a
    , MayHaveOrganizationalUnitName a
    , MayHaveOrganizationName a
    , MayHaveLocalityName a
    , MayHaveDescription a
    ) =>
    IsDevice a
