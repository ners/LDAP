module LDAP.Schema.GroupOfUniqueNames where

import LDAP.Schema.AttributeTypes
import LDAP.Schema.Top

class
    ( IsTop a
    , MustHaveCommonName a
    , MustHaveUniqueMember a
    , MayHaveBusinessCategory a
    , MayHaveSeeAlso a
    , MayHaveOwner a
    , MayHaveOrganizationalUnitName a
    , MayHaveOrganizationName a
    , MayHaveDescription a
    ) =>
    IsGroupOfUniqueNames a
