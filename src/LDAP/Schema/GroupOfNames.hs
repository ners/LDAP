module LDAP.Schema.GroupOfNames where

import LDAP.Schema.AttributeTypes
import LDAP.Schema.Top

class
    ( IsTop a
    , MustHaveCommonName a
    , MustHaveMember a
    , MayHaveBusinessCategory a
    , MayHaveSeeAlso a
    , MayHaveOwner a
    , MayHaveOrganizationalUnitName a
    , MayHaveOrganizationName a
    , MayHaveDescription a
    ) =>
    IsGroupOfNames a
