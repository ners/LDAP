module LDAP.Schema.ApplicationEntity where

import LDAP.Schema.AttributeTypes
import LDAP.Schema.Top

class
    ( IsTop a
    , MustHaveCommonName a
    , MustHavePresentationAddress a
    , MayHaveSupportedApplicationContext a
    , MayHaveSeeAlso a
    , MayHaveOrganizationalUnitName a
    , MayHaveOrganizationName a
    , MayHaveLocalityName a
    , MayHaveDescription a
    ) =>
    IsApplicationEntity a
