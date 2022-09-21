module LDAP.Schema.StrongAuthenticationUser where

import LDAP.Schema.AttributeTypes
import LDAP.Schema.Top

class
    ( IsTop a
    , MustHaveUserCertificate a
    ) =>
    IsStrongAuthenticationUser a
