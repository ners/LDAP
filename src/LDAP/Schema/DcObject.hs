module LDAP.Schema.DcObject where

import LDAP.Schema.AttributeTypes
import LDAP.Schema.Top

class
    ( IsTop a
    , MustHaveDomainComponent a
    ) =>
    IsDcObject a
