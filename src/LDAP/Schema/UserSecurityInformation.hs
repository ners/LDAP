module LDAP.Schema.UserSecurityInformation where

import LDAP.Schema.AttributeTypes
import LDAP.Schema.Top

class
    ( IsTop a
    , MustHaveSupportedAlgorithms a
    ) =>
    IsUserSecurityInformation a
