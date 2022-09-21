module LDAP.Schema.Alias where

import LDAP.Schema.AttributeTypes
import LDAP.Schema.Top

class
    ( IsTop a
    , MustHaveAliasedObjectName a
    ) =>
    IsAlias a
