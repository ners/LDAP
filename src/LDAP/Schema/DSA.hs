module LDAP.Schema.DSA where

import LDAP.Schema.ApplicationEntity
import LDAP.Schema.AttributeTypes

class
    ( IsApplicationEntity a
    , MayHaveKnowledgeInformation a
    ) =>
    IsDSA a
