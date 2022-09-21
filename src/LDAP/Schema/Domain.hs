module LDAP.Schema.Domain where

import LDAP.Schema.AttributeTypes
import LDAP.Schema.Top

class
    ( IsTop a
    , MustHaveDomainComponent a
    , MayHaveUserPassword a
    , MayHaveX121Address a
    , MayHavePreferredDeliveryMethod a
    , MayHaveTelephoneNumber a
    , MayHaveStreet a
    , MayHavePhysicalDeliveryOfficeName a
    ) =>
    IsDomain a
