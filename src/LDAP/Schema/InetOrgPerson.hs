module LDAP.Schema.InetOrgPerson where

import LDAP.Schema.AttributeTypes
import LDAP.Schema.OrganizationalPerson

class
    ( IsOrganizationalPerson a
    , MustHaveDomainComponent a
    , MayHaveUserPassword a
    , MayHaveX121Address a
    , MayHavePreferredDeliveryMethod a
    , MayHaveTelephoneNumber a
    , MayHaveStreet a
    , MayHavePhysicalDeliveryOfficeName a
    ) =>
    IsInetOrgPerson a
