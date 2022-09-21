module LDAP.Schema.OrganizationalRole where

import LDAP.Schema.AttributeTypes
import LDAP.Schema.Top

class
    ( IsTop a
    , MayHaveX121Address a
    , MayHaveRegisteredAddress a
    , MayHaveDestinationIndicator a
    , MayHavePreferredDeliveryMethod a
    , MayHaveTelexNumber a
    , MayHaveTeletextTerminalIdentifier a
    , MayHaveTelephoneNumber a
    , MayHaveInternationaliSDNNumber a
    , MayHaveFacsimileTelephoneNumber a
    , MayHaveSeeAlso a
    , MayHaveRoleOccupant a
    , MayHaveStreet a
    , MayHavePostOfficeBox a
    , MayHavePostalCode a
    , MayHavePostalAddress a
    , MayHavePhysicalDeliveryOfficeName a
    , MayHaveOrganizationalUnitName a
    , MayHaveState a
    , MayHaveLocalityName a
    , MayHaveDescription a
    ) =>
    IsOrganizationalRole a
