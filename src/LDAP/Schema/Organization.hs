module LDAP.Schema.Organization where

import LDAP.Schema.AttributeTypes
import LDAP.Schema.Top

class
    ( IsTop a
    , MustHaveOrganizationName a
    , MayHaveUserPassword a
    , MayHaveSearchGuide a
    , MayHaveSeeAlso a
    , MayHaveBusinessCategory a
    , MayHaveX121Address a
    , MayHaveRegisteredAddress a
    , MayHaveDestinationIndicator a
    , MayHavePreferredDeliveryMethod a
    , MayHaveTelexNumber a
    , MayHaveTeletextTerminalIdentifier a
    , MayHaveTelephoneNumber a
    , MayHaveInternationaliSDNNumber a
    , MayHaveFacsimileTelephoneNumber a
    , MayHaveStreet a
    , MayHavePostOfficeBox a
    , MayHavePostalCode a
    , MayHavePostalAddress a
    , MayHavePhysicalDeliveryOfficeName a
    , MayHaveState a
    , MayHaveLocalityName a
    , MayHaveDescription a
    ) =>
    IsOrganization a
