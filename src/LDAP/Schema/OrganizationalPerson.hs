{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LDAP.Schema.OrganizationalPerson where

import LDAP.Schema.AttributeTypes
import LDAP.Schema.Person
import LDAP.Schema.Top
import LDAP.Schema.Util

makeObjectClass
    "OrganizationalPerson"
    [ ''IsPerson
    , ''MayHaveTitle
    , ''MayHaveX121Address
    , ''MayHaveRegisteredAddress
    , ''MayHaveDestinationIndicator
    , ''MayHavePreferredDeliveryMethod
    , ''MayHaveTelexNumber
    , ''MayHaveTeletextTerminalIdentifier
    , ''MayHaveTelephoneNumber
    , ''MayHaveInternationaliSDNNumber
    , ''MayHaveFacsimileTelephoneNumber
    , ''MayHaveStreet
    , ''MayHavePostOfficeBox
    , ''MayHavePostalCode
    , ''MayHavePostalAddress
    , ''MayHavePhysicalDeliveryOfficeName
    , ''MayHaveOrganizationalUnitName
    , ''MayHaveState
    , ''MayHaveLocalityName
    ]
