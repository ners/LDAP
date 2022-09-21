{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LDAP.Schema.Person where

import LDAP.Schema.AttributeTypes
import LDAP.Schema.Top
import LDAP.Schema.Util

makeObjectClass
    "Person"
    [ ''IsTop
    , ''MustHaveSurname
    , ''MustHaveCommonName
    , ''MayHaveUserPassword
    , ''MayHaveTelephoneNumber
    , ''MayHaveSeeAlso
    , ''MayHaveDescription
    ]
