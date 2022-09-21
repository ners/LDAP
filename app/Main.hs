{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Control.Lens
import qualified Data.Binary as Binary
import Data.Text (Text)

import LDAP.Schema.AttributeTypes
import LDAP.Schema.OrganizationalPerson
import LDAP.Schema.Person
import LDAP.Schema.Top

import LDAP.Search.Filter
import LDAP.Search.Filter.Binary

data User = User
    { _username :: Text
    , _password :: Maybe Text
    }
    deriving (Show)

makeLenses ''User

instance MustHaveSurname User where surname = username . coerced
instance MustHaveCommonName User where commonName = username . coerced
instance MayHaveUserPassword User where maybeUserPassword = password . coerced

makePerson ''User
makeOrganizationalPerson ''User

main :: IO ()
main = do
    let u = User "Max Mustermann" (Just "foobar")
    let u' = set password (Just "p@$$w0rd") u
    print u'
    let f =
            And
                [ Or []
                , Not $ Or []
                ]
    let bf = Binary.encode f
    print bf
    print $ Binary.decode @Filter "(!(&))"
