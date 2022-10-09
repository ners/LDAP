{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Control.Lens
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Binary.Parser as Binary

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

decodeFull :: forall a. Binary a => ByteString -> a
decodeFull = Binary.runGet (Binary.get @a <* Binary.endOfInput)

main :: IO ()
main = do
    let u = User "Max Mustermann" (Just "foobar")
    let u' = set password (Just "p@$$w0rd") u
    print u'
