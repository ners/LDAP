module LDAP.Schema.DistinguishedName where

import Data.List (intercalate)
import Data.Text (Text, unpack)

data DistinguishedNamePart = DistinguishedNamePart Text Text

instance Show DistinguishedNamePart where
    show (DistinguishedNamePart k v) = unpack k <> "=" <> unpack v

newtype DistinguishedName = DistinguishedName [DistinguishedNamePart]

instance Show DistinguishedName where
    show (DistinguishedName ns) = intercalate "," $ show <$> ns
