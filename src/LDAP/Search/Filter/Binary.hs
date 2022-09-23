{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module LDAP.Search.Filter.Binary where

import Codec.Binary.UTF8.Generic (UTF8Bytes)
import Control.Applicative (many, Alternative((<|>)))
import Data.Binary
import Data.Binary.Parser hiding (isDigit, isHexDigit)
import Data.Char (isAlpha, isDigit, isHexDigit, chr)
import Data.Foldable (asum, toList)
import Data.List (intersperse, intercalate)
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.Text (Text)
import Data.Binary.Parser.Char8 (char)
import LDAP.Search.Filter
import qualified Codec.Binary.UTF8.Generic as UTF8
import qualified Data.Binary.Parser.Char8 as Char8
import qualified Data.Text as Text
import Numeric (readHex)

many1 :: Alternative f => f a -> f (NonEmpty a)
many1 p = (:|) <$> p <*> many p

putRawList :: Binary a => [a] -> Put
putRawList = mapM_ put

putString :: String -> Put
putString = putRawList

putText :: Text -> Put
putText = putString . Text.unpack

-- https://datatracker.ietf.org/doc/html/rfc4515#section-3
instance Binary Filter where
    put f = put '(' >> putf f >> put ')'
      where
        putf :: Filter -> Put
        putf (AndFilter fs) = put '&' >> mapM_ put fs
        putf (OrFilter fs) = put '|' >> mapM_ put fs
        putf (NotFilter f) = put '!' >> put f
        putf (SimpleFilter ft AttributeValueAssertion{..}) = do
            put attributeDesc
            put ft
            putText assertionValue
    get = do
        char '('
        f <- simpleFilter
            --asum
            --    [ andFilter
            --    , orFilter
            --    , notFilter
            --    , simpleFilter
            --    --, presentFilter
            --    --, substringFilter
            --    --, extensibleFilter
            --    ]
        char ')'
        return f
      where
        andFilter = char '&' >> AndFilter <$> many1 get
        orFilter = char '|' >> OrFilter <$> many1 get
        notFilter = char '!' >> NotFilter <$> get
        simpleFilter = do
            attr <- get @AttributeDescription
            filterType <- get @FilterType
            value <- valueEncoding
            return $ SimpleFilter filterType $ AttributeValueAssertion
                { attributeDesc = attr
                , assertionValue = value
                }
        --presentFilter =
        --substringFilter =
        --extensibleFilter = 

instance Binary FilterType where
    put Equal = putString "="
    put GreaterOrEqual = putString ">="
    put LessOrEqual = putString "<="
    put ApproxEqual = putString "~="
    get = asum
            [ string "=" >> return Equal
            , string "~=" >> return ApproxEqual
            , string ">=" >> return GreaterOrEqual
            , string "<=" >> return LessOrEqual
            ]

instance Binary ObjectIdentifier where
    put (DescrOid t) = putText t
    put (NumericOid n ns) = putString $ intercalate "." ts
        where
            ns' = n : toList ns
            ts = show <$> ns'
    get = asum [ numeric, descr ]
        where
            numeric = do
                n0:n1:ns <- decimal `sepBy` char '.'
                return $ NumericOid n0 (n1 :| ns)
            descr = do
                x <- leadKeychar
                xs <- many keychar
                return $ DescrOid $ Text.pack $ x : xs

alpha :: Get Char
alpha = Char8.satisfy isAlpha

digit :: Get Char
digit = Char8.satisfy isDigit

hexDigit :: Get Char
hexDigit = Char8.satisfy isHexDigit

hyphen :: Get Char
hyphen = char '-' >> return '-'

leadKeychar :: Get Char
leadKeychar = alpha

keychar :: Get Char
keychar = asum [ alpha, digit, hyphen ]

-- Idea: the escaped portion of the valueEncoding and the subset exceptions
-- only apply to ASCII code points. We can thus process it first without any
-- concern for multi-byte encoding, then process the UTF-8 byte string as a
-- whole.
valueEncoding :: Get Text
valueEncoding = Text.pack <$> many valueEncodingByte
    where
        valueEncodingByte = escaped <|> unescaped
        -- TODO? make excluded characters trigger a parse error by flipping the satisfy check around and failing explicitly
        unescaped =  Char8.satisfy (`notElem` ['\NUL', '(', ')', '*', '\\'])
        escaped = do
            Char8.char '\\'
            a <- hexDigit
            b <- hexDigit
            return $ chr $ fst $ head $ readHex [a, b]

instance Binary AttributeDescription where
    put AttributeDescription{..} = do
        put attributeType
        putText $ Text.intercalate ";" $ "" : attributeOptions
    get = AttributeDescription <$> get <*> many (char ';' >> getOption)
        where
            getOption :: Get Text
            getOption = Text.pack . toList <$> many1 keychar