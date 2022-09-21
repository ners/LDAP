{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module LDAP.Search.Filter.Binary where

import Data.Binary
import Data.Binary.Parser
import Data.Foldable (asum)
import LDAP.Search.Filter

instance Binary Filter where
    put f = put '(' >> putf f >> put ')'
      where
        putf :: Filter -> Put
        putf (And fs) = put '&' >> mapM_ put fs
        putf (Or fs) = put '|' >> mapM_ put fs
        putf (Not f) = put '!' >> put f
    get = do
        string "("
        f <-
            asum
                [ andFilter
                , orFilter
                , notFilter
                , simpleFilter
                --, presentFilter
                --, substringFilter
                --, extensibleFilter
                ]
        string ")"
        return f
      where
        andFilter = string "&" >> return (And [])
        orFilter = string "|" >> return (Or [])
        notFilter = string "!" >> Not <$> get
        simpleFilter = do
            -- attr <- get
            -- filterType <- asum
            --         [ string "=" >> return EqualityMatch
            --         , string "~=" >> return ApproxMatch
            --         , string ">=" >> return GreaterOrEqual
            --         , string "<=" >> return LessOrEqual
            --         ]
            -- value <- get
            --return $ filterType $ AttributeDescription { attributeDesc = attr, attributeOptions = [] }
            return $ And []
        --presentFilter =
        --substringFilter =
        --extensibleFilter = 
