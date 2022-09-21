module LDAP.Schema.CertificationAuthorityV2 where

import LDAP.Schema.AttributeTypes
import LDAP.Schema.CertificationAuthority
import LDAP.Schema.Top

class
    ( IsCertificationAuthority a
    , MustHaveDeltaRevocationList a
    ) =>
    IsCertificationAuthorityV2 a
