module LDAP.Schema.CertificationAuthority where

import LDAP.Schema.AttributeTypes
import LDAP.Schema.Top

class
    ( IsTop a
    , MustHaveAuthorityRevocationList a
    , MustHaveCertificateRevocationList a
    , MustHaveCACertificate a
    , MayHaveCrossCertificatePair a
    ) =>
    IsCertificationAuthority a
