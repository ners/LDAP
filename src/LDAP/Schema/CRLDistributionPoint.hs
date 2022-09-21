module LDAP.Schema.CRLDistributionPoint where

import LDAP.Schema.AttributeTypes
import LDAP.Schema.Top

class
    ( IsTop a
    , MustHaveCommonName a
    , MayHaveCertificateRevocationList a
    , MayHaveAuthorityRevocationList a
    , MayHaveDeltaRevocationList a
    ) =>
    IsCRLDistributionPoint a
