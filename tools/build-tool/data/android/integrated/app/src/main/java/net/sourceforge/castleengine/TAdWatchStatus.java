/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;


/* Must be synchronized with pascal TAdWatchStatus in src/services/CastleAds.pas */
public enum TAdWatchStatus {
    wsWatched,
    wsUnknownError,
    wsNetworkNotAvailable,
    wsNoAdsAvailable,
    wsUserAborted,
    wsAdNotReady,
    wsAdNetworkNotInitialized,
    wsInvalidRequest,
    wsAdTypeUnsupported,
    wsApplicationReinitialized
}
