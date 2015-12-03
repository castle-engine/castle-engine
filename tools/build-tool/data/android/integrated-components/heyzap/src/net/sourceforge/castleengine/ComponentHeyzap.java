/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.util.Log;

import com.heyzap.sdk.ads.HeyzapAds;
import com.heyzap.sdk.ads.InterstitialAd;
import com.heyzap.sdk.ads.VideoAd;
import com.heyzap.sdk.ads.HeyzapAds.OnStatusListener;

/**
 * Heyzap (https://www.heyzap.com/)
 * integration with Castle Game Engine Android application.
 */
public class ComponentHeyzap extends ComponentAbstract
{
    private static final String TAG = "${NAME}.castleengine.ComponentHeyzap";

    private boolean initialized, scheduledStart, scheduledResume;
    private boolean mShown;

    public ComponentHeyzap(MainActivity activity)
    {
        super(activity);
    }

    private void initialize(String publisherId)
    {
        if (initialized) {
            return;
        }

        HeyzapAds.start(publisherId, getActivity());

        /* set listener to send message when ad is closed,
           see https://developers.heyzap.com/docs/android_sdk_advanced */
        OnStatusListener staticListener = new OnStatusListener() {
            @Override
            public void onShow(String tag) {
                // Ad is now showing
            }

            @Override
            public void onClick(String tag) {
                // Ad was clicked on. You can expect the user to leave your application temporarily.
            }

            @Override
            public void onHide(String tag) {
                // Ad was closed. The user has returned to your application.
                interstitialDone();
            }

            @Override
            public void onFailedToShow(String tag) {
                // Display was called but there was no ad to show
                interstitialDone();
            }

            @Override
            public void onAvailable(String tag) {
                // An ad has been successfully fetched
            }

            @Override
            public void onFailedToFetch(String tag) {
                // No ad was able to be fetched
                interstitialDone();
            }

            @Override
            public void onAudioStarted() {
                // The ad about to be shown will require audio. Any background audio should be muted.
            }

            @Override
            public void onAudioFinished() {
                // The ad being shown no longer requires audio. Any background audio can be resumed.
            }
        };
        InterstitialAd.setOnStatusListener(staticListener);

        OnStatusListener videoListener = new OnStatusListener() {
            @Override
            public void onShow(String tag) {
                // Ad is now showing
            }

            @Override
            public void onClick(String tag) {
                // Ad was clicked on. You can expect the user to leave your application temporarily.
            }

            @Override
            public void onHide(String tag) {
                // Ad was closed. The user has returned to your application.
                interstitialDone();
            }

            @Override
            public void onFailedToShow(String tag) {
                // Display was called but there was no ad to show
                interstitialDone();
            }

            @Override
            public void onAvailable(String tag) {
                // An ad has been successfully fetched
            }

            @Override
            public void onFailedToFetch(String tag) {
                // No ad was able to be fetched

                // Do not react to onFailedToFetch for videos,
                // as we handle their fetching failure manually by checking
                // VideoAd.isAvailable
                // interstitialDone();
            }

            @Override
            public void onAudioStarted() {
                // The ad about to be shown will require audio. Any background audio should be muted.
            }

            @Override
            public void onAudioFinished() {
                // The ad being shown no longer requires audio. Any background audio can be resumed.
            }
        };
        VideoAd.setOnStatusListener(videoListener);
        VideoAd.fetch();

        Log.i(TAG, "Heyzap initialized (will send delayed onStart: " + scheduledStart + ", will send delayed onResume: " + scheduledResume + ")");
        initialized = true;

        if (scheduledStart) {
            onStart();
            scheduledStart = false;
        }
        if (scheduledResume) {
            onResume();
            scheduledResume = false;
        }
    }

    @Override
    public void onDestroy()
    {
        if (!initialized) {
            return;
        }
        // nothing to do here now
    }

    @Override
    public void onResume()
    {
        if (!initialized) {
            scheduledResume = true; // send onResume to Heyzap SDK when we will be initialized
            return;
        }
        // nothing to do here now
    }

    @Override
    public void onPause()
    {
        scheduledResume = false;
        if (!initialized) {
            return;
        }
        // nothing to do here now
    }

    @Override
    public void onStart()
    {
        if (!initialized) {
            scheduledStart = true; // send onStart to Heyzap SDK when we will be initialized
            return;
        }

    }

    private void startTestActivity()
    {
        if (initialized) {
            HeyzapAds.startTestActivity(getActivity());
        }
    }

    @Override
    public void onStop()
    {
        scheduledStart = false;
        if (!initialized) {
            return;
        }
        // nothing to do here now
    }

    @Override
    public boolean onBackPressed()
    {
        if (!initialized) {
            return false; // let default activity onBackPressed to work
        }

        // If an interstitial is on screen, close it.
        return HeyzapAds.onBackPressed();
    }

    private void interstitialDone()
    {
        if (mShown) {
            messageSend(new String[]{"ads-heyzap-interstitial-display", "shown"});
            mShown = false;
        }
    }

    private void showInterstitial()
    {
        if (initialized) {
            mShown = true;
            InterstitialAd.display(getActivity());
            Log.i(TAG, "Interstitial showing");
        } else {
            // pretend that ad was displayed, in case native app waits for it
            messageSend(new String[]{"ads-heyzap-interstitial-display", "shown"});
        }
    }

    private void showVideo()
    {
        if (initialized) {
            if (VideoAd.isAvailable()) {
                mShown = true;
                VideoAd.display(getActivity());
                VideoAd.fetch();
                Log.i(TAG, "Video showing");
            } else {
                Log.i(TAG, "Video not in cache yet, just skip it");
                // pretend that ad was displayed, in case native app waits for it
                messageSend(new String[]{"ads-heyzap-interstitial-display", "shown"});
            }
        } else {
            // pretend that ad was displayed, in case native app waits for it
            messageSend(new String[]{"ads-heyzap-interstitial-display", "shown"});
        }
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 2 && parts[0].equals("ads-heyzap-initialize")) {
            initialize(parts[1]);
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("ads-heyzap-show-interstitial") && parts[1].equals("static")) {
            showInterstitial();
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("ads-heyzap-show-interstitial") && parts[1].equals("video")) {
            showVideo();
            return true;
        } else
        if (parts.length == 1 && parts[0].equals("ads-heyzap-start-test-activity")) {
            startTestActivity();
            return true;
        } else
        {
            return false;
        }
    }
}
