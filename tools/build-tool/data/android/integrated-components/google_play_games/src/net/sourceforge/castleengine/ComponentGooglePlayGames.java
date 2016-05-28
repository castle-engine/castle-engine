/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.content.Intent;
import android.content.IntentSender;
import android.util.Log;
import android.app.Activity;
import android.os.Bundle;

import com.google.android.gms.common.ConnectionResult;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.ResultCallback;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.games.Games;
import com.google.android.gms.games.leaderboard.LeaderboardVariant;
import com.google.android.gms.games.leaderboard.Leaderboards.LoadPlayerScoreResult;

import ${QUALIFIED_NAME}.R;

/**
 * Integration of Google Games (achievements, leaderboards and more) with
 * Castle Game Engine.
 */
public class ComponentGooglePlayGames extends ComponentAbstract implements
    GoogleApiClient.ConnectionCallbacks,
    GoogleApiClient.OnConnectionFailedListener
{
    private static final String TAG = "${NAME}.castleengine.ComponentGooglePlayGames";
    private static int REQUEST_SIGN_IN = 9001;
    private static int REQUEST_ACHIEVEMENTS = 9101;
    private static int REQUEST_LEADERBOARD = 9102;

    private boolean initialized, scheduledStart;

    private GoogleApiClient mGoogleApiClient;

    /**
     * Once connected, query for this leaderboard best score.
     */
    private String mDefaultLeaderboardToRefresh;

    public ComponentGooglePlayGames(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "google-play-games";
    }

    private void initialize()
    {
        if (initialized) {
            return;
        }

        String appId = getActivity().getResources().getString(R.string.app_id);
        if (appId.equals("")) {
            Log.e(TAG, "You must define Google Play Games id of your game in CastleEngineManifest.xml, like <google_play_services app_id=\"xxxx\" />. You get this id after creating Google Game Services for your game in Google Developer Console. Without this, GooglePlayGames integration cannot be initialized.");
            return;
        }

        // Create the Google Api Client with access to the Play Game services
        mGoogleApiClient = new GoogleApiClient.Builder(getActivity())
            .addConnectionCallbacks(this)
            .addOnConnectionFailedListener(this)
            .addApi(Games.API).addScope(Games.SCOPE_GAMES)
            .build();
        initialized = true;

        if (scheduledStart) {
            onStart();
            scheduledStart = false;
        }
    }

    @Override
    public void onStart() {
        if (!initialized) {
            scheduledStart = true; // send connect() to mGoogleApiClient when we will be initialized
            return;
        }

        mGoogleApiClient.connect();
    }

    @Override
    public void onStop() {
        scheduledStart = false;
        if (!initialized) {
            return;
        }

        if (mGoogleApiClient.isConnected()) {
            mGoogleApiClient.disconnect();
        }
    }

    /**
     * Return initialized and mGoogleSignedIn,
     * while by the way also checking related stuff, to be secure. */
    private boolean checkGamesConnection()
    {
        if (initialized && mGoogleApiClient == null) {
            Log.w(TAG, "initialized is true, but mGoogleApiClient == null");
            initialized = false;
        }
        if (initialized && mGoogleSignedIn && !mGoogleApiClient.isConnected()) {
            Log.w(TAG, "mGoogleSignedIn is true, but mGoogleApiClient.isConnected() == false");
            mGoogleSignedIn = false;
        }
        return initialized && mGoogleSignedIn;
    }

    // Are we signed in. Starts at false (native code can assume this
    // starting value too), and we notify native code about every change.
    private boolean mGoogleSignedIn = false;

    // Set mGoogleSignedIn variable, also send to native code notification.
    private void setGoogleSignedIn(boolean value)
    {
        if (mGoogleSignedIn != value) {
            mGoogleSignedIn = value;
            messageSend(new String[]{"google-sign-in-status",
                (mGoogleSignedIn ? "true" : "false")});
        }
    }

    // Score to send to a leaderboard, once we're connected.
    // Use only if > 0.
    private long mScoreToSendWhenConnected;
    private String mScoreToSendWhenConnectedLeaderboard;

    @Override
    public void onConnected(Bundle connectionHint)
    {
        Log.i(TAG, "onConnected (Google Games connected OK!)");

        // The player is signed in.
        // We can now e.g. hide the sign-in button and/or allow the player to proceed.
        setGoogleSignedIn(true);
        if (mDefaultLeaderboardToRefresh != null) {
            refreshCurrentScore(mDefaultLeaderboardToRefresh);
        }

        if (mScoreToSendWhenConnected > 0) {
            if (checkGamesConnection()) {
                Games.Leaderboards.submitScore(mGoogleApiClient,
                    mScoreToSendWhenConnectedLeaderboard, mScoreToSendWhenConnected);
                Log.i(TAG, "Submitting scheduled score " + mScoreToSendWhenConnected);
                mScoreToSendWhenConnected = 0;
                mScoreToSendWhenConnectedLeaderboard = null;
            } else {
                Log.e(TAG, "Cannot submit scheduled score, we are not connected inside onConnected - weird, unless the connection broke immediately");
            }
        }
    }

    private boolean mResolvingConnectionFailure = false;
    private boolean mAutoStartSignInFlow = true;
    private boolean mSignInClicked = false;
    private boolean mDuringSignOut = false;

    /**
     * Resolve a connection failure from
     * {@link com.google.android.gms.common.api.GoogleApiClient.OnConnectionFailedListener#onConnectionFailed(com.google.android.gms.common.ConnectionResult)}
     * Like BaseGameUtils.resolveConnectionFailure,
     * but do not show default (ugly and non-reliably-working) error dialogs.
     *
     * @param activity the Activity trying to resolve the connection failure.
     * @param client the GoogleAPIClient instance of the Activity.
     * @param result the ConnectionResult received by the Activity.
     * @param requestCode a request code which the calling Activity can use to identify the result
     *   of this resolution in onActivityResult.
     * @return true if the connection failure is resolved, false otherwise.
     */
    private static boolean resolveConnectionFailure(Activity activity,
        GoogleApiClient client, ConnectionResult result, int requestCode)
    {
        if (result.hasResolution()) {
            try {
                result.startResolutionForResult(activity, requestCode);
                Log.i(TAG, "Connection failure: doing startResolutionForResult");
                return true;
            } catch (IntentSender.SendIntentException e) {
                // The intent was canceled before it was sent.  Return to the default
                // state and attempt to connect to get an updated ConnectionResult.
                client.connect();
                Log.i(TAG, "Connection failure: doing startResolutionForResult but failed, so doing simple client.connect");
                return false;
            }
        } else {
            // not resolvable... so show an error message
            Log.w(TAG, "Connection failure: There was an issue with sign-in to Google Games, please try again later.");
            return false;
        }
    }

    @Override
    public void onConnectionFailed(ConnectionResult connectionResult)
    {
        Log.w(TAG, "onConnectionFailed");

        if (mResolvingConnectionFailure) {
            // already resolving
            return;
        }

        // if the sign-in button was clicked or if auto sign-in is enabled,
        // launch the sign-in flow
        if (mSignInClicked || mAutoStartSignInFlow) {
            mAutoStartSignInFlow = false;
            mSignInClicked = false;
            mResolvingConnectionFailure = true;

            // Attempt to resolve the connection failure.
            if (!resolveConnectionFailure(getActivity(),
                    mGoogleApiClient, connectionResult, REQUEST_SIGN_IN)) {
                mResolvingConnectionFailure = false;
            }
        }

        // We can now e.g. display the sign-in button.
        setGoogleSignedIn(false);
    }

    @Override
    public void onConnectionSuspended(int i)
    {
        Log.i(TAG, "onConnectionSuspended, attempting to reconnect");
        setGoogleSignedIn(false);
        // Attempt to reconnect
        mGoogleApiClient.connect();
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent intent) {
        if (requestCode == REQUEST_SIGN_IN) {
            Log.i(TAG, "onActivityResult - REQUEST_SIGN_IN");
            mSignInClicked = false;
            mResolvingConnectionFailure = false;
            if (resultCode == Activity.RESULT_OK) {
                mGoogleApiClient.connect();
            } else {
                Log.w(TAG, "Unable to sign in to Google Games.");
            }
        }
    }

    private void signInClicked()
    {
        /* don't act when inside sign-out process, or not initialized */
        if (mDuringSignOut || !initialized) {
            return;
        }

        mSignInClicked = true;
        // It is Ok to call it while connecting, as docs
        // https://developer.android.com/reference/com/google/android/gms/common/api/GoogleApiClient.html#connect()
        // say:
        // If the client is already connected or connecting, this method does nothing.
        mGoogleApiClient.connect();
    }

    private void signOutClicked()
    {
        /* don't act when inside sign-out process, or not initialized */
        if (mDuringSignOut || !initialized || !mGoogleApiClient.isConnected()) {
            return;
        }

        mSignInClicked = false;
        // According to docs,
        // https://developer.android.com/reference/com/google/android/gms/games/Games.html#signOut(com.google.android.gms.common.api.GoogleApiClient)
        // this does not actually call mGoogleApiClient.disconnect().
        // So we observe it and eventually call mGoogleApiClient.disconnect
        // later, otherwise future call to signInClicked() would be ignored
        // (as we're connected already ---- just not signed in as anyone).
        mDuringSignOut = true;
        Games.signOut(mGoogleApiClient).setResultCallback(
            new ResultCallback<Status>() {
                public void onResult(Status status)
                {
                    if (!status.isSuccess()) {
                        Log.w(TAG, "Failed to sign out from Games. Droppping mGoogleApiClient anyway.");
                    }
                    if (mGoogleApiClient != null) {
                        mGoogleApiClient.disconnect();
                    }
                    mDuringSignOut = false;
                    setGoogleSignedIn(false);
                }
            });
    }

    private void showAchievements()
    {
        if (checkGamesConnection()) {
            getActivity().startActivityForResult(Games.Achievements.getAchievementsIntent(
                mGoogleApiClient), REQUEST_ACHIEVEMENTS);
        } else {
            Log.i(TAG, "Not connected to Google Games -> connecting, in response to showAchievements");
            signInClicked();
        }
    }

    /**
     * Unlock achievement in Google Games.
     * Achievement id must be created in Google Play Developer console.
     */
    private void achievement(String achievementId)
    {
        if (checkGamesConnection()) {
            Games.Achievements.unlock(mGoogleApiClient, achievementId);
        } else {
            Log.w(TAG, "Achievement unlocked, but not connected to Google Games, ignoring");
        }
    }

    private void showLeaderboard(String leaderboardId)
    {
        if (checkGamesConnection()) {
            getActivity().startActivityForResult(Games.Leaderboards.getLeaderboardIntent(
                mGoogleApiClient, leaderboardId), REQUEST_LEADERBOARD);
        } else {
            Log.i(TAG, "Not connected to Google Games -> connecting, in response to showLeaderboard");
            signInClicked();
        }
    }

    private void submitScore(String leaderboardId, long score)
    {
        if (checkGamesConnection()) {
            Games.Leaderboards.submitScore(mGoogleApiClient, leaderboardId, score);
        } else {
            if (mScoreToSendWhenConnected < score ||
                !mScoreToSendWhenConnectedLeaderboard.equals(leaderboardId)) {
                mScoreToSendWhenConnected = score;
                mScoreToSendWhenConnectedLeaderboard = leaderboardId;
            }
            Log.w(TAG, "Not connected to Google Games, scheduling leaderboard score submission for later");
        }
    }

    private void refreshCurrentScore(String leaderboardId)
    {
        if (checkGamesConnection()) {
            final String saveLeaderboardId = leaderboardId;
            Games.Leaderboards.loadCurrentPlayerLeaderboardScore(mGoogleApiClient,
                leaderboardId,
                LeaderboardVariant.TIME_SPAN_ALL_TIME,
                LeaderboardVariant.COLLECTION_PUBLIC).
                setResultCallback(new ResultCallback<LoadPlayerScoreResult>() {
                    public void onResult(LoadPlayerScoreResult result)
                    {
                        if (!result.getStatus().isSuccess()) {
                            Log.w(TAG, "Failed to get own leaderboard score.");
                            return;
                        }
                        long myScore =
                            result.getScore() != null ?
                            result.getScore().getRawScore() : 0;
                        messageSend(new String[]{"best-score", saveLeaderboardId, Long.toString(myScore)});
                    }
                });
        } else {
            Log.w(TAG, "Not connected to Google Games, cannot get leaderboard position");
        }
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 1 && parts[0].equals("google-play-games-initialize")) {
            initialize();
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("get-best-score")) {
            mDefaultLeaderboardToRefresh = parts[1];
            /* refresh score now, if possible */
            if (checkGamesConnection()) {
                refreshCurrentScore(mDefaultLeaderboardToRefresh);
            }
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("achievement")) {
            achievement(parts[1]);
            return true;
        } else
        if (parts.length == 3 && parts[0].equals("submit-score")) {
            submitScore(parts[1], Long.parseLong(parts[2]));
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("show") && parts[1].equals("achievements")) {
            showAchievements();
            return true;
        } else
        if (parts.length == 3 && parts[0].equals("show") && parts[1].equals("leaderboard")) {
            showLeaderboard(parts[2]);
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("google-sign-in") && parts[1].equals("true")) {
            signInClicked();
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("google-sign-in") && parts[1].equals("false")) {
            signOutClicked();
            return true;
        } else {
            return false;
        }
    }
}
