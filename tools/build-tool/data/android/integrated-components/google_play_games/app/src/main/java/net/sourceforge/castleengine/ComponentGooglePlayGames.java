/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.lang.IllegalStateException;

import android.content.Intent;
import android.content.IntentSender;
import android.util.Log;
import android.app.Activity;
import android.os.Bundle;
import android.os.AsyncTask;

import com.google.android.gms.common.ConnectionResult;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.ResultCallback;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.drive.Drive;
import com.google.android.gms.games.Games;
import com.google.android.gms.games.leaderboard.LeaderboardVariant;
import com.google.android.gms.games.leaderboard.Leaderboards.LoadPlayerScoreResult;
import com.google.android.gms.games.snapshot.Snapshot;
import com.google.android.gms.games.snapshot.SnapshotMetadata;
import com.google.android.gms.games.snapshot.Snapshots;
import com.google.android.gms.games.snapshot.SnapshotMetadataChange;

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
    private static final int REQUEST_SIGN_IN = 9001;
    private static final int REQUEST_ACHIEVEMENTS = 9101;
    private static final int REQUEST_LEADERBOARD = 9102;
    private static final int REQUEST_SAVED_GAMES = 9103;

    private boolean initialized, scheduledStart;

    private GoogleApiClient mGoogleApiClient;

    public ComponentGooglePlayGames(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "google-play-games";
    }

    private void initialize(boolean autoStartSignInFlow, boolean saveGames)
    {
        if (initialized) {
            return;
        }

        String appId = getActivity().getResources().getString(R.string.app_id);
        if (appId.equals("")) {
            Log.e(TAG, "You must define Google Play Games id of your game in CastleEngineManifest.xml, like <google_play_services app_id=\"xxxx\" />. You get this id after creating Google Game Services for your game in Google Developer Console. Without this, GooglePlayGames integration cannot be initialized.");
            return;
        }

        mAutoStartSignInFlow = autoStartSignInFlow;

        // Create the Google Api Client with access to the Play Game services
        GoogleApiClient.Builder builder = new GoogleApiClient.Builder(getActivity())
            .addConnectionCallbacks(this)
            .addOnConnectionFailedListener(this)
            .addApi(Games.API).addScope(Games.SCOPE_GAMES);
        if (saveGames) {
            builder = builder.addApi(Drive.API).addScope(Drive.SCOPE_APPFOLDER);
        }
        mGoogleApiClient = builder.build();
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
            messageSend(new String[]{"google-sign-in-status", booleanToString(mGoogleSignedIn)});
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

        Games.setViewForPopups(mGoogleApiClient, getActivity().findViewById(android.R.id.content));

        // The player is signed in.
        // We can now hide the sign-in button.
        setGoogleSignedIn(true);

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

        if (mOnConnectedFinish != null) {
            mOnConnectedFinish.run();
            mOnConnectedFinish = null;
        }
    }

    private abstract class OnConnectedFinish {
        public abstract void run();
    }

    private boolean mResolvingConnectionFailure = false;
    private boolean mAutoStartSignInFlow = true;
    private boolean mSignInClicked = false;
    /* When mSignInClicked, this may be non-null to indicate an action to make
     * when sign-in succeeds. */
    private OnConnectedFinish mOnConnectedFinish = null;
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
            // leave mOnConnectedFinish non-null, we may yet sign-in
            mResolvingConnectionFailure = true;

            // Attempt to resolve the connection failure.
            if (!resolveConnectionFailure(getActivity(),
                    mGoogleApiClient, connectionResult, REQUEST_SIGN_IN)) {
                mResolvingConnectionFailure = false;
                mOnConnectedFinish = null;
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
            Log.i(TAG, "Received activity result: Google Play Games SIGN_IN");
            mSignInClicked = false;
            mResolvingConnectionFailure = false;
            if (resultCode == Activity.RESULT_OK) {
                if (mGoogleApiClient != null) {
                    mGoogleApiClient.connect();
                } else {
                    /* reproducible on Sony Xperia phone C5305 (from P) with old Android Os
                       and new Google Play Services. The services do some wild things
                       there anyway: OpenSnapshotResult result code is sometimes 16
                       (which is not documented as valid result code). */
                    Log.w(TAG, "mGoogleApiClient == null when we received Google Play Games sign in. Indicates that connection to Google Play Games was reached after Java activity died and was recreated.");
                }
            } else {
                Log.w(TAG, "Unable to sign in to Google Games.");
            }
        }

        if (requestCode == REQUEST_SAVED_GAMES) {
            /* adapted from https://developers.google.com/games/services/android/savedgames */
            if (resultCode == Activity.RESULT_OK) {
                if (intent != null) {
                    if (intent.hasExtra(Snapshots.EXTRA_SNAPSHOT_METADATA)) {
                        // Load a snapshot.
                        SnapshotMetadata snapshotMetadata = (SnapshotMetadata)
                                intent.getParcelableExtra(Snapshots.EXTRA_SNAPSHOT_METADATA);
                        String currentSaveName = snapshotMetadata.getUniqueName();
                        messageSend(new String[]{"chosen-save-game", currentSaveName});
                    } else
                    if (intent.hasExtra(Snapshots.EXTRA_SNAPSHOT_NEW)) {
                        messageSend(new String[]{"chosen-save-game-new"});
                    } else {
                        Log.w(TAG, "Received REQUEST_SAVED_GAMES, with RESULT_OK, but intent has no extra");
                        messageSend(new String[]{"chosen-save-game-cancel"});
                    }
                } else {
                    Log.w(TAG, "Received REQUEST_SAVED_GAMES, with RESULT_OK, but intent is null");
                    messageSend(new String[]{"chosen-save-game-cancel"});
                }
            } else {
                messageSend(new String[]{"chosen-save-game-cancel"});
            }
        }
    }

    private void signInClicked(OnConnectedFinish onConnectedFinish)
    {
        /* don't act when inside sign-out process, or not initialized */
        if (mDuringSignOut || !initialized) {
            return;
        }

        mSignInClicked = true;
        mOnConnectedFinish = onConnectedFinish;
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
        mOnConnectedFinish = null;
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
            signInClicked(new OnConnectedFinish () {
                public void run() { showAchievements(); }
            });
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

    private void showSaveGames(final String title, final boolean allowAddButton, final boolean allowDelete, final int maxNumberOfSaveGamesToShow)
    {
        if (checkGamesConnection()) {
            int realMaxNumberOfSaveGamesToShow;
            if (maxNumberOfSaveGamesToShow < 0) {
                /* For some reason (bug?), Google Play Games dialog to choose
                   the saved game @italic(does not) show "new save game" buton
                   when this is Snapshots.DISPLAY_LIMIT_NONE.
                   It behaves then like AllowAddButton is always @false.
                   So instead, we use some ridiculously large number for
                   MaxNumberOfSaveGamesToShow. */
                realMaxNumberOfSaveGamesToShow = 1000;
            } else {
                realMaxNumberOfSaveGamesToShow = maxNumberOfSaveGamesToShow;
            }
            Intent savedGamesIntent = Games.Snapshots.getSelectSnapshotIntent(mGoogleApiClient,
                title, allowAddButton, allowDelete, realMaxNumberOfSaveGamesToShow);
            getActivity().startActivityForResult(savedGamesIntent, REQUEST_SAVED_GAMES);
        } else {
            Log.i(TAG, "Not connected to Google Games -> connecting, in response to showSaveGames");
            signInClicked(new OnConnectedFinish () {
                public void run() { showSaveGames(title, allowAddButton, allowDelete, maxNumberOfSaveGamesToShow); }
            });
        }
    }

    // Not configurable from Pascal *for now*.
    private static final int conflictResolution = Snapshots.RESOLUTION_POLICY_LONGEST_PLAYTIME;

    // The savegame contents is converted from/to a string using this encoding.
    // Not configurable from Pascal *for now*.
    private static final String saveGameEncoding = "UTF-8";

    /* Make a log, and messageSend, that loading savegame failed. */
    private final void saveGameLoadingError(String errorStr)
    {
        Log.e(TAG, errorStr);
        messageSend(new String[]{"save-game-loaded", "false", errorStr});
    }

    private void saveGameLoad(final String saveGameName)
    {
        if (checkGamesConnection()) {
            AsyncTask<Void, Void, Snapshots.OpenSnapshotResult> task =
                new AsyncTask<Void, Void, Snapshots.OpenSnapshotResult> ()
            {
                @Override
                protected Snapshots.OpenSnapshotResult doInBackground(Void... params) {
                    boolean createIfNotFound = true; // not configurable from Pascal for now
                    // Open the saved game using its name.
                    try {
                        return Games.Snapshots.open(mGoogleApiClient,
                            saveGameName, createIfNotFound, conflictResolution).await();
                    } catch (IllegalStateException e) {
                        /* Snapshots.open() can always fail with
                           "GoogleApiClient is not connected yet." */
                        return null;
                    }
                }

                @Override
                protected void onPostExecute(Snapshots.OpenSnapshotResult result) {
                    if (result != null) {
                        // Check the result of the open operation
                        if (result.getStatus().isSuccess()) {
                            Snapshot snapshot = result.getSnapshot();
                            // Read the byte content of the saved game.
                            try {
                                byte[] saveGameBytes;
                                saveGameBytes = snapshot.getSnapshotContents().readFully();
                                String saveGameStr = new String(saveGameBytes, saveGameEncoding);
                                messageSend(new String[]{"save-game-loaded", "true", saveGameStr});
                            } catch (IOException e) {
                                saveGameLoadingError("Google Play Games error when reading snapshot: " + e.getMessage());
                            }
                        } else {
                            saveGameLoadingError("Google Play Games error when loading save game (" +
                              result.getStatus().getStatusCode() + "): " +
                              result.getStatus().getStatusMessage());
                        }
                    } else {
                        saveGameLoadingError("Google Play Games disconneted while trying to load savegame");
                    }
                }
            };

            task.execute();
        } else {
            saveGameLoadingError("Not connected to Google Play Games");
        }
    }

    private void saveGameSave(final String saveGameName, final String saveGameContents, final String description, final long playedTimeMillis)
    {

        if (checkGamesConnection()) {
            AsyncTask<Void, Void, Void> task = new AsyncTask<Void, Void, Void> ()
            {
                @Override
                protected Void doInBackground(Void... params) {
                    boolean createIfNotFound = true;

                    // Open the saved game using its name.
                    // This is the *1st* operation that takes time, and should therefore be in a thread!!
                    Snapshots.OpenSnapshotResult result;
                    try {
                        result = Games.Snapshots.open(mGoogleApiClient,
                            saveGameName, createIfNotFound, conflictResolution).await();
                    } catch (IllegalStateException e) {
                        /* Snapshots.open() can always fail with
                           "GoogleApiClient is not connected yet." */
                        result = null;
                    }

                    // Check the result of the open operation
                    if (result != null) {
                        if (result.getStatus().isSuccess()) {
                            Snapshot snapshot = result.getSnapshot();
                            try {
                                snapshot.getSnapshotContents().writeBytes(saveGameContents.getBytes(saveGameEncoding));
                            } catch (UnsupportedEncodingException e) {
                                Log.e(TAG, "Error while saving a save game, encoding " + saveGameEncoding + " unsupported: " + e.getMessage());
                            }

                            // Create the change operation
                            SnapshotMetadataChange metadataChange = new SnapshotMetadataChange.Builder()
                                    .setDescription(description)
                                    .setPlayedTimeMillis(playedTimeMillis)
                                    .build();

                            // Commit the operation
                            commitAndCloseWatchingResult(snapshot, metadataChange);
                        } else {
                            Log.e(TAG, "Error while opening a save game for writing (" +
                              result.getStatus().getStatusCode() + "): " +
                              result.getStatus().getStatusMessage());
                        }
                    } else {
                        Log.e(TAG, "Google Play Games disconneted while trying to save savegame");
                    }

                    return null;
                }

                private void commitAndCloseWatchingResult(Snapshot snapshot, SnapshotMetadataChange metadataChange)
                {
                    PendingResult<Snapshots.CommitSnapshotResult> pending =
                      Games.Snapshots.commitAndClose(mGoogleApiClient, snapshot, metadataChange);

                    // This is the *2nd* operation that takes time, and should therefore be in a thread!!
                    Snapshots.CommitSnapshotResult result = pending.await();

                    if (!result.getStatus().isSuccess()) {
                        Log.e(TAG, "Google Play Games error when saving the game (" +
                            result.getStatus().getStatusCode() + "): " +
                            result.getStatus().getStatusMessage());
                    }
                }

                @Override
                protected void onPostExecute(Void result) {
                    // Nothing. Right now, we don't communicate to main thread
                    // (or native code) whether save succeeded or not.
                }
            };

            task.execute();
        } else {
            Log.e(TAG, "Not connected to Google Play Games, cannot save savegame.");
        }
    }

    private void showLeaderboard(final String leaderboardId)
    {
        if (checkGamesConnection()) {
            getActivity().startActivityForResult(Games.Leaderboards.getLeaderboardIntent(
                mGoogleApiClient, leaderboardId), REQUEST_LEADERBOARD);
        } else {
            Log.i(TAG, "Not connected to Google Games -> connecting, in response to showLeaderboard");
            signInClicked(new OnConnectedFinish () {
                public void run() { showLeaderboard(leaderboardId); }
            });
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

    private void requestPlayerBestScore(String leaderboardId)
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
        if (parts.length == 3 && parts[0].equals("google-play-games-initialize")) {
            initialize(stringToBoolean(parts[1]), stringToBoolean(parts[2]));
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("request-player-best-score")) {
            requestPlayerBestScore(parts[1]);
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
        if (parts.length == 6 && parts[0].equals("show") && parts[1].equals("save-games")) {
            showSaveGames(parts[2], stringToBoolean(parts[3]), stringToBoolean(parts[4]), Integer.parseInt(parts[5]));
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("save-game-load")) {
            saveGameLoad(parts[1]);
            return true;
        } else
        if (parts.length == 5 && parts[0].equals("save-game-save")) {
            saveGameSave(parts[1], parts[2], parts[3], Long.parseLong(parts[4]));
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("google-sign-in") && stringToBoolean(parts[1])) {
            signInClicked(null);
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("google-sign-in") && !stringToBoolean(parts[1])) {
            signOutClicked();
            return true;
        } else {
            return false;
        }
    }
}
