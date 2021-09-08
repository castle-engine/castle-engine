/* -*- tab-width: 4 -*- */

/*
  Copyright 2018-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

package net.sourceforge.castleengine;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.lang.IllegalStateException;

import androidx.annotation.NonNull;
import android.content.Intent;
import android.content.IntentSender;
import android.util.Log;
import android.app.Activity;
import android.app.AlertDialog;

import com.google.android.gms.auth.api.Auth;
import com.google.android.gms.auth.api.signin.GoogleSignIn;
import com.google.android.gms.auth.api.signin.GoogleSignInAccount;
import com.google.android.gms.auth.api.signin.GoogleSignInClient;
import com.google.android.gms.auth.api.signin.GoogleSignInOptions;
import com.google.android.gms.auth.api.signin.GoogleSignInResult;
import com.google.android.gms.auth.api.signin.GoogleSignInApi;
import com.google.android.gms.common.api.ApiException;
import com.google.android.gms.games.AchievementsClient;
import com.google.android.gms.games.AnnotatedData;
import com.google.android.gms.games.EventsClient;
import com.google.android.gms.games.Games;
import com.google.android.gms.games.GamesClient;
import com.google.android.gms.games.LeaderboardsClient;
import com.google.android.gms.games.Player;
import com.google.android.gms.games.PlayersClient;
import com.google.android.gms.games.event.Event;
import com.google.android.gms.games.event.EventBuffer;
import com.google.android.gms.tasks.OnCompleteListener;
import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.android.gms.tasks.Task;

import ${QUALIFIED_NAME}.R;

/**
 * Integration of Google Games (achievements, leaderboards and more) with
 * Castle Game Engine.
 */
public class ServiceGooglePlayGames extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceGooglePlayGames";

    private static final int REQUEST_SIGN_IN = 9001;
    private static final int REQUEST_ACHIEVEMENTS = 9101;
    private static final int REQUEST_LEADERBOARD = 9102;
    private static final int REQUEST_SAVED_GAMES = 9103;

    private static final int STATUS_SIGNED_OUT = 0;
    private static final int STATUS_SIGNING_IN = 1;
    private static final int STATUS_SIGNED_IN = 2;
    private static final int STATUS_SIGNING_OUT = 3;

    // All these 3 booleans are indepedent.
    private boolean
        // Pascal code called initialize() (otherwise we don't want to talk to Google servers at all)
        initialized,
        // Application is in foreground (between onResume and onPause)
        resumed,
        // Pascal code indicated user wants to be signed-in now.
        // We should strive to change mStatus to STATUS_SIGNED_IN.
        wantsSignIn;

    // was initialize() last called with saveGames
    boolean mSaveGames;

    private GoogleSignInAccount account;

    public ServiceGooglePlayGames(MainActivity activity)
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
            logError(CATEGORY, "Initializing again is not supported (we are already initialized, and changing autoStartSignInFlow/saveGames at this point is not supported)");
            return;
        }

        String appId = getActivity().getResources().getString(R.string.app_id);
        if (appId.equals("")) {
            logError(CATEGORY, "You must define Google Play Games id of your game in CastleEngineManifest.xml, like <google_play_services app_id=\"xxxx\" />. You get this id after creating Google Game Services for your game in Google Developer Console. Without this, GooglePlayGames integration cannot be initialized.");
            return;
        }

        initialized = true;
        mSaveGames = saveGames;

        if (autoStartSignInFlow) {
            wantsSignIn = true;
        }
        signIn();
    }

    /* Sign-in to Google Play Games, if the application is now resumed, wantsSignIn, initialized.

       You can call this even when not "wantsSignIn && resumed && initialized",
       it will just silently exit.
       Otherwise this immediately sets mStatus = STATUS_SIGNING_IN,
       and starts the process to change it to mStatus = STATUS_SIGNED_IN eventually.

       Prefers to use existing signed-in user (so e.g. no sign-in may be necessary
       if you pause+resume the application, that *may* keep the user signed-in).
       Prefers the silent sign-in.
       But fallback on interactive sign-in when necessary.

       Note that wantsSignIn implies also initialized.
       We cannot have wantsSignIn == true, with initialized == false.

       Follows https://developers.google.com/games/services/android/signin
    */
    private void signIn()
    {
        if (!(initialized && wantsSignIn && resumed)) {
            return;
        }

        logInfo(CATEGORY, "Starting sign-in process.");
        setStatus(STATUS_SIGNING_IN);

        GoogleSignInOptions signInOptions;
        if (mSaveGames) {
            signInOptions =
                new GoogleSignInOptions.Builder(GoogleSignInOptions.DEFAULT_GAMES_SIGN_IN)
                    .requestScopes(Games.SCOPE_GAMES_SNAPSHOTS)
                    .build();
        } else {
            signInOptions = GoogleSignInOptions.DEFAULT_GAMES_SIGN_IN;
        }

        GoogleSignInAccount lastAccount = GoogleSignIn.getLastSignedInAccount(getActivity());
        if (GoogleSignIn.hasPermissions(account, signInOptions.getScopeArray())) {
            // Already signed in.
            // The signed in account is stored in the 'account' variable.
            GoogleSignInAccount account = lastAccount;
            successfullSignIn();
        } else {
            // Haven't been signed-in before. Try the silent sign-in first.
            final GoogleSignInClient signInClient = GoogleSignIn.getClient(getActivity(), signInOptions);
            signInClient
                .silentSignIn()
                .addOnCompleteListener(
                    getActivity(),
                    new OnCompleteListener<GoogleSignInAccount>() {
                        @Override
                        public void onComplete(@NonNull Task<GoogleSignInAccount> task) {
                            if (task.isSuccessful()) {
                                // The signed in account is stored in the task's result.
                                account = task.getResult();
                                successfullSignIn();
                            } else {
                                // Player will need to sign-in explicitly using via UI.
                                Intent intent = signInClient.getSignInIntent();
                                getActivity().startActivityForResult(intent, REQUEST_SIGN_IN);
                            }
                        }
                    });
        }
    }

    @Override
    public void onResume()
    {
        resumed = true;
        signIn();
    }

    @Override
    public void onPause()
    {
        resumed = false;
    }

    /**
     * Return if we are initialized and mStatus is STATUS_SIGNED_IN.
     * Also does some basic checks. */
    private boolean checkGamesConnection()
    {
        if (mStatus != STATUS_SIGNED_OUT && !initialized) {
            logWarning(CATEGORY, "Weird state: when status is not STATUS_SIGNED_OUT, initialized should be true");
        }

        return initialized && mStatus == STATUS_SIGNED_IN;
    }

    // Are we signed in (use STATUS_xxx constants).
    private int mStatus = STATUS_SIGNED_OUT;

    // Set mStatus, and send new mStatus to Pascal code.
    private void setStatus(int value)
    {
        if (mStatus != value) {
            mStatus = value;
            messageSend(new String[]{"game-service-status", Long.toString(value)});
        }
    }

    // Score to send to a leaderboard, once we're connected.
    // Use only if > 0.
    private long mScoreToSendWhenConnected;
    private String mScoreToSendWhenConnectedLeaderboard;

    private abstract class OnConnectedFinish {
        public abstract void run();
    }

    /* An action to make when sign-in succeeds next time. */
    private OnConnectedFinish mOnConnectedFinish = null;

    /* Signed-in OK, account set OK.
       Set mStatus to STATUS_SIGNED_IN only this way. */
    private void successfullSignIn()
    {
        // The player is signed in.
        // We can now hide the sign-in button.
        setStatus(STATUS_SIGNED_IN);

        GamesClient gamesClient = Games.getGamesClient(getActivity(), account);
        gamesClient.setViewForPopups(getActivity().findViewById(android.R.id.content));

        if (mScoreToSendWhenConnected > 0) {
            if (checkGamesConnection()) {
                /* TODO:

                Games.Leaderboards.submitScore(mGoogleApiClient,
                    mScoreToSendWhenConnectedLeaderboard, mScoreToSendWhenConnected);
                logInfo(CATEGORY, "Submitting scheduled score " + mScoreToSendWhenConnected);
                mScoreToSendWhenConnected = 0;
                mScoreToSendWhenConnectedLeaderboard = null;
                */
            } else {
                logError(CATEGORY, "Cannot submit scheduled score, we are not connected inside onConnected - weird, unless the connection broke immediately");
            }
        }

        if (mOnConnectedFinish != null) {
            mOnConnectedFinish.run();
            mOnConnectedFinish = null;
        }
    }

    private boolean mSignInClicked = false;

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent intent)
    {
        super.onActivityResult(requestCode, resultCode, intent);

        if (requestCode == REQUEST_SIGN_IN) {
            GoogleSignInResult result = Auth.GoogleSignInApi.getSignInResultFromIntent(intent);
            if (result.isSuccess()) {
                // The signed in account is stored in the result.
                account = result.getSignInAccount();
                successfullSignIn();
            } else {
                String message = result.getStatus().getStatusMessage();
                if (message == null || message.isEmpty()) {
                    message = "Cannot sign-in to Google Play Games";
                }
                // TODO: should this be AlertDialog? Not logError? Should end-user see this?
                new AlertDialog.Builder(getActivity()).setMessage(message)
                    .setNeutralButton("OK", null).show();
            }
        }

        /* TODO:

        if (requestCode == REQUEST_SAVED_GAMES) {
            /* adapted from https://developers.google.com/games/services/android/savedgames * /
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
                        logWarning(CATEGORY, "Received REQUEST_SAVED_GAMES, with RESULT_OK, but intent has no extra");
                        messageSend(new String[]{"chosen-save-game-cancel"});
                    }
                } else {
                    logWarning(CATEGORY, "Received REQUEST_SAVED_GAMES, with RESULT_OK, but intent is null");
                    messageSend(new String[]{"chosen-save-game-cancel"});
                }
            } else {
                messageSend(new String[]{"chosen-save-game-cancel"});
            }
        }

        */
    }

    private void signInClicked(OnConnectedFinish onConnectedFinish)
    {
        if (mStatus == STATUS_SIGNED_OUT && initialized) {
            mOnConnectedFinish = onConnectedFinish;
            wantsSignIn = true;
            signIn();
        }
    }

    private void signOutClicked()
    {
        /* don't act when inside sign-out process, or not initialized */
        if (mStatus == STATUS_SIGNING_OUT || !initialized) {
            return;
        }

        wantsSignIn = false;
        mOnConnectedFinish = null;
        setStatus(STATUS_SIGNING_OUT);

        GoogleSignInClient signInClient = GoogleSignIn.getClient(getActivity(),
             GoogleSignInOptions.DEFAULT_GAMES_SIGN_IN);
        signInClient.signOut().addOnCompleteListener(
            getActivity(),
            new OnCompleteListener<Void>() {
                @Override
                public void onComplete(@NonNull Task<Void> task) {
                    if (!task.isSuccessful()) {
                        logWarning(CATEGORY, "Failed to sign out from Google Play Games. Changing our state anyway, to not communicate with Google Play Games.");
                    }
                    setStatus(STATUS_SIGNED_OUT);
                }
            });
    }

    private void showAchievements()
    {
        if (checkGamesConnection()) {
            Games.getAchievementsClient(getActivity(), GoogleSignIn.getLastSignedInAccount(getActivity()))
                  .getAchievementsIntent()
                  .addOnSuccessListener(new OnSuccessListener<Intent>() {
                    @Override
                    public void onSuccess(Intent intent) {
                        getActivity().startActivityForResult(intent, REQUEST_ACHIEVEMENTS);
                    }
                  });
        } else {
            logInfo(CATEGORY, "Not connected to Google Games -> connecting, in response to showAchievements");
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
            Games.getAchievementsClient(getActivity(), GoogleSignIn.getLastSignedInAccount(getActivity()))
                .unlock(achievementId);
        } else {
            logWarning(CATEGORY, "Achievement unlocked, but not connected to Google Games, ignoring");
        }
    }

    private void showSaveGames(final String title, final boolean allowAddButton, final boolean allowDelete, final int maxNumberOfSaveGamesToShow)
    {
        /* TODO:

        if (checkGamesConnection()) {
            int realMaxNumberOfSaveGamesToShow;
            if (maxNumberOfSaveGamesToShow < 0) {
                /* For some reason (bug?), Google Play Games dialog to choose
                   the saved game @italic(does not) show "new save game" buton
                   when this is Snapshots.DISPLAY_LIMIT_NONE.
                   It behaves then like AllowAddButton is always @false.
                   So instead, we use some ridiculously large number for
                   MaxNumberOfSaveGamesToShow. * /
                realMaxNumberOfSaveGamesToShow = 1000;
            } else {
                realMaxNumberOfSaveGamesToShow = maxNumberOfSaveGamesToShow;
            }
            Intent savedGamesIntent = Games.Snapshots.getSelectSnapshotIntent(mGoogleApiClient,
                title, allowAddButton, allowDelete, realMaxNumberOfSaveGamesToShow);
            getActivity().startActivityForResult(savedGamesIntent, REQUEST_SAVED_GAMES);
        } else {
            logInfo(CATEGORY, "Not connected to Google Games -> connecting, in response to showSaveGames");
            signInClicked(new OnConnectedFinish () {
                public void run() { showSaveGames(title, allowAddButton, allowDelete, maxNumberOfSaveGamesToShow); }
            });
        }

        */
    }

    // Not configurable from Pascal *for now*.
//    private static final int conflictResolution = Snapshots.RESOLUTION_POLICY_LONGEST_PLAYTIME;

    // The savegame contents is converted from/to a string using this encoding.
    // Not configurable from Pascal *for now*.
//    private static final String saveGameEncoding = "UTF-8";

    /* Make a log, and messageSend, that loading savegame failed. */
    private final void saveGameLoadingError(String errorStr)
    {
        logError(CATEGORY, errorStr);
        messageSend(new String[]{"save-game-loaded", "false", errorStr});
    }

    private void saveGameLoad(final String saveGameName)
    {
        /* TODO:

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
                           "GoogleApiClient is not connected yet." * /
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

        */
    }

    private void saveGameSave(final String saveGameName, final String saveGameContents, final String description, final long playedTimeMillis)
    {
        /* TODO:

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
                           "GoogleApiClient is not connected yet." * /
                        result = null;
                    }

                    // Check the result of the open operation
                    if (result != null) {
                        if (result.getStatus().isSuccess()) {
                            Snapshot snapshot = result.getSnapshot();
                            try {
                                snapshot.getSnapshotContents().writeBytes(saveGameContents.getBytes(saveGameEncoding));
                            } catch (UnsupportedEncodingException e) {
                                logError(CATEGORY, "Error while saving a save game, encoding " + saveGameEncoding + " unsupported: " + e.getMessage());
                            }

                            // Create the change operation
                            SnapshotMetadataChange metadataChange = new SnapshotMetadataChange.Builder()
                                    .setDescription(description)
                                    .setPlayedTimeMillis(playedTimeMillis)
                                    .build();

                            // Commit the operation
                            commitAndCloseWatchingResult(snapshot, metadataChange);
                        } else {
                            logError(CATEGORY, "Error while opening a save game for writing (" +
                              result.getStatus().getStatusCode() + "): " +
                              result.getStatus().getStatusMessage());
                        }
                    } else {
                        logError(CATEGORY, "Google Play Games disconneted while trying to save savegame");
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
                        logError(CATEGORY, "Google Play Games error when saving the game (" +
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
            logError(CATEGORY, "Not connected to Google Play Games, cannot save savegame.");
        }

        */
    }

    private void showLeaderboard(final String leaderboardId)
    {
        /* TODO:

        if (checkGamesConnection()) {
            getActivity().startActivityForResult(Games.Leaderboards.getLeaderboardIntent(
                mGoogleApiClient, leaderboardId), REQUEST_LEADERBOARD);
        } else {
            logInfo(CATEGORY, "Not connected to Google Games -> connecting, in response to showLeaderboard");
            signInClicked(new OnConnectedFinish () {
                public void run() { showLeaderboard(leaderboardId); }
            });
        }

        */
    }

    private void submitScore(String leaderboardId, long score)
    {
        /* TODO:

        if (checkGamesConnection()) {
            Games.Leaderboards.submitScore(mGoogleApiClient, leaderboardId, score);
        } else {
            if (mScoreToSendWhenConnected < score ||
                !mScoreToSendWhenConnectedLeaderboard.equals(leaderboardId)) {
                mScoreToSendWhenConnected = score;
                mScoreToSendWhenConnectedLeaderboard = leaderboardId;
            }
            logWarning(CATEGORY, "Not connected to Google Games, scheduling leaderboard score submission for later");
        }

        */
    }

    private void requestPlayerBestScore(String leaderboardId)
    {
        /* TODO:

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
                            logWarning(CATEGORY, "Failed to get own leaderboard score.");
                            return;
                        }
                        long myScore =
                            result.getScore() != null ?
                            result.getScore().getRawScore() : 0;
                        messageSend(new String[]{"best-score", saveLeaderboardId, Long.toString(myScore)});
                    }
                });
        } else {
            logWarning(CATEGORY, "Not connected to Google Games, cannot get leaderboard position");
        }

        */
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 3 && parts[0].equals("game-service-initialize")) {
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
        if (parts.length == 2 && parts[0].equals("game-service-sign-in") && stringToBoolean(parts[1])) {
            signInClicked(null);
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("game-service-sign-in") && !stringToBoolean(parts[1])) {
            signOutClicked();
            return true;
        } else {
            return false;
        }
    }
}
