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

import androidx.annotation.NonNull;

import android.content.Intent;
import android.app.Activity;
import android.app.AlertDialog;

import com.google.android.gms.common.Scopes;
import com.google.android.gms.common.api.Scope;
import com.google.android.gms.auth.api.Auth;
import com.google.android.gms.auth.api.signin.GoogleSignIn;
import com.google.android.gms.auth.api.signin.GoogleSignInAccount;
import com.google.android.gms.auth.api.signin.GoogleSignInClient;
import com.google.android.gms.auth.api.signin.GoogleSignInOptions;
import com.google.android.gms.auth.api.signin.GoogleSignInResult;
import com.google.android.gms.games.AchievementsClient;
import com.google.android.gms.games.SnapshotsClient;
import com.google.android.gms.games.snapshot.Snapshot;
import com.google.android.gms.games.snapshot.SnapshotMetadata;
import com.google.android.gms.games.snapshot.SnapshotMetadataChange;
import com.google.android.gms.games.AnnotatedData;
import com.google.android.gms.games.Games;
import com.google.android.gms.games.GamesClient;
import com.google.android.gms.games.LeaderboardsClient;
import com.google.android.gms.games.leaderboard.LeaderboardScore;
import com.google.android.gms.games.leaderboard.LeaderboardVariant;
import com.google.android.gms.tasks.OnCompleteListener;
import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.android.gms.tasks.Task;

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

        String appId =
            //getActivity().getResources().getString(R.string.app_id);
            // Simpler to just get the value by CGE services macro expansion
            "${ANDROID.GOOGLE_PLAY_GAMES.APP_ID}";
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
                    .requestScopes(
                        //Drive.SCOPE_APPFOLDER
                        // Non-deprecated version, following https://stackoverflow.com/questions/60543379/google-play-games-saved-games-deprecated/62321726#62321726
                        new Scope(Scopes.DRIVE_APPFOLDER)
                    )
                    .build();
        } else {
            signInOptions = GoogleSignInOptions.DEFAULT_GAMES_SIGN_IN;
        }

        final Activity a = getActivity();
        GoogleSignInAccount lastAccount = GoogleSignIn.getLastSignedInAccount(a);
        if (lastAccount != null &&
            GoogleSignIn.hasPermissions(lastAccount, signInOptions.getScopeArray())) {
            successfullSignIn();
        } else {
            // Haven't been signed-in before. Try the silent sign-in first.
            final GoogleSignInClient signInClient = GoogleSignIn.getClient(a, signInOptions);
            signInClient
                .silentSignIn()
                .addOnCompleteListener(a,
                    new OnCompleteListener<GoogleSignInAccount>() {
                        @Override
                        public void onComplete(@NonNull Task<GoogleSignInAccount> task) {
                            if (task.isSuccessful()) {
                                successfullSignIn();
                            } else {
                                // Player will need to sign-in explicitly using via UI.
                                Intent intent = signInClient.getSignInIntent();
                                a.startActivityForResult(intent, REQUEST_SIGN_IN);
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
     * Return non-null if we are initialized, mStatus is STATUS_SIGNED_IN,
     * and we have a valid account.
     * Does all checks required by all Google functions that we're connected.
     * The returned account is ready to be passed where you would usually pass
     * "GoogleSignIn.getLastSignedInAccount" result.
     */
    private GoogleSignInAccount checkGamesConnection()
    {
        if (mStatus != STATUS_SIGNED_OUT && !initialized) {
            logWarning(CATEGORY, "Weird state: when status is not STATUS_SIGNED_OUT, initialized should be true");
        }

        if (initialized && mStatus == STATUS_SIGNED_IN) {
            /* This method calls and checks GoogleSignIn.getLastSignedInAccount,
             * as it can return null despite other checks,
             * and then methods like getAchievementsClient would raise
             * NullPointerException.
             */
            return GoogleSignIn.getLastSignedInAccount(getActivity());
        } else {
            return null;
        }
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

        GoogleSignInAccount account = checkGamesConnection();
        if (account == null) {
            logError(CATEGORY, "Weird state: Right when successfullSignIn is called, checkGamesConnection should return non-null");
            return;
        }

        GamesClient gamesClient = Games.getGamesClient(getActivity(), account);
        gamesClient.setViewForPopups(getActivity().findViewById(android.R.id.content));

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
                successfullSignIn();
            } else {
                setStatus(STATUS_SIGNED_OUT);

                String message = result.getStatus().getStatusMessage();
                if (message == null || message.isEmpty()) {
                    message = "Cannot sign-in to Google Play Games";
                }

                // Do not show, better to let Pascal application show UI about this.
                /*
                new AlertDialog.Builder(getActivity()).setMessage(message)
                    .setNeutralButton("OK", null)
                    .show();
                */

                logError(CATEGORY, "Cannot sign-in to Google Play Games (extra message: " + message + ")");

                /* Setting wantsSignIn, to avoid a loop caused by onResume
                   (which seems to always happen when this occurs)
                   trying to sign-in again and again.
                   Testcase: just try application without valid Google Play Games
                   app_id, so that it always fails. */
                wantsSignIn = false;
            }
        }

        if (requestCode == REQUEST_SAVED_GAMES) {
            /* https://developers.google.com/games/services/v1/android/savedgames */
            if (resultCode == Activity.RESULT_OK) {
                if (intent != null) {
                    if (intent.hasExtra(SnapshotsClient.EXTRA_SNAPSHOT_METADATA)) {
                        // Load a snapshot.

                        /* Google deprecated getParcelableExtra in favor of safer
                           https://developer.android.com/reference/android/content/Intent#getParcelableExtra(java.lang.String,%20java.lang.Class%3CT%3E)
                           But the new API is only available on new devices,
                           so in reality one just has to either

                           - use old API
                           - or use both old and new APIs, switching at runtime.

                           See
                           - https://stackoverflow.com/questions/73019160/the-getparcelableextra-method-is-deprecated
                           - https://issuetracker.google.com/issues/242048899?pli=1
                           - https://issuetracker.google.com/issues/242048899#comment15

                           We'll switch to new API once it will be more convenient,
                           e.g. by AndroidX wrapper.
                         */
                        @SuppressWarnings("deprecation")
                        SnapshotMetadata snapshotMetadata = (SnapshotMetadata)
                            intent.getParcelableExtra(SnapshotsClient.EXTRA_SNAPSHOT_METADATA);

                        String currentSaveName = snapshotMetadata.getUniqueName();
                        messageSend(new String[]{"chosen-save-game", currentSaveName});
                    } else
                    if (intent.hasExtra(SnapshotsClient.EXTRA_SNAPSHOT_NEW)) {
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
        GoogleSignInAccount account = checkGamesConnection();
        if (account != null) {
            final Activity a = getActivity();
            Games.getAchievementsClient(a, account)
                .getAchievementsIntent()
                .addOnSuccessListener(new OnSuccessListener<Intent>() {
                    @Override
                    public void onSuccess(Intent intent) {
                        a.startActivityForResult(intent, REQUEST_ACHIEVEMENTS);
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
        GoogleSignInAccount account = checkGamesConnection();
        if (account != null) {
            Activity a = getActivity();
            Games.getAchievementsClient(a, account)
                .unlock(achievementId);
        } else {
            logWarning(CATEGORY, "Achievement unlocked, but not connected to Google Games, ignoring");
        }
    }

    private void showSaveGames(final String title, final boolean allowAddButton, final boolean allowDelete, final int maxNumberOfSaveGamesToShow)
    {
        GoogleSignInAccount account = checkGamesConnection();
        if (account != null) {
            int realMaxNumberOfSaveGamesToShow;
            if (maxNumberOfSaveGamesToShow < 0) {
                /* TODO: recheck now.
                   For some reason (bug?), Google Play Games dialog to choose
                   the saved game @italic(does not) show "new save game" buton
                   when this is DISPLAY_LIMIT_NONE (-1).
                   It behaves then like AllowAddButton is always @false.
                   So instead, we use some ridiculously large number for
                   MaxNumberOfSaveGamesToShow. */
                realMaxNumberOfSaveGamesToShow = 1000;
            } else {
                realMaxNumberOfSaveGamesToShow = maxNumberOfSaveGamesToShow;
            }
            final Activity a = getActivity();
            SnapshotsClient snapshotsClient = Games.getSnapshotsClient(a, account);
            Task<Intent> intentTask = snapshotsClient.getSelectSnapshotIntent(
                title, allowAddButton, allowDelete, realMaxNumberOfSaveGamesToShow);
            intentTask.addOnSuccessListener(new OnSuccessListener<Intent>() {
                @Override
                public void onSuccess(Intent intent) {
                    a.startActivityForResult(intent, REQUEST_SAVED_GAMES);
                }
            });
        } else {
            logInfo(CATEGORY, "Not connected to Google Games -> connecting, in response to showSaveGames");
            signInClicked(new OnConnectedFinish () {
                public void run() { showSaveGames(title, allowAddButton, allowDelete, maxNumberOfSaveGamesToShow); }
            });
        }
    }

    // Not configurable from Pascal *for now*.
    private static final int conflictResolution = SnapshotsClient.RESOLUTION_POLICY_MOST_RECENTLY_MODIFIED;

    // The savegame contents is converted from/to a string using this encoding.
    // Not configurable from Pascal *for now*.
    // TODO: Ideally, savegame contents should be treated as binary, not modified by any string processing.
    private static final String saveGameEncoding = "UTF-8";

    /* Make a log, and messageSend, that loading savegame failed. */
    private final void saveGameLoadingError(String errorStr)
    {
        logError(CATEGORY, errorStr);
        messageSend(new String[]{"save-game-loaded", "false", errorStr});
    }

    private void saveGameLoad(final String saveGameName)
    {
        GoogleSignInAccount account = checkGamesConnection();
        if (account != null) {
            Activity a = getActivity();
            SnapshotsClient snapshotsClient = Games.getSnapshotsClient(a, account);
            /* createIfNotFound is false here, this way trying to load from saveGameName
               that was never saved -- will fail, instead of returning
               empty contents (which the caller would find unexpected,
               e.g. it's not valid XML or JSON if caller expects that). */
            snapshotsClient.open(saveGameName, /*createIfNotFound*/ false, conflictResolution)
                .addOnFailureListener(new OnFailureListener() {
                    @Override
                    public void onFailure(@NonNull Exception e) {
                        saveGameLoadingError("Error while opening Snapshot for loading: " + e.getMessage());
                    }
                })
                .addOnSuccessListener(new OnSuccessListener<SnapshotsClient.DataOrConflict<Snapshot>>() {
                    @Override
                    public void onSuccess(SnapshotsClient.DataOrConflict<Snapshot> dataOrConflict) {
                        Snapshot snapshot = dataOrConflict.getData();
                        if (snapshot != null) {
                            // Opening the snapshot was a success and any conflicts have been resolved.
                            try {
                                // Extract the raw data from the snapshot.
                                byte[] saveGameBytes;
                                saveGameBytes = snapshot.getSnapshotContents().readFully();
                                String saveGameStr = new String(saveGameBytes, saveGameEncoding);
                                messageSend(new String[]{"save-game-loaded", "true", saveGameStr});
                            } catch (IOException e) {
                                saveGameLoadingError("Google Play Games error when reading snapshot: " + e.getMessage());
                            }
                        } else {
                            // There was a conflict and the user must select a resolution strategy.
                            SnapshotsClient.SnapshotConflict conflict = dataOrConflict.getConflict();
                            logInfo(CATEGORY, "Conflict when loading savegame: " + conflict.toString());
                        }
                    }
                });
        } else {
            saveGameLoadingError("Not connected to Google Play Games");
        }
    }

    private void saveGameSave(final String saveGameName,
        final String saveGameContents, final String description,
        final long playedTimeMillis)
    {
        GoogleSignInAccount account = checkGamesConnection();
        if (account != null) {
            Activity a = getActivity();
            SnapshotsClient snapshotsClient = Games.getSnapshotsClient(a, account);
            snapshotsClient.open(saveGameName, /*createIfNotFound*/ true, conflictResolution)
                .addOnFailureListener(new OnFailureListener() {
                    @Override
                    public void onFailure(@NonNull Exception e) {
                      logError(CATEGORY, "Error while opening Snapshot for saving: " + e.getMessage());
                    }
                })
                .addOnSuccessListener(new OnSuccessListener<SnapshotsClient.DataOrConflict<Snapshot>>() {
                    @Override
                    public void onSuccess(SnapshotsClient.DataOrConflict<Snapshot> result) {
                        Snapshot snapshot = result.getData();
                        try {
                            snapshot.getSnapshotContents().writeBytes(saveGameContents.getBytes(saveGameEncoding));
                        } catch (UnsupportedEncodingException e) {
                            logError(CATEGORY, "Error while saving a save game, encoding " + saveGameEncoding + " unsupported: " + e.getMessage());
                        }
                        SnapshotMetadataChange metadataChange =
                            new SnapshotMetadataChange.Builder()
                                .setDescription(description)
                                .setPlayedTimeMillis(playedTimeMillis)
                                .build();
                        snapshotsClient.commitAndClose(snapshot, metadataChange);
                    }
                });
        } else {
            logError(CATEGORY, "Not connected to Google Play Games, cannot save savegame.");
        }
    }

    private void showLeaderboard(final String leaderboardId)
    {
        GoogleSignInAccount account = checkGamesConnection();
        if (account != null) {
            final Activity a = getActivity();
            Games.getLeaderboardsClient(a, account)
                .getLeaderboardIntent(leaderboardId)
                .addOnSuccessListener(new OnSuccessListener<Intent>() {
                    @Override
                    public void onSuccess(Intent intent) {
                        a.startActivityForResult(intent, REQUEST_LEADERBOARD);
                    }
                });
        } else {
            logInfo(CATEGORY, "Not connected to Google Games -> connecting, in response to showLeaderboard");
            signInClicked(new OnConnectedFinish () {
                public void run() { showLeaderboard(leaderboardId); }
            });
        }
    }

    /* Send score to given leaderboard.
     *
     * If not connected to Google Play Games, just emit a warning and do nothing.
     * In the past we had here logic to store the score and send it later
     * (when connection will happen) but it was quite incomplete (only last score,
     * only for 1 leaderboard) and inconsistent with achievements
     * (for which no equivalent "send later" logic existed).
     * So now we just emit a warning and do nothing. Higher-level game code
     * should handle this situation, e.g. by resending the score on connection.
     */
    private void submitScore(String leaderboardId, long score)
    {
        GoogleSignInAccount account = checkGamesConnection();
        if (account != null) {
            Activity a = getActivity();
            LeaderboardsClient leaderboardsClient = Games.getLeaderboardsClient(a, account);
            leaderboardsClient.submitScore(leaderboardId, score);
        } else {
            logWarning(CATEGORY, "Not connected to Google Games, not sending score");
        }
    }

    private void requestPlayerBestScore(String leaderboardId)
    {
        GoogleSignInAccount account = checkGamesConnection();
        if (account != null) {
            Activity a = getActivity();
            LeaderboardsClient leaderboardsClient = Games.getLeaderboardsClient(a, account);
            final String saveLeaderboardId = leaderboardId;
            leaderboardsClient.loadCurrentPlayerLeaderboardScore(leaderboardId,
                LeaderboardVariant.TIME_SPAN_ALL_TIME,
                LeaderboardVariant.COLLECTION_PUBLIC).
                addOnSuccessListener(new OnSuccessListener<AnnotatedData<LeaderboardScore>>() {
                    @Override
                    public void onSuccess(AnnotatedData<LeaderboardScore> leaderboardScoreAnnotatedData) {
                        LeaderboardScore leaderboardScore = leaderboardScoreAnnotatedData.get();
                        long myScore =
                            leaderboardScore != null ?
                            leaderboardScore.getRawScore() : 0;
                        messageSend(new String[]{"best-score", saveLeaderboardId, Long.toString(myScore)});
                    }
                });
        } else {
            logWarning(CATEGORY, "Not connected to Google Games, cannot get leaderboard position");
        }
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
