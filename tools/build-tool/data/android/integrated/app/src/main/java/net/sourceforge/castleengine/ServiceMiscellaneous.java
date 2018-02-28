/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.view.View;
import android.os.Build;
import android.content.Context;
import android.content.Intent;
import android.content.res.TypedArray;
import android.net.Uri;
import android.widget.Toast;

import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;

/**
 * Integration of various Android small stuff with
 * Castle Game Engine.
 */
public class ServiceMiscellaneous extends ServiceAbstract
{
    public ServiceMiscellaneous(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "miscellaneous";
    }

    /** Immersive mode. */
    @Override
    public void onWindowFocusChanged(boolean hasFocus) {
        int[] attrs = {android.R.attr.windowFullscreen};
        TypedArray ta = getActivity().getTheme().obtainStyledAttributes(attrs);
        if (ta.getBoolean(0, true) && hasFocus) {
            /* To have all the flags and methods below available
             * (in particular, SYSTEM_UI_FLAG_IMMERSIVE_STICKY)
             * wee need Android API version 19. Check the version at runtime,
             * to handle various API versions with the same apk.
             */
            if (Build.VERSION.SDK_INT >= 19) {
                View decorView = getActivity().getWindow().getDecorView();
                decorView.setSystemUiVisibility(
                    View.SYSTEM_UI_FLAG_LAYOUT_STABLE
                    | View.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION
                    | View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN
                    | View.SYSTEM_UI_FLAG_HIDE_NAVIGATION
                    | View.SYSTEM_UI_FLAG_FULLSCREEN
                    | View.SYSTEM_UI_FLAG_IMMERSIVE_STICKY
                );
            }
        }
    }

    /* Shares ---------------------------------------------------------------- */

    /**
     * Share a text with other applications.
     * See https://developer.android.com/training/sharing/send.html
     */
    private void shareText(String title, String subject, String text)
    {
        Intent sendIntent = new Intent();
        sendIntent.setAction(Intent.ACTION_SEND);
        sendIntent.putExtra(Intent.EXTRA_TEXT, text);
        sendIntent.putExtra(Intent.EXTRA_SUBJECT, subject);
        sendIntent.setType("text/plain");
        getActivity().startActivity(Intent.createChooser(sendIntent, title));
    }

    /**
     * View URL.
     * See http://stackoverflow.com/questions/4969217/share-application-link-in-android
     */
    private void viewUrl(String url)
    {
        Intent intent = new Intent(Intent.ACTION_VIEW);
        intent.setData(Uri.parse(url));
        getActivity().startActivity(intent);
    }

    private void downloadDataFromUrl(String urlToDownload)
    {
        final URL url;
        try {
            url = new URL(urlToDownload);
        }
        catch (Exception e) {
            messageSend(new String[]{"download-error", e.getMessage()});
            return;
        }

        File urlDocumentsDir = getActivity().getDir("inbox", Context.MODE_PRIVATE);
        String tempFileName = url.getPath().replaceAll("[/|><]", "_");
        final String tempDownloadFile = urlDocumentsDir.getAbsolutePath() + "/" + tempFileName;

        Thread thread = new Thread(new Runnable(){
            @Override
            public void run(){
                try {
                    InputStream inStream = url.openStream();

                    DataInputStream stream = new DataInputStream(inStream);
                    BufferedInputStream bufferedReader = new BufferedInputStream(stream);

                    OutputStream streamOut = new FileOutputStream(new File(tempDownloadFile));

                    int size = 0;
                    byte[] buffer = new byte[1024];

                    while ((size = bufferedReader.read(buffer)) != -1)
                    {
                        streamOut.write(buffer, 0, size);
                    }

                    streamOut.close();
                    stream.close();

                    new Handler(Looper.getMainLooper()).post(new Runnable() {   // run in main thread
                        @Override
                        public void run() {
                            messageSend(new String[]{"download-finished", tempDownloadFile});
                        }
                    });
                }
                catch (Exception e) {
                    Log.e("DL_URL", "DownloadDataFromUrl exception: " + e.getMessage());
                    messageSend(new String[]{"download-error", e.getMessage()});
                }
            }
        });
        thread.start();
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 2 && parts[0].equals("view-url")) {
            viewUrl(parts[1]);
            return true;
        } else
        if (parts.length == 4 && parts[0].equals("share-text")) {
            shareText(parts[1], parts[2], parts[3]);
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("on-screen-notification")) {
            Toast.makeText(getActivity().getApplicationContext(), parts[1], Toast.LENGTH_SHORT).show();
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("download-url")) {
            downloadDataFromUrl(parts[1]);
            return true;
        } else
        {
            return false;
        }
    }
}
