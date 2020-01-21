/* -*- tab-width: 4 -*- */

/*
  Copyright 2018-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

package net.sourceforge.castleengine;

import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.content.Context;

import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;

public class ServiceDownloadUrls extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceDownloadUrls";

    public ServiceDownloadUrls(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "download_urls";
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 2 && parts[0].equals("download-url")) {
            downloadDataFromUrl(parts[1]);
            return true;
        }
        else
            return false;
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
                    logError(CATEGORY, "downloadDataFromUrl exception: " + e.getMessage());
                    messageSend(new String[]{"download-error", e.getMessage()});
                }
            }
        });
        thread.start();
    }
}
