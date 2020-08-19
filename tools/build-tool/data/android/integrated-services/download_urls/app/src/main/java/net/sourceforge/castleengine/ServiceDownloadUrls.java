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
        if (parts.length == 3 && parts[0].equals("download-url")) {
            downloadDataFromUrl(Integer.parseInt(parts[1]), parts[2]);
            return true;
        }
        else
            return false;
    }

    private void downloadDataFromUrl(final int downloadId, String urlToDownload)
    {
        final URL url;
        try {
            url = new URL(urlToDownload);
        }
        catch (Exception e) {
            messageSend(new String[]{"download-error", Integer.toString(downloadId), e.getMessage()});
            return;
        }

        // Store the file in cache dir, see https://stackoverflow.com/questions/3425906/creating-temporary-files-in-android
        File urlDocumentsDir = getActivity().getCacheDir();
        final String tempDownloadFile = urlDocumentsDir.getAbsolutePath() + "/" + Integer.toString(downloadId);

        Thread thread = new Thread(new Runnable(){
            @Override
            public void run(){
                try {
                    InputStream inStream = url.openStream();

                    DataInputStream stream = new DataInputStream(inStream);
                    BufferedInputStream bufferedReader = new BufferedInputStream(stream);

                    OutputStream streamOut = new FileOutputStream(new File(tempDownloadFile));

                    int size = 0;
                    long totalSize = 0;
                    byte[] buffer = new byte[1024];

                    while ((size = bufferedReader.read(buffer)) != -1)
                    {
                        streamOut.write(buffer, 0, size);
                        totalSize += size;
                        messageSendFromThread(new String[]{"download-progress", Integer.toString(downloadId), Long.toString(totalSize)});
                    }

                    streamOut.close();
                    stream.close();

                    messageSendFromThread(new String[]{"download-success", Integer.toString(downloadId), tempDownloadFile});
                }
                catch (Exception e) {
                    logError(CATEGORY, "downloadDataFromUrl exception: " + e.getMessage());
                    messageSendFromThread(new String[]{"download-error", Integer.toString(downloadId), e.getMessage()});
                }
            }
        });
        thread.start();
    }
}
