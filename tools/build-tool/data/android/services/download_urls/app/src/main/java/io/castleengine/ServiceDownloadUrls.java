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

package io.castleengine;

import android.util.Log;
import android.content.Context;

import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import java.io.PrintWriter;
import java.net.URL;
import java.net.URLConnection;
import java.net.HttpURLConnection;
import java.util.Arrays;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;

public class ServiceDownloadUrls extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceDownloadUrls";

    private final List<Integer> interruptIds = new ArrayList<Integer>();

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
        if (parts.length == 6 && parts[0].equals("download-url")) {
            downloadDataFromUrl(Integer.parseInt(parts[1]), parts[2], parts[3], parts[4], parts[5]);
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("download-interrupt")) {
            synchronized(interruptIds) {
                interruptIds.add(Integer.parseInt(parts[1]));
            }
            return true;
        }
        else
            return false;
    }

    /*
     * Send to Pascal http response headers.
     * Note that this is done in non-main thread.
     */
    private void sendResponseHeaders(Map<String,List<String>> httpResponseHeaders, String downloadIdStr)
    {
        List<String> httpResponseHeadersList = new ArrayList<String>();
        httpResponseHeadersList.add("download-http-response-headers");
        httpResponseHeadersList.add(downloadIdStr);

        for (Map.Entry<String,List<String>> entry : httpResponseHeaders.entrySet()) {
            String key = entry.getKey();
            List<String> value = entry.getValue();
            if (value.size() == 0) {
                httpResponseHeadersList.add(key + ": ");
            } else {
                for (String headerValue : value) {
                    httpResponseHeadersList.add(key + ": " + headerValue);
                }
            }
        }
        messageSendFromThread(httpResponseHeadersList.toArray(new String[0]));
    }

    private void downloadDataFromUrl(final int downloadId,
      final String urlToDownload,
      final String httpMethod,
      final String httpRequestBody,
      final String httpHeaders)
    {
        final String downloadIdStr = Integer.toString(downloadId);
        final URL url;
        try {
            url = new URL(urlToDownload);
        }
        catch (Exception e) {
            messageSend(new String[]{"download-error", downloadIdStr, e.getMessage()});
            return;
        }

        Thread thread = new Thread(new Runnable(){
            @Override
            public void run(){
                try {
                    // https://docs.oracle.com/javase/8/docs/api/java/net/URLConnection.html
                    // https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html
                    HttpURLConnection connection = (HttpURLConnection) url.openConnection();

                    connection.setRequestMethod(httpMethod);
                    if (httpHeaders.length() != 0) {
                        String[] headers = httpHeaders.split("\n");
                        for (String header : headers) {
                            String[] headerParts = header.split(":");
                            if (headerParts.length == 2) {
                                connection.setRequestProperty(headerParts[0], headerParts[1]);
                            }
                        }
                    }

                    if (httpRequestBody.length() != 0) {
                        connection.setDoOutput(true);
                        // TODO: we need to consider a way to deal with binary data
                        byte[] httpRequestBodyBytes = httpRequestBody.getBytes("utf-8");
                        connection.getOutputStream().write(httpRequestBodyBytes, 0, httpRequestBodyBytes.length);
                    }

                    // not necessary, as Response/Stream methods call it internally, just to separate set up from parse
                    connection.connect();

                    int responseCode = connection.getResponseCode();
                    messageSendFromThread(new String[]{"download-response-code", downloadIdStr,
                        Integer.toString(responseCode), connection.getResponseMessage()
                    });

                    InputStream inStream = null;
                    if (responseCode < 400)
                        inStream = connection.getInputStream();
                    else
                        inStream = connection.getErrorStream();

                    Map<String,List<String>> httpResponseHeaders = connection.getHeaderFields();
                    sendResponseHeaders(httpResponseHeaders, downloadIdStr);

                    int size = 0;
                    byte[] buffer = new byte[1024 * 1024];
                    while ((size = inStream.read(buffer)) != -1)
                    {
                        messageSendFromThread(new String[]{"download-progress", downloadIdStr},
                            /* Always copy buffer to new array instance, as messageSendFromThread
                               may store new array reference in a message queue. */
                            Arrays.copyOfRange(buffer, 0, size));

                        boolean interrupt = false;
                        synchronized(interruptIds) {
                            int indexInInterruptList = interruptIds.indexOf(downloadId);
                            if (indexInInterruptList != -1) {
                                interrupt = true;
                                interruptIds.remove(indexInInterruptList);
                            }
                        }
                        if (interrupt) {
                            break;
                        }
                    }

                    inStream.close();

                    messageSendFromThread(new String[]{"download-success", downloadIdStr});
                }
                catch (Exception e) {
                    logError(CATEGORY, "downloadDataFromUrl exception: " + PrintStackTrace(e));
                    messageSendFromThread(new String[]{"download-error", downloadIdStr, e.getMessage()});
                }
            }
        });
        thread.start();
    }

    private String PrintStackTrace(Exception ex)
    {
        StringWriter errors = new StringWriter();
        ex.printStackTrace(new PrintWriter(errors));
        return errors.toString();
    }
}
