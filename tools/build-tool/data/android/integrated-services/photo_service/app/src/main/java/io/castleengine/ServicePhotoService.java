/* -*- tab-width: 4 -*- */

/*
  Copyright 2018-2020 Michalis Kamburelis, Jan Adamec.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

package io.castleengine;

import android.content.Intent;
import android.net.Uri;
import android.os.Environment;
import android.util.Log;
import android.widget.Toast;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

public class ServicePhotoService extends ServiceAbstract
{
    private static final String CATEGORY = "ServicePhotoService";

    public String getName()
    {
        return "photo_service";
    }

    public ServicePhotoService(MainActivity activity)
    {
        super(activity);
    }
    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 2 && parts[0].equals("photoservice-store-image"))
        {
            String sourceImagePath = parts[1];
            try {
                URL srcImageUrl = new URL(sourceImagePath);

                String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.US).format(new Date());
                String imageFileName = "Image_" + timeStamp + "." + getFileExt(sourceImagePath);

                File storageDir = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_PICTURES);
                File dstImage = new File(storageDir, imageFileName);

                // Make sure the Pictures directory exists.
                storageDir.mkdirs();

                //String dstPhotoPath = dstImage.getAbsolutePath();

                copyUrlToFile(srcImageUrl, dstImage);

                Intent mediaScanIntent = new Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE);
                Uri contentUri = Uri.fromFile(dstImage);
                mediaScanIntent.setData(contentUri);
                getActivity().sendBroadcast(mediaScanIntent);

                Toast.makeText(getActivity(), "Image saved", Toast.LENGTH_SHORT).show();

            } catch (Exception e) {
                logWarning(CATEGORY, "Error writing screenshot: " + e.toString());
            }

            return true;
        }
        else
            return false;
    }

    private static String getFileExt(String fileName) {
        return fileName.substring(fileName.lastIndexOf(".") + 1, fileName.length());
    }

    /*private static void copyFile(File src, File dst) throws IOException {
        InputStream in = new FileInputStream(src);
        try {
            OutputStream out = new FileOutputStream(dst);
            try {
                // Transfer bytes from in to out
                byte[] buf = new byte[1024];
                int len;
                while ((len = in.read(buf)) > 0) {
                    out.write(buf, 0, len);
                }
            } finally {
                out.close();
            }
        } finally {
            in.close();
        }
    }*/

    private static void copyUrlToFile(URL src, File dst) throws IOException {
        InputStream in = src.openStream();
        try {
            OutputStream out = new FileOutputStream(dst);
            try {
                // Transfer bytes from in to out
                byte[] buf = new byte[1024];
                int len;
                while ((len = in.read(buf)) > 0) {
                    out.write(buf, 0, len);
                }
            } finally {
                out.close();
            }
        } finally {
            in.close();
        }
    }
}
