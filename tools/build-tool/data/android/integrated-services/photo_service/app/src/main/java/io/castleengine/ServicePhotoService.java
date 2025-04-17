/* -*- tab-width: 4 -*- */

/*
  Copyright 2018-2024 Michalis Kamburelis, Jan Adamec.

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
//import android.widget.Toast;
import android.content.ContentResolver;
import android.content.ContentValues;
import android.provider.MediaStore;
import android.os.Build;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.MalformedURLException;
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

    /* Add image to MediaStore using Android Q API.
       See
       https://stackoverflow.com/questions/57726896/mediastore-images-media-insertimage-deprecated
       https://developer.android.com/training/data-storage/shared/media
    */
    private void imageSaveQ(String sourceImagePath, String mimeType)
        throws IOException, MalformedURLException
    {
        logInfo(CATEGORY, "Saving image using modern (>= Q) API " + sourceImagePath);

        ContentValues values = new ContentValues();
        values.put(MediaStore.MediaColumns.DISPLAY_NAME, getFileBaseName(sourceImagePath));

        /* Do not write DATA.

           https://developer.android.com/reference/android/provider/MediaStore.MediaColumns#DATA
           says: """From Android 11 onwards, this column is read-only for apps
           that target R and higher. On those devices, when creating or updating
           a uri, this column's value is not accepted. Instead, to update
           the filesystem location of a file, use the values of the DISPLAY_NAME
           and RELATIVE_PATH columns."""
           And indeed writing it fails (testcase: Fairphone 4) with error
           ""IllegalArgumentException: Mutation of _data is not allowed when target Android""".
        */
        //values.put(MediaStore.MediaColumns.DATA, sourceImagePath);

        /* What Environment.DIRECTORY_xxx to use?
           - DIRECTORY_DCIM
           - DIRECTORY_PICTURES
           - DIRECTORY_SCREENSHOTS?
           See https://developer.android.com/reference/android/os/Environment .
           - DIRECTORY_SCREENSHOTS fails (Fairphone 4):
             """Error writing screenshot: java.lang.IllegalArgumentException: Primary directory Screenshots not allowed for content://media/external/images/media; allowed directories are [DCIM, Pictures]""" .
           - The DIRECTORY_PICTURES seemed OK (Samsung Galaxy Tab A, emulated Android 34)
             but DIRECTORY_DCIM is better: works on Fairphone 4 too.
           - So DIRECTORY_DCIM wins as the only that works everywhere.
        */
        values.put(MediaStore.MediaColumns.RELATIVE_PATH, Environment.DIRECTORY_DCIM);

        if (!stringNullOrEmpty(mimeType)) {
            values.put(MediaStore.MediaColumns.MIME_TYPE, mimeType);
        } else {
            logWarning(CATEGORY, "No MIME type provided for " + sourceImagePath + ", the image may be not recognized by the system");
        }
        values.put(MediaStore.MediaColumns.IS_PENDING, 1);

        ContentResolver resolver = getActivity().getContentResolver();
        Uri newUri = resolver.insert(MediaStore.Images.Media.EXTERNAL_CONTENT_URI, values);
        OutputStream outputStream = resolver.openOutputStream(newUri);
        URL srcImageUrl = new URL(sourceImagePath);
        copyUrlToStream(srcImageUrl, outputStream);

        values.clear();
        values.put(MediaStore.MediaColumns.IS_PENDING, 0);
        resolver.update(newUri, values, null, null);
    }

    /* Add image using deprecated API. */
    @SuppressWarnings("deprecation")
    private void imageSaveDeprecated(String sourceImagePath)
        throws IOException
    {
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

        // let application do notification
        //Toast.makeText(getActivity(), "Image saved", Toast.LENGTH_SHORT).show();
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 3 && parts[0].equals("photoservice-store-image"))
        {
            String sourceImagePath = parts[1];
            String mimeType = parts[2];
            try {
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
                    imageSaveQ(sourceImagePath, mimeType);
                } else {
                    imageSaveDeprecated(sourceImagePath);
                }
            } catch (Exception e) {
                logWarning(CATEGORY, "Error writing screenshot: " + e.toString());
            }
            return true;
        }
        else
            return false;
    }

    private static String getFileExt(String fileName)
    {
        return fileName.substring(fileName.lastIndexOf(".") + 1, fileName.length());
    }

    private static String getFileBaseName(String fileName)
    {
        return fileName.substring(fileName.lastIndexOf("/") + 1, fileName.length());
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

    private static void copyUrlToStream(URL src, OutputStream out) throws IOException
    {
        InputStream in = src.openStream();
        try {
            try {
                // Transfer bytes from in to out
                byte[] buf = new byte[1024 * 1024];
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

    private static void copyUrlToFile(URL src, File dst) throws IOException
    {
        copyUrlToStream(src, new FileOutputStream(dst));
    }
}
