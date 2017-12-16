/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.database.Cursor;
import android.net.Uri;
import android.os.Handler;
import android.os.Looper;
import android.provider.MediaStore;
import android.util.Log;

import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;

public class ServiceOpenAssociatedUrls extends ServiceAbstract
{
    private static final String TAG = "ASSOC_URL";

    public String getName()
    {
        return "open_associated_urls";
    }

    public ServiceOpenAssociatedUrls(MainActivity activity)
    {
        super(activity);
    }

    @Override
    public void onCreate()
    {
    	Intent intent = getActivity().getIntent();
		openIntent(intent);
	}

	@Override
	public void onNewIntent(Intent intent)
	{
		openIntent(intent);
	}

	private void openIntent(Intent intent)
	{
		// https://stackoverflow.com/a/26034600/2870459
		String action = intent.getAction();
		if (action != null && action.compareTo(Intent.ACTION_VIEW) == 0)
		{
			String scheme = intent.getScheme();
			if (scheme == null)
				return;

			ContentResolver resolver = getActivity().getContentResolver();

			File urlDocumentsDir = getActivity().getDir("inbox", Context.MODE_PRIVATE);

			if (scheme.compareTo(ContentResolver.SCHEME_CONTENT) == 0)
			{
				Uri uri = intent.getData();
				if (uri == null)
					return;

				String name = getContentName(resolver, uri);

				Log.i(TAG, "Content intent detected: " + action + " : " + intent.getDataString() + " : " + intent.getType() + " : " + name);
				try
				{
					InputStream input = resolver.openInputStream(uri);
					String importfilepath = urlDocumentsDir.getAbsolutePath() + "/" + name;
					InputStreamToFile(input, importfilepath);
					messageSend(new String[]{"open_associated_url", "file://" + importfilepath});
				}
				catch (Exception e)
				{
					Log.e(TAG, "resolver.openInputStream exception: " + e.getMessage());
				}
			}
			else if (scheme.compareTo(ContentResolver.SCHEME_FILE) == 0)
			{
				Uri uri = intent.getData();
				if (uri == null)
					return;

				String name = uri.getLastPathSegment();

				Log.i(TAG, "File intent detected: " + action + " : " + intent.getDataString() + " : " + intent.getType() + " : " + name);
				messageSend(new String[]{"open_associated_url", uri.toString()});
			}
			else if (scheme.compareTo("http") == 0 || scheme.compareTo("https") == 0 || scheme.compareTo("ftp") == 0)
			{
				Uri uri = intent.getData();
				if (uri == null)
					return;

				String name = uri.getLastPathSegment();

				Log.i(TAG, "Http intent detected: " + action + " : " + intent.getDataString() + " : " + intent.getType() + " : " + name);
				/*
				// open directly from http:
				messageSend(new String[]{"open_associated_url", uri.toString()});
				/*/
				try {
					String importfilepath = urlDocumentsDir.getAbsolutePath() + "/" + name;
					DownloadDataFromUrl(new URL(uri.toString()), importfilepath);
				}
				catch (Exception e)
				{
					Log.e(TAG, "URL exception: " + e.getMessage());
				}
				//*/
			}
		}
	}

	private String getContentName(ContentResolver resolver, Uri uri)
	{
		Cursor cursor = resolver.query(uri, null, null, null, null);
		cursor.moveToFirst();
		int nameIndex = cursor.getColumnIndex(MediaStore.MediaColumns.DISPLAY_NAME);
		if (nameIndex >= 0) {
			return cursor.getString(nameIndex);
		} else {
			return null;
		}
	}

	private void InputStreamToFile(InputStream in, String file)
	{
		try
		{
			OutputStream out = new FileOutputStream(new File(file));

			int size = 0;
			byte[] buffer = new byte[1024];

			while ((size = in.read(buffer)) != -1)
			{
				out.write(buffer, 0, size);
			}

			out.close();
		}
		catch (Exception e)
		{
			Log.e(TAG, "InputStreamToFile exception: " + e.getMessage());
		}
	}

	private void DownloadDataFromUrl(final URL url, final String file)
	{
		Thread thread = new Thread(new Runnable(){
			@Override
			public void run(){
				try {
					InputStream inStream = url.openStream();

					DataInputStream stream = new DataInputStream(inStream);
					BufferedInputStream bufferedReader = new BufferedInputStream(stream);

					InputStreamToFile(bufferedReader, file);

					stream.close();

					new Handler(Looper.getMainLooper()).post(new Runnable() {   // run in main thread
						@Override
						public void run() {
							messageSend(new String[]{"open_associated_url", "file://" + file});
						}
					});
				}
				catch (Exception e) {
					Log.e(TAG, "DownloadDataFromUrl exception: " + e.getMessage());
				}
			}
		});
		thread.start();
	}
}
