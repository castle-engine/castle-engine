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

import android.Manifest;
import android.view.View;
import android.os.Build;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;

import android.view.Window;
import android.app.Activity; 
import android.view.WindowManager;
import android.util.Log;

import android.view.inputmethod.InputMethodManager;

/**
 * Vibrations, callled from Castle Game Engine.
 * While the code is fairly trivial, it's good to put it in a separate
 * service, since this requires special permission on Android.
 */
public class ServiceKeyboard extends ServiceAbstract
{
    public ServiceKeyboard(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "keyboard";
    }

    /* See
       https://stackoverflow.com/questions/5105354/how-to-show-soft-keyboard-when-edittext-is-focused
    */

    public void showKeyboard()
    {
        //getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE);
        //getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_VISIBLE);

        InputMethodManager imm = (InputMethodManager) getActivity().getSystemService(Context.INPUT_METHOD_SERVICE);
        imm.showSoftInput(getActivity().getWindow().getDecorView(), 0);
        //imm.toggleSoftInput(InputMethodManager.SHOW_FORCED, 0);
        Log.e("keyboard", "show");
    }

    public void hideKeyboard()
    {
        //getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE);
        //getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_VISIBLE);

        InputMethodManager imm = (InputMethodManager) getActivity().getSystemService(Context.INPUT_METHOD_SERVICE);
        imm.hideSoftInputFromWindow(getActivity().getWindow().getDecorView().getWindowToken(), 0);
        getActivity().getWindow().getDecorView().requestFocus();
        //imm.toggleSoftInput(InputMethodManager.SHOW_FORCED, 0);
        Log.e("keyboard", "hide");
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 1 && parts[0].equals("show_keyboard")) {
            showKeyboard();
            return true;
        } else if (parts.length == 1 && parts[0].equals("hide_keyboard")) {
            hideKeyboard();
            return true;
        } else {
            return false;
        }
    }
}

