/* -*- tab-width: 4 -*- */

/*
  Copyright 2018-2022 Andrzej Kilijański, Michalis Kamburelis.

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
import android.view.ViewGroup;
import android.os.Build;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;

import android.view.Window;
import android.app.Activity; 
import android.view.WindowManager;
import android.util.Log;
import android.view.inputmethod.EditorInfo;
import android.text.InputType;
import android.view.inputmethod.BaseInputConnection;
import android.view.inputmethod.InputConnection;
import android.view.KeyEvent;

import android.view.inputmethod.InputMethodManager;

class CastleInputConnection extends BaseInputConnection 
{
    ServiceKeyboard serviceKeyboard;

    CastleInputConnection(ServiceKeyboard service, View targetView, boolean fullEditor) 
    {
        super(targetView, fullEditor);
        serviceKeyboard = service;
    }

    // Support for soft keyboard key events 
    // https://developer.android.com/reference/android/view/inputmethod/InputConnection#sendKeyEvent(android.view.KeyEvent)
    // https://developer.android.com/reference/android/view/KeyEvent
    @Override
    public boolean sendKeyEvent(KeyEvent event) 
    {
        // not working in InputType.TYPE_CLASS_TEXT;
        serviceKeyboard.logInfo("CastleInputConnection", "key event - code " + Integer.toString(event.getKeyCode()));
        // use enter to hide keyboard
        if (event.getKeyCode() == KeyEvent.KEYCODE_ENTER) 
        {
            serviceKeyboard.logInfo("CastleInputConnection", "key event - hide kyeboard");
            serviceKeyboard.hideKeyboard();
            return true;
        }

        String keyCode = Integer.toString(event.getKeyCode());
        String keyString;

        // https://developer.android.com/reference/android/view/KeyEvent#getUnicodeChar(int)
        // https://developer.android.com/reference/android/view/KeyEvent#getUnicodeChar()
        int character = event.getUnicodeChar();
        if (character != 0)
        {
            //String s = (new StringBuilder()).append(character).toString();
            keyString =  new String(Character.toChars(character));
            serviceKeyboard.logInfo("CastleInputConnection", "key event - char = " + keyString);
        }
        else
        {
            keyString = new String("");
        }


        // https://developer.android.com/reference/android/view/KeyEvent#getAction()
        if (event.getAction() == KeyEvent.ACTION_DOWN)
        {
            serviceKeyboard.logInfo("CastleInputConnection", "key event - key down");
            serviceKeyboard.messageSend(new String[]{"castle-key-down", keyCode, keyString});
        }
        else if (event.getAction() == KeyEvent.ACTION_UP)
        {
            serviceKeyboard.logInfo("CastleInputConnection", "key event - key up");
            serviceKeyboard.messageSend(new String[]{"castle-key-up", keyCode, keyString});
        }
        return true;
    }


    @Override
    public boolean commitText(CharSequence text, int newCursorPosition) 
    {
        serviceKeyboard.logInfo("CastleInputConnection", "commitText - " + text.toString());
        serviceKeyboard.logInfo("CastleInputConnection", "cursor pos - " + Integer.toString(newCursorPosition));
        
        int i = 0;
        while (i < text.length())
        {
            char c = text.charAt(i);
            if (Character.isHighSurrogate(c))
            {
                serviceKeyboard.logInfo("CastleInputConnection", "surrogate");
                i++;
                char c2 = text.charAt(i);
                String s = new String (Character.toChars(Character.toCodePoint(c, c2)));
                serviceKeyboard.messageSend(new String[]{"castle-key-down", "0", s});
                serviceKeyboard.messageSend(new String[]{"castle-key-up", "0", s});
            }
            String s = new String(Character.toChars(c));
            serviceKeyboard.messageSend(new String[]{"castle-key-down", "0", s});
            serviceKeyboard.messageSend(new String[]{"castle-key-up", "0", s});
            i++;
        }

        //sendedButNotCommited.setText("");
        return true;
    }

}


class CastleKeyboardInputView extends View 
{
    CastleInputConnection inputConnection;
    ServiceKeyboard serviceKeyboard;

    public CastleKeyboardInputView(ServiceKeyboard service, Context context) 
    {
        super(context);
        serviceKeyboard = service;
        setFocusable(true);
        setFocusableInTouchMode(true); // without this line we can't show keyboard by ServiceKeyboard.showKeyboard()
    }    

    // https://stackoverflow.com/questions/5419766/how-to-capture-soft-keyboard-input-in-a-view
    @Override
    public InputConnection onCreateInputConnection(EditorInfo outAttrs) 
    {
        inputConnection = new CastleInputConnection(serviceKeyboard, this, false);

        //outAttrs.inputType = InputType.TYPE_NULL; // only soft key events - when this is set keyboard returns key events
        // Because InputType.TYPE_NULL dont support some characters we need change to InputType.TYPE_CLASS_TEXT
        // but then no key events are not sent, so we need use commitText() and create them our self
        outAttrs.inputType = InputType.TYPE_CLASS_TEXT; 
        outAttrs.imeOptions = EditorInfo.IME_FLAG_NO_FULLSCREEN | EditorInfo.IME_FLAG_NO_EXTRACT_UI;

        return inputConnection;
    }
}

/**
 * Service for keyboard support on android devices
 */
public class ServiceKeyboard extends ServiceAbstract
{

    CastleKeyboardInputView keyboardInputView;

    public ServiceKeyboard(MainActivity activity)
    {
        super(activity);
        keyboardInputView = new CastleKeyboardInputView(this, getActivity().getWindow().getContext());
        // https://stackoverflow.com/questions/40237846/programmatically-add-a-view-to-the-bottom-of-the-screen
        ViewGroup rootView = (ViewGroup) getActivity().findViewById(android.R.id.content);
        rootView.addView(keyboardInputView);
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
        //getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE); - not working
        //getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_VISIBLE); - not working

        InputMethodManager imm = (InputMethodManager) getActivity().getSystemService(Context.INPUT_METHOD_SERVICE);
        //getActivity().getWindow().getDecorView().requestFocus();
        //imm.showSoftInput(getActivity().getWindow().getDecorView(), 0);
        keyboardInputView.requestFocus(); // needed to show keyboard work properly
        imm.showSoftInput(keyboardInputView, 0);
        //imm.toggleSoftInput(InputMethodManager.SHOW_FORCED, 0);
        logInfo("keyboard", "show");
    }

    public void hideKeyboard()
    {
        //getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE); - not working
        //getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_VISIBLE); - not working

        InputMethodManager imm = (InputMethodManager) getActivity().getSystemService(Context.INPUT_METHOD_SERVICE);
        //imm.hideSoftInputFromWindow(getActivity().getWindow().getDecorView().getWindowToken(), 0);
        imm.hideSoftInputFromWindow(keyboardInputView.getWindowToken(), 0);
        //getActivity().getWindow().getDecorView().requestFocus();
        //imm.toggleSoftInput(InputMethodManager.SHOW_FORCED, 0);
        logInfo("keyboard", "hide");
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 1 && parts[0].equals("castle-show-keyboard")) {
            showKeyboard();
            return true;
        } else if (parts.length == 1 && parts[0].equals("castle-hide-keyboard")) {
            hideKeyboard();
            return true;
        } else {
            return false;
        }
    }
}

