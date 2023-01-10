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
import android.view.inputmethod.ExtractedTextRequest;
import android.view.inputmethod.ExtractedText;

import android.view.inputmethod.InputMethodManager;

class CastleInputConnection extends BaseInputConnection 
{
    ServiceKeyboard serviceKeyboard; 
    String sentButNotCommited; // chars that was sent but are still in composing mode
    String fullText; // full text in the edit

    CastleInputConnection(ServiceKeyboard service, View targetView, boolean fullEditor) 
    {
        super(targetView, fullEditor);
        serviceKeyboard = service;
        sentButNotCommited = "";
        fullText = "";
    }

    /* Support for soft keyboard key events 
       https://developer.android.com/reference/android/view/inputmethod/InputConnection#sendKeyEvent(android.view.KeyEvent)
       https://developer.android.com/reference/android/view/KeyEvent */
    @Override
    public boolean sendKeyEvent(KeyEvent event) 
    {
        // it seems that this function is called only by the backspace key - and only when there is no composing text
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "------- call sendKeyEvent()");
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "sendKeyEvent - code " + Integer.toString(event.getKeyCode()));

        // use enter to hide keyboard - only works when EditorInfo.inputType = InputType.TYPE_NULL; 
        if (event.getKeyCode() == KeyEvent.KEYCODE_ENTER) 
        {
            serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "sendKeyEvent - hide kyeboard");
            serviceKeyboard.hideKeyboard();
            return true;
        }

        if (event.getKeyCode() == KeyEvent.KEYCODE_BACK)
        {
            serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "sendKeyEvent - KEYCODE_BACK");
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
            serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "sendKeyEvent - char = " + keyString);
        }
        else
        {
            keyString = new String("");
        }


        // https://developer.android.com/reference/android/view/KeyEvent#getAction()
        if (event.getAction() == KeyEvent.ACTION_DOWN)
        {
            serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "sendKeyEvent - key down");
            serviceKeyboard.messageSend(new String[]{"castle-key-down", keyCode, keyString});
        }
        else if (event.getAction() == KeyEvent.ACTION_UP)
        {
            serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "sendKeyEvent - key up");
            serviceKeyboard.messageSend(new String[]{"castle-key-up", keyCode, keyString});
        }
        return true;
    }


    /* Called when user end composing some text part */
    @Override
    public boolean commitText(CharSequence text, int newCursorPosition) 
    {
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "------- call commitText()");
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "commitText - " + text.toString());
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "cursor pos - " + Integer.toString(newCursorPosition));


        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "sent text: - " + sentButNotCommited);
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "text to commit: - " + text.toString());
        
        String textToCommit = text.toString();

        if (sentButNotCommited.equals(textToCommit))
        {
            serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "the same text sent - " + text.toString());
            sentButNotCommited = "";
            return true;
        }    

        if ((textToCommit.length() > 0) && (sentButNotCommited.equals(textToCommit.substring(0, textToCommit.length()-1))))
        {
            // this fixes case when you tap dictionary hint then gboard adds extra space - no need to call updateText()
            if (textToCommit.charAt(textToCommit.length()-1) == 32)
            {
                serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "the same text sent - " + textToCommit + "but space on end");
                serviceKeyboard.messageSend(new String[]{"castle-key-down", "0", " "});
                serviceKeyboard.messageSend(new String[]{"castle-key-up", "0", " "});
                fullText = fullText + " ";
                sentButNotCommited = "";
                return true;
            }
        }

        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "------- call updateText from commit - " + text.toString());
        updateText(text.toString());

        sentButNotCommited = "";
        return true;
    }

    /* Called when composing text changed */
    @Override
    public boolean setComposingText(CharSequence text, int newCursorPosition) 
    {
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "------- call setComposingText()");
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "composingText - " + text.toString());
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "cursor pos - " + Integer.toString(newCursorPosition));

        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "------- call updateText from composing - " + text.toString());
        updateText(text.toString());
        return true;
    }

    /* Called by keyboard when backspace is presed and composing text is empty */
    @Override
    public boolean deleteSurroundingText (int beforeLength, int afterLength)
    {
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "------- call deleteSurroundingText()");
        // seems this is only called when composing text is empty and user press backspace ?
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "deleteSurroundingText beforeLength: " + Integer.toString(beforeLength));
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "deleteSurroundingText afterLength: " + Integer.toString(afterLength));

        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "deleteSurroundingText fullText before: '" + fullText + "'");

        if (sentButNotCommited.length() > 0)
        {
            serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "deleteSurroundingText - sentButNotCommited.length() > 0 = " + Integer.toString(sentButNotCommited.length()));
            if (sentButNotCommited.length() >= beforeLength)
            {
                sentButNotCommited = sentButNotCommited.substring(0, sentButNotCommited.length() - beforeLength);
            }
            else
                sentButNotCommited = "";

        }
        
        int backSpaceCountToSend = 0;

        if (fullText.length() >= beforeLength)
        {
            fullText = fullText.substring(0, fullText.length() - beforeLength);
            backSpaceCountToSend = beforeLength;
        }
        else
        {
            fullText = "";
            backSpaceCountToSend = beforeLength;
        }

        int i = 0;
        while (i < backSpaceCountToSend)
        {
            serviceKeyboard.messageSend(new String[]{"castle-key-down", "67", ""});
            serviceKeyboard.messageSend(new String[]{"castle-key-up", "67", ""});
            i++;
        }
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "deleteSurroundingText fullText after: '" + fullText + "'");

        return true;
    }

    /* Called by keyboard when backspace is presed and composing text is empty and (deleteSurroundingText() is not overriden?) */
    @Override
    public boolean deleteSurroundingTextInCodePoints (int beforeLength, int afterLength)
    {
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "------- call deleteSurroundingTextInCodePoints()");
        // seems this is only called when composing text is empty and user press backspace ?
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "deleteSurroundingTextInCodePoints beforeLength: " + Integer.toString(beforeLength));
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "deleteSurroundingTextInCodePoints afterLength: " + Integer.toString(afterLength));

        return true;
    }

    /* Called by keyboard when word of text is deleted */
    @Override
    public boolean setComposingRegion (int start, int end)
    {
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "------- call setComposingRegion()");
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "setComposingRegion start: " + Integer.toString(start));
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "setComposingRegion stop: " + Integer.toString(end));
        
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "setComposingRegion fullText: " + fullText);
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "setComposingRegion old sentButNotCommited: '" + sentButNotCommited + "'");
        sentButNotCommited = fullText.substring(start, end);
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "setComposingRegion new sentButNotCommited: '" + sentButNotCommited + "'");
        return true;
    }

    /* Changes current uncommited text to newText and also takes care fullText be up to date */
    public void updateText(String newText)
    {
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "------- call updateText()");
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "updateText - newText: '" + newText + "'");
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "updateText - sentButNotCommited: '" + sentButNotCommited + "'");

        if (sentButNotCommited.equals(newText))
        {
            // nothing changes so simply exit
            serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "the same text sent - '" + newText + "'");
            return;
        }

        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "updateText - check1 ");
        if (sentButNotCommited.length() > newText.length())
        {
            // remove excess of characters
            int charsToRemove = sentButNotCommited.length() - newText.length();
            serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "updateText - charsToRemove - " + Integer.toString(charsToRemove));
            int i = charsToRemove;
            while (i > 0)
            {
                // take care of the surrogates when sending backspace key - sometimes there can be two java chars for one codepoint
                //serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "updateText - check letter " + sentButNotCommited.charAt(newText.length() + i - 1));
                if (Character.isLowSurrogate(sentButNotCommited.charAt(newText.length() + i - 1)))
                {
                    serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "updateText - send backkey skip surrogate.");
                    i--;
                    continue; 
                }
                
                serviceKeyboard.messageSend(new String[]{"castle-key-down", "67", ""});
                serviceKeyboard.messageSend(new String[]{"castle-key-up", "67", ""});
                i--;
            }

            sentButNotCommited = sentButNotCommited.substring(0, newText.length());
            serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "old composing text: '" + sentButNotCommited + "'");
            serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "new composing text: '" + newText + "'");

            serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "old fullText: '" + fullText + "'");
            fullText = fullText.substring(0, fullText.length() - charsToRemove);
            serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "new fullText: '" + fullText + "'");
        }

        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "updateText - check2 ");
        // at this moment new text is longer or has the same length like the new one
        if (sentButNotCommited.equals(newText))
        {
            // text is the same just exit
            // fullText should also be the same
            return;
        }

        // how many matching characters
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "updateText - check3 ");
        int matchingCharacters = 0;
        int matchingCodepoints = 0;
        for (int i = 0 ; i < sentButNotCommited.length() ; i++)
        {
            serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "updateText - check3.1 ");
            if (sentButNotCommited.charAt(i) != newText.charAt(i))
                break;
            
            matchingCharacters = i + 1;
        } 
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "updateText - matchingCharacters - " + Integer.toString(matchingCharacters));

        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "updateText - check4 ");
        if (matchingCharacters != sentButNotCommited.length())
        {
            // need to remove some characters
            serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "updateText - check4.1 ");

            int charsToRemove = sentButNotCommited.length() - matchingCharacters;
            int i = 0;
            while (i < charsToRemove)
            {
                // take care of the surrogates when sending backspace key - sometimes there can be two java chars for one codepoint
                // serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "updateText - check letter " + sentButNotCommited.charAt(matchingCharacters + i));
                if (Character.isLowSurrogate(sentButNotCommited.charAt(matchingCharacters + i)))
                {
                    serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "updateText - send backspace skip surrogate.");
                    i++;
                    continue; 
                }
                
                serviceKeyboard.messageSend(new String[]{"castle-key-down", "67", ""});
                serviceKeyboard.messageSend(new String[]{"castle-key-up", "67", ""});
                i++;
            }
        }

        // remove matching characters from new composing text
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "updateText - check5 ");
        String charsToAdd = new String("");
        if (matchingCharacters > 0 )
            charsToAdd = newText.substring(matchingCharacters, newText.length());
        else
            charsToAdd = newText;

        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "charsToAdd: '" + charsToAdd + "'");

        int charsToRemoveFromFullText = sentButNotCommited.length() - matchingCharacters;
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "charsToRemoveFromFullText: " + Integer.toString(charsToRemoveFromFullText));

        sentButNotCommited = sentButNotCommited.substring(0, matchingCharacters);
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "old matching characters: '" + sentButNotCommited + "'");
        sentButNotCommited = sentButNotCommited + charsToAdd;
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "new composing text: '" + sentButNotCommited + "'");

        // update fullText
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "fullText before: '" + fullText + "'");
        fullText = fullText.substring(0, fullText.length() - charsToRemoveFromFullText);
        fullText = fullText + charsToAdd;
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "fullText after: '" + fullText + "'");

        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "updateText - check6 ");
        int i = 0;
        while (i < charsToAdd.length())
        {
            char c = charsToAdd.charAt(i);
            if (Character.isHighSurrogate(c))
            {
                serviceKeyboard.logInfo("CastleInputConnection", "surrogate");
                i++;
                char c2 = charsToAdd.charAt(i);
                String s = new String (Character.toChars(Character.toCodePoint(c, c2)));
                serviceKeyboard.messageSend(new String[]{"castle-key-down", "0", s});
                serviceKeyboard.messageSend(new String[]{"castle-key-up", "0", s});
            }
            String s = new String(Character.toChars(c));
            serviceKeyboard.messageSend(new String[]{"castle-key-down", "0", s});
            serviceKeyboard.messageSend(new String[]{"castle-key-up", "0", s});
            i++;
        }
    }
    
    /* Called when user tap Enter key */
    @Override
    public boolean performEditorAction (int editorAction)
    {
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "performEditorAction() - '" + fullText + "'");
        if (EditorInfo.IME_ACTION_DONE == editorAction)
        {
            serviceKeyboard.logInfo("CastleInputConnection", "performEditorAction() - 'EditorInfo.IME_ACTION_DONE'");
            serviceKeyboard.hideKeyboard();
            // we need remove force capture input after hiding the keyboard
            serviceKeyboard.messageSend(new String[]{"castle-keyboard-hide-remove-force-capture-input"});
            return true;
        }
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "performEditorAction() - editorAction: " + Integer.toString(editorAction));
        return false;
    }

    @Override
    public boolean finishComposingText()
    {
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "finishComposingText() - '" + fullText + "'");
        // microsoft swype keyboard needs to clear sentButNotCommited here (not neded by gboard)
        sentButNotCommited = "";
        return true;
    }
    
    @Override
    public ExtractedText getExtractedText (ExtractedTextRequest request, int flags)
    {
        // looks unused so simplified implementation - we always return fullText regardless to request.hintMaxChars
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "getExtractedText() - '" + fullText + "'");
        ExtractedText eText = new ExtractedText();
        eText.text = fullText;
        eText.startOffset = 0;
        eText.selectionEnd = fullText.length();
        eText.selectionStart = fullText.length();
        return eText;
    }

    @Override
    public CharSequence getTextAfterCursor (int n, int flags)
    {
        // we only support cursor at the end of text
        return null;
    }

    /* This method is called by keyboard to show dictionary hints */
    @Override    
    public CharSequence getTextBeforeCursor (int n, int flags)
    {
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "getTextBeforeCursor - number of chars to get: " + Integer.toString(n));
        if (fullText.length() <= n) {
            serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "getTextBeforeCursor - returns: '" + fullText + "'");
            return fullText;
        }
        
        // Checking is n surrogate (second byte of 4byte character)
        int index = fullText.length() - n - 1;
        if (Character.isLowSurrogate(fullText.charAt(index)))
        {
            index++;
            serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "getTextBeforeCursor - returns one char less because it's low surrogate: '" + fullText.substring(index) + "'");    
        }
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "getTextBeforeCursor - returns: '" + fullText.substring(index) + "'");
        return fullText.substring(index);
    }

    @Override
    public CharSequence getSelectedText (int flags)
    {
        // we don't support text selection now
        return null;
    }

    /* When edit box has some text we need sent it to our input connection */
    public void setInitText(String initText)
    {
        fullText = initText;
        sentButNotCommited = "";
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "setInitText() - '" + fullText + "'");
    }
}

/* View used to capture text */
class CastleKeyboardInputView extends View
{
    CastleInputConnection inputConnection;
    ServiceKeyboard serviceKeyboard;
    String initText;
    boolean passMode;

    public CastleKeyboardInputView(ServiceKeyboard service, Context context) 
    {
        super(context);
        serviceKeyboard = service;
        setFocusable(true);
        setFocusableInTouchMode(true); // without this line we can't show keyboard by ServiceKeyboard.showKeyboard()
        initText = "";
        passMode = false;
    }    

    /* See: https://stackoverflow.com/questions/5419766/how-to-capture-soft-keyboard-input-in-a-view */
    @Override
    public InputConnection onCreateInputConnection(EditorInfo outAttrs) 
    {
        inputConnection = new CastleInputConnection(serviceKeyboard, this, false);

        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "onCreateInputConnection - initText: '" + initText + "'");
        inputConnection.setInitText(initText);
        // outAttrs.inputType = InputType.TYPE_NULL; // only soft key events - when this is set keyboard returns key events but only ASCI characters are available
        // Because InputType.TYPE_NULL don't support some characters we need change to InputType.TYPE_CLASS_TEXT
        // but then key events are not sent, so we need use commitText() and create them our self
        outAttrs.inputType = InputType.TYPE_CLASS_TEXT; 

        // We use here EditorInfo.IME_ACTION_DONE that is used to close keyboard in CastleInputConnection.PerformAction() to close on screen keyboard
        outAttrs.imeOptions = EditorInfo.IME_FLAG_NO_FULLSCREEN | EditorInfo.IME_FLAG_NO_EXTRACT_UI | EditorInfo.IME_ACTION_DONE;

        if (passMode)
        {
            serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "onCreateInputConnection - outAttrs - password mode");
            outAttrs.imeOptions = outAttrs.imeOptions | EditorInfo.IME_FLAG_NO_PERSONALIZED_LEARNING; /* | EditorInfo.IME_FLAG_FORCE_ASCII; // perhaps too restrictive */
            outAttrs.inputType = outAttrs.inputType | InputType.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD;
        }
        
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "onCreateInputConnection - outAttrs.initialSelEnd/Start: " + Integer.toString(initText.length()));
        outAttrs.initialSelEnd = initText.length();
        outAttrs.initialSelStart = initText.length();

        return inputConnection;
    }

    public void setInitText(String text)
    {
        initText = text;
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "setInitText - initText: '" + initText + "'");

        if (inputConnection != null)
            inputConnection.setInitText(initText);
    }

    public void setPasswordMode(boolean passMode)
    {
        this.passMode = passMode;    
    }

    /* This callback is used when user uses navigation back button to hide keyboard.
       We need then hide focus */
    @Override
    public boolean onKeyPreIme (int keyCode, KeyEvent event) 
    {
        serviceKeyboard.logInfoInDebugMode("CastleInputConnection", "onKeyPreIme: " + Integer.toString(keyCode));

        if (event.getAction() == KeyEvent.ACTION_UP && keyCode == KeyEvent.KEYCODE_BACK)
        {
            // we need remove force capture input after hiding the keyboard
            serviceKeyboard.messageSend(new String[]{"castle-keyboard-hide-remove-force-capture-input"});
            return false; // let the key go to ime
        }
        return false;
    }
}

/**
 * Service for keyboard support on android devices
 */
public class ServiceKeyboard extends ServiceAbstract
{

    CastleKeyboardInputView keyboardInputView;
    private final boolean debug = false; // set to true for debug (more logs - a lot more ;)

    /* Creates view that will be used to capture text */
    public ServiceKeyboard(MainActivity activity)
    {
        super(activity);
        keyboardInputView = new CastleKeyboardInputView(this, getActivity().getWindow().getContext());
        // https://stackoverflow.com/questions/40237846/programmatically-add-a-view-to-the-bottom-of-the-screen
        ViewGroup rootView = (ViewGroup) getActivity().findViewById(android.R.id.content);
        rootView.addView(keyboardInputView);
    }

    /* Returns service name */
    public String getName()
    {
        return "keyboard";
    }

    /* Shows software keyboard and sets initial text - the text that is currently in the edit field here should be passed

       See:
       https://stackoverflow.com/questions/5105354/how-to-show-soft-keyboard-when-edittext-is-focused
    */
    public void showKeyboard(String initText, boolean passMode)
    {
        //getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE); - not working
        //getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_VISIBLE); - not working

        InputMethodManager imm = (InputMethodManager) getActivity().getSystemService(Context.INPUT_METHOD_SERVICE);

        // Keyboard can crash when you try open it second time (because it expect a different text while composing)
        // so we try close it here before open - restartInput() fixes that
        keyboardInputView.requestFocus(); // needed to show keyboard work properly
        keyboardInputView.setInitText(initText);
        keyboardInputView.setPasswordMode(passMode);
        logInfoInDebugMode("keyboard", "restart input");
        imm.restartInput(keyboardInputView);
        logInfoInDebugMode("keyboard", "show");
        imm.showSoftInput(keyboardInputView, 0);
    }

    /* Hides keyboard */
    public void hideKeyboard()
    {
        //getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE); - not working
        //getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_VISIBLE); - not working
        InputMethodManager imm = (InputMethodManager) getActivity().getSystemService(Context.INPUT_METHOD_SERVICE);
        imm.hideSoftInputFromWindow(keyboardInputView.getWindowToken(), 0);
        logInfoInDebugMode("keyboard", "hide");
    }

    /* Log only when debug = true */
    public void logInfoInDebugMode(String category, String message)
    {
        if (debug)
            logInfo(category, message);
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 3 && parts[0].equals("castle-show-keyboard")) {
            logInfoInDebugMode("keyboard", "show keyboard '" + parts[1] +"' password mode: '" + parts[2] + "'");
            showKeyboard(parts[1], parts[2].equals("true"));
            return true;
        } else if (parts.length == 1 && parts[0].equals("castle-hide-keyboard")) 
        {
            hideKeyboard();
            return true;
        } else {
            return false;
        }
    }

    
}

