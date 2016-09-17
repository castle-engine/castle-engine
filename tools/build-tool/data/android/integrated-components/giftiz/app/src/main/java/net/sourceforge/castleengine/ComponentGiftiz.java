/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.util.Log;

import com.purplebrain.giftiz.sdk.GiftizSDK;
import com.purplebrain.giftiz.sdk.GiftizSDK.Inner.ButtonNeedsUpdateDelegate;

/**
 * Giftiz (http://giftiz.com/)
 * integration with Castle Game Engine Android application.
 */
public class ComponentGiftiz extends ComponentAbstract
    implements ButtonNeedsUpdateDelegate
{
    private static final String TAG = "${NAME}.castleengine.ComponentGiftiz";

    public ComponentGiftiz(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "giftiz";
    }

    @Override
    public void onCreate() {
        super.onCreate();
        GiftizSDK.Inner.setButtonNeedsUpdateDelegate(this);
    }

    @Override
    public void onResume()
    {
        super.onResume();
        GiftizSDK.onResumeMainActivity(getActivity());
        updateButtonImage();
    }

    @Override
    public void buttonNeedsUpdate()
    {
        updateButtonImage();
    }

    private void updateButtonImage()
    {
        switch (GiftizSDK.Inner.getButtonStatus(getActivity())) {
            case ButtonInvisible: messageSend(new String[]{"giftiz-button-status", "invisible"}); break;
            case ButtonNaked: messageSend(new String[]{"giftiz-button-status", "naked"}); break;
            case ButtonBadge: messageSend(new String[]{"giftiz-button-status", "badge"}); break;
            case ButtonWarning: messageSend(new String[]{"giftiz-button-status", "warning"}); break;
        }
    }

    @Override
    public void onPause()
    {
        super.onPause();
        GiftizSDK.onPauseMainActivity(getActivity());
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 1 && parts[0].equals("giftiz-mission-complete")) {
            GiftizSDK.missionComplete(getActivity());
            return true;
        } else
        if (parts.length == 1 && parts[0].equals("giftiz-button-clicked")) {
            GiftizSDK.Inner.buttonClicked(getActivity());
            return true;
        } else {
            return false;
        }
    }
}
