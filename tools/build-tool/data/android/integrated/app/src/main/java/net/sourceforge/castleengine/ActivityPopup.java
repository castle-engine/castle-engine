/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.widget.LinearLayout;
import android.widget.PopupWindow;
import android.view.ViewGroup.LayoutParams;
import android.view.ViewGroup.MarginLayoutParams;
import android.view.View;
import android.app.Activity;
import android.os.Build;
import android.util.Log;

/**
 * Helper to implement popups, for example to contain ad banners
 * (from any ad SDK --- admob...).
 * Allows to display a popup on top of a NativeActivity
 * (that has a fixed UI layout defined by NativeActivity implementation,
 * and so one cannot just add another View inside it easily).
 */
public class ActivityPopup implements View.OnLayoutChangeListener
{
    private static final String CATEGORY = "ActivityPopup";

    private View parentView;
    private PopupWindow popup;
    private ServiceAbstract service;

    public ActivityPopup(ServiceAbstract aService, int gravity, View childView)
    {
        service = aService;
        Activity activity = service.getActivity();

        // Inspired by http://www.dynadream.com/ddweb/index.php/Special_Blog?id=20
        popup = new PopupWindow(activity);

        /*
        if (smallAdMobSize) {
            // This is the minimum size for AdMob, we need to set this in case our
            // target device run at 320x480 resolution
            // (Otherwise no ad will be shown, see the padding kill below)
            // Trick from http://www.dynadream.com/ddweb/index.php/Special_Blog?id=20
            popup.setWidth(320);
            popup.setHeight(50);
            popup.setWindowLayoutMode(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
        } else {
        */

        // For better-paying ads, you want to give more space.
        // AdMob docs suggest using SMART_BANNER.
        // And SMART_BANNER docs say:
        //
        //   Note: The smart banner view in your layout must consume the full width of the device. If it doesn't, you'll get a warning with the message "Not enough space to show ad", and the banner will not be displayed.
        //
        // on https://developers.google.com/admob/android/banner?hl=en

        //popup.setWindowLayoutMode(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT);
        // above deprecated in API >= 23
        popup.setWidth(LayoutParams.MATCH_PARENT);
        popup.setHeight(LayoutParams.WRAP_CONTENT);
        popup.setClippingEnabled(false);
        //popup.setBackgroundDrawable(null);

        LinearLayout layout = new LinearLayout(activity);
        layout.setOrientation(LinearLayout.VERTICAL);
        /*
        if (smallAdMobSize) {
            // The layout system for the PopupWindow will kill some pixels due
            // to margins/paddings etc... (No way to remove it), so padd it to adjust
            // Trick from http://www.dynadream.com/ddweb/index.php/Special_Blog?id=20
            layout.setPadding(-10, -10, -10, -10);
            MarginLayoutParams params = new MarginLayoutParams(
                LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
            params.setMargins(0, 0, 0, 0);
            layout.addView(childView, params);
        } else {
        */

        layout.addView(childView);
        popup.setContentView(layout);

        //ViewGroup decorView = (ViewGroup)getWindow().getDecorView();
        //parentView = decorView.getChildAt(0);
        parentView = activity.findViewById(android.R.id.content);

        // Note: do not call popup.showAtLocation earlier,
        // see http://stackoverflow.com/questions/17787011/android-view-windowmanagerbadtokenexception-unable-to-add-window-token-null
        popup.showAtLocation(parentView, gravity, 0, 0);

        //ServiceAbstract.logInfo(CATEGORY, "build version is " + Build.VERSION.SDK_INT);
        if (Build.VERSION.SDK_INT >= 11) {
            layout.addOnLayoutChangeListener(this);
        }
    }

    public void onLayoutChange(View v,
        int left, int top, int right, int bottom,
        int oldLeft, int oldTop, int oldRight, int oldBottom)
    {
        int width = right - left;
        int height = bottom - top;

        int[] leftTopScreen = new int[2];
        v.getLocationOnScreen(leftTopScreen);
        int parentHeight = parentView.getHeight();
        int screenLeft = leftTopScreen[0];
        int screenBottom = parentHeight - (leftTopScreen[1] + height); // 0 is bottom for CGE

        // Now it's almost Ok.
        // Except there's additional rectangle border, not accounted for in current
        // screenLeft, screenBottom, width, height. But we get guess the size of this border
        // by looking at top/left here (relative to parent, whatever the parent is for LinearLayout
        // --- I'm guessing some internal PopupWindow view?...).
        int horizontalBorder = left;
        int verticalBorder = top;
        screenLeft -= horizontalBorder;
        screenBottom -= verticalBorder;
        width += horizontalBorder * 2;
        height += verticalBorder * 2;

        sendBannerSize(screenLeft, screenBottom, width, height);
    }

    private void sendBannerSize(int screenLeft, int screenBottom, int width, int height)
    {
        service.messageSend(new String[]{"ads-" + service.getName() + "-banner-size",
            Integer.toString(screenLeft),
            Integer.toString(screenBottom),
            Integer.toString(width),
            Integer.toString(height)});
    }

    public void dispose()
    {
        popup.dismiss();
        popup = null;
        sendBannerSize(0, 0, 0, 0); // equal to TRectangle.Empty in CGE
    }
}
