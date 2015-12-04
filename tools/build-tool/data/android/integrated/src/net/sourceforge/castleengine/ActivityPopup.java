/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.widget.LinearLayout;
import android.widget.PopupWindow;
import android.view.ViewGroup.LayoutParams;
import android.view.ViewGroup.MarginLayoutParams;
import android.view.View;
import android.app.Activity;

/**
 * Helper to implement popups, for example to contain ad banners
 * (from any ad SDK --- admob, heyzap...).
 * Allows to display a popup on top of a NativeActivity
 * (that has a fixed UI layout defined by NativeActivity implementation,
 * and so one cannot just add another View inside it easily).
 */
public class ActivityPopup
{
    public ActivityPopup(Activity activity, int gravity, View childView)
    {
        // Inspired by http://www.dynadream.com/ddweb/index.php/Special_Blog?id=20
        popup = new PopupWindow(activity);

        // This is the minimum size for AdMob, we need to set this in case our
        // target device run at 320x480 resolution
        // (Otherwise no ad will be shown, see the padding kill below)
        // Trick from http://www.dynadream.com/ddweb/index.php/Special_Blog?id=20
        popup.setWidth(320);
        popup.setHeight(50);
        popup.setWindowLayoutMode(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
        popup.setClippingEnabled(false);

        LinearLayout layout = new LinearLayout(activity);
        // The layout system for the PopupWindow will kill some pixels due
        // to margins/paddings etc... (No way to remove it), so padd it to adjust
        // Trick from http://www.dynadream.com/ddweb/index.php/Special_Blog?id=20
        layout.setPadding(-10, -10, -10, -10);
        MarginLayoutParams params = new MarginLayoutParams(
            LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
        params.setMargins(0, 0, 0, 0);
        layout.setOrientation(LinearLayout.VERTICAL);
        layout.addView(childView, params);
        popup.setContentView(layout);

        //ViewGroup decorView = (ViewGroup)getWindow().getDecorView();
        //View gameView = decorView.getChildAt(0);
        View gameView = activity.findViewById(android.R.id.content);

        // Note: do not call popup.showAtLocation earlier,
        // see http://stackoverflow.com/questions/17787011/android-view-windowmanagerbadtokenexception-unable-to-add-window-token-null
        popup.showAtLocation(gameView, gravity, 0, 0);
    }

    private PopupWindow popup;

    public void dispose()
    {
        popup.dismiss();
    }
}
