/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

/**
 * Integration of OpenAL with Castle Game Engine on Android.
 */
public class ComponentSound extends ComponentAbstract
{
    private static final String TAG = "${NAME}.castleengine.ComponentSound";

    public ComponentSound(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "sound";
    }
}
