/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

/**
 * Integration of OpenAL with Castle Game Engine on Android.
 */
public class ServiceSound extends ServiceAbstract
{
    private static final String TAG = "${NAME}.castleengine.ServiceSound";

    public ServiceSound(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "sound";
    }
}
