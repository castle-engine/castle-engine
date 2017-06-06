/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

/**
 * Integration of Tremolo (OggVorbis) with Castle Game Engine on Android.
 */
public class ServiceOggVorbis extends ServiceAbstract
{
    private static final String TAG = "${NAME}.castleengine.ServiceOggVorbis";

    public ServiceOggVorbis(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "ogg_vorbis";
    }
}
