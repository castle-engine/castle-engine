/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

/**
 * Integration of Tremolo (OggVorbis) with Castle Game Engine on Android.
 */
public class ComponentOggVorbis extends ComponentAbstract
{
    private static final String TAG = "${NAME}.castleengine.ComponentOggVorbis";

    public ComponentOggVorbis(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "ogg_vorbis";
    }
}
