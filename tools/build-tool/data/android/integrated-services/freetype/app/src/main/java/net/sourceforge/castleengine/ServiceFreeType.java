/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

/**
 * Integration of FreeType with Castle Game Engine on Android.
 */
public class ServiceFreeType extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceFreeType";

    public ServiceFreeType(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "freetype";
    }
}
