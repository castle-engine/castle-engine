/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import java.util.Random;

/**
 * Random string for biling payload,
 * code based on http://stackoverflow.com/questions/18613520/what-should-be-the-developer-payload-in-android-in-app-billing-v3-api
 * http://stackoverflow.com/questions/17196562/token-that-identify-the-user/17205999#17205999
 */
public class RandomString {
    private static final char[] symbols = new char[36];

    static {
        for (int idx = 0; idx < 10; ++idx)
            symbols[idx] = (char) ('0' + idx);
        for (int idx = 10; idx < 36; ++idx)
            symbols[idx] = (char) ('a' + idx - 10);
    }

    private final Random random = new Random();

    private final char[] buf;

    public RandomString(int length) {
        if (length < 1)
            throw new IllegalArgumentException("length < 1: " + length);
        buf = new char[length];
    }

    public String nextString() {
        for (int idx = 0; idx < buf.length; ++idx)
            buf[idx] = symbols[random.nextInt(symbols.length)];
        return new String(buf);
    }
}
