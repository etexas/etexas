package com.harmonia.qa.ETEXASWebQATests.utilities;

import java.util.Random;

/**
 * Random data generator specific to values needed in ETexas UI
 * 
 * @author llaroussini
 */
public class ETexasRandomDataGenerator {

    /**
     * Instance of Random
     */
    private static final Random random = new Random();

    /**
     * Gets a random Latitude value (between -90 and 90)
     *
     * @return random Latitude value
     */
    public static Integer randomLatitudeValue() {
        return random.nextInt(181) - 90;
    }

    /**
     * Gets a random Longitude value (between -180 and 180)
     *
     * @return random Longitude value
     */
    public static Integer randomLongitudeValue() {
        return random.nextInt(361) - 180;
    }

}
