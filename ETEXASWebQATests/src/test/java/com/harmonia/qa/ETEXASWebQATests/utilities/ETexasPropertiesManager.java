package com.harmonia.qa.ETEXASWebQATests.utilities;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * Gets properties from the text file for use in the ETexasProperties interface
 * 
 * @author cbulloss
 */
public class ETexasPropertiesManager {

    /**
     * The name of the properties file accessible by java.
     */
    private static final String BUNDLE_NAME = "etexas";

    /**
     * The actual resource bundle which exposes the values in the properties
     * file.
     */
    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

    /**
     * Prevent construction
     */
    private ETexasPropertiesManager() {
    }

    /**
     * Gets a value from the messages file
     * 
     * @param key the key for the value to retrieve
     * @return the value associated with the key
     */
    public static String getString(String key) {
        try {
            return RESOURCE_BUNDLE.getString(key);
        }
        catch (MissingResourceException e) {
            return '!' + key + '!';
        }
    }
}
