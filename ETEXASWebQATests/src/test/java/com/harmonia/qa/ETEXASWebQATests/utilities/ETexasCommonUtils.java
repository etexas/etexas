package com.harmonia.qa.ETEXASWebQATests.utilities;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.webdriver.tests.base.CommonUtils;

/**
 * Commonly used utilities for eTexas automation
 *
 * @author cbulloss
 */
public class ETexasCommonUtils extends CommonUtils implements ETexasProperties {

    /**
     * Navigates to the landing page
     *
     * @return the newly loaded landing page
     */
    public static LandingPage goToLandingPage() {
        driver().get(LANDING_PAGE_URL);
        return getPage(LandingPage.class);
    }

    /**
     * Sleeps for a specified period of time
     *
     * @param millis the period of time to sleep in millis
     */
    public static void sleep(long millis) {
        long start = System.currentTimeMillis();
        try {
            Thread.sleep(millis);
        }
        catch (InterruptedException e) {//Ignore
        }
        long end = System.currentTimeMillis();
        logSleepWait(end - start);
    }

    /**
     * Parses the raw JSON response
     *
     * @param string the JSON string that was returned from the server
     * @return a JsonNode that contains only the values that need to be verified
     */
    public static JsonNode parseGetApplicationResponse(String string) {
        ObjectMapper objectMapper = new ObjectMapper();
        JsonNode test = null;
        try {
            test = objectMapper.readTree(string);
        }
        catch (JsonProcessingException e) {
            e.printStackTrace();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        return test;
    }
}
