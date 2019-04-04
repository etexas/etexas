package com.harmonia.qa.ETEXASWebQATests.webdriver.bases;

import org.junit.Rule;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.PageFactory;
import org.openqa.selenium.support.ui.WebDriverWait;

import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasProperties;
import com.harmonia.qa.par.Utilities.rules.ETexasTestWatcher;
import com.harmonia.qa.webdriver.utilities.BasicWebDriverManager;
import com.harmonia.qa.webdriver.utilities.ScreenshotTestRule;

/**
 * A standard test base for ETexas tests.
 *
 * @author llaroussini
 */
public class ETexasTestBase extends ETexasCommonUtils implements ETexasProperties {

    /**
     * Instance of the ScreenshotTestRule to capture screenshots for test
     * failures.
     */
    @Rule
    public ScreenshotTestRule screenshotRule = new ScreenshotTestRule();

    /**
     * JUnit rule for executing and evaluating test statements
     */
    @Rule
    public ETexasTestWatcher watchIt = new ETexasTestWatcher();

    /**
     * The file name and path where entity data will be stored
     */
    private static String jsonFileName;

    /**
     * Get a page with the default driver
     *
     * @param <T> the type of page to expect in return
     * @param page the class object of the page to retrieve (any class with a
     *        constructor that accepts a driver)
     * @return the page
     */
    public static <T> T getPage(Class<T> page) {
        try {
            return PageFactory.initElements(driver(), page);
        }
        catch (Exception e) {
            throw new IllegalStateException("Failed to load page: " + page.getName(), e);
        }
    }

    /**
     * Sets the file name (and path) where entity data will be stored
     *
     * @param fileName the file name and path where entity data will be saved
     */
    public static void setJsonFileName(String fileName) {
        jsonFileName = fileName;
    }

    /**
     * Gets the JSON file name used to store Entity Data in the current testing
     * context
     *
     * @return the filename where entity data should be stored
     */
    public static String getJsonFileName() {
        return jsonFileName;
    }

    /************/
    /** Waits **/
    /************/

    /**
     * Get a wait object with a default timeout
     *
     * @return a wait object
     */
    public static WebDriverWait newWait() {
        return newWait(IMPLICIT_WAIT);
    }

    /**
     * Get a wait object with the supplied timeout
     *
     * @param timeOut_Seconds how long, in seconds, that the wait should happen
     *        until an error is thrown
     * @return a new wait object
     */
    public static WebDriverWait newWait(int timeOut_Seconds) {
        return new WebDriverWait(driver(), timeOut_Seconds);
    }

    /************************/
    /** WebDriver proxying **/
    /************************/

    /**
     * WebDriver accessor
     *
     * @return the managed web driver object
     */
    public static WebDriver driver() {
        return BasicWebDriverManager.get().driver();
    }

    /**
     * Quits the driver.
     */
    public static void quit() {
        BasicWebDriverManager.get().quit();
    }
}
