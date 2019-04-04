package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.EmbeddedApp;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the view device window
 *
 * @author llaroussini
 */
public class ViewDeviceWindow extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public ViewDeviceWindow(WebDriver driver) {
        super(driver);
    }

    ///////////////////
    // ID's & Locators
    ///////////////////

    /**
     * Text prefix displayed in the viewing message header
     */
    private static final String VIEW_DEVICE_HEADER_TEXT_PREFIX = "View Device: ";

    /**
     * Xpath to the installed apps content area
     */
    private static final String INSTALLED_APPS_SECTION_XPATH = "//span[text()='Installed Apps']/ancestor::div[6]";

    /**
     * Xpath to the installed apps list
     */
    private static final String INSTALLED_APPS_LIST_XPATH = "//span[text()='Installed Apps']/ancestor::div[6]//table";

    /**
     * Xpath to the app parameters content area
     */
    private static final String APP_PARAMETERS_SECTION_XPATH = "//div[contains (text(), 'App Parameters')]/ancestor::div[contains (@class, 'x-window-item')]";

    /**
     * Xpath to the app parameters list
     */
    private static final String APP_PARAMETERS_LIST_XPATH = "//div[contains(@id, 'appParamsTable')]//table";

    /**
     * Xpath of the Close button
     */
    private static final String CLOSE_BUTTON_XPATH = ".//span[@data-ref='btnInnerEl'][text()='Close']";

    /**
     * Xpath prefix to app row
     */
    private static final String APP_ROW_XPATH_PREFIX = "//span[contains(@id, 'gridcolumn')][text()='Installed Apps']/ancestor::div[contains(@id, 'grid')]//tr//td//div[contains(text(), '";

    /**
     * Xpath prefix to app parameter row
     */
    private static final String APP_PARAM_ROW_XPATH_PREFIX = "//div[contains(@id, 'appParamsTable')]//tr//td//div[contains(text(),'";

    /**
     * Xpath suffix to app row
     */
    private static final String APP_ROW_XPATH_SUFFIX = "')]/ancestor::tr";

    ///////////////////
    // Getters
    ///////////////////

    /**
     * Gets the installed apps section
     *
     * @return the installed apps section element
     */
    private El getInstalledAppsSection() {
        return el(By.xpath(INSTALLED_APPS_SECTION_XPATH));
    }

    /**
     * Gets the app parameters section
     *
     * @return the app parameters section element
     */
    private El getAppParametersSection() {
        return el(By.xpath(APP_PARAMETERS_SECTION_XPATH));
    }

    /**
     * Gets the row in Installed Apps table with the given app
     *
     * @param appName -the name of the app expected
     * @return the row with the given app
     */
    private El getInstalledAppRow(String appName) {
        return el(By.xpath(APP_ROW_XPATH_PREFIX + appName + APP_ROW_XPATH_SUFFIX));
    }

    /**
     * Gets the row in App Parameters table with the given parameter
     *
     * @param parameterName -the name of the parameter expected
     * @return the row with the given parameter
     */
    private El getAppParameterRow(String parameterName) {
        return el(By.xpath(APP_PARAM_ROW_XPATH_PREFIX + parameterName + APP_ROW_XPATH_SUFFIX));
    }

    /**
     * Gets the close button
     *
     * @param deviceName -the name of the device
     * @return the close button
     */
    private El getCloseButton(String deviceName) {
        El window = el(By.xpath(TITLE_XPATH_PREFIX + VIEW_DEVICE_HEADER_TEXT_PREFIX + deviceName + "')]" + FORM_WINDOW_PARENT_XPATH));
        return window.el(By.xpath(CLOSE_BUTTON_XPATH));
    }

    ///////////////////
    // Checkers
    ///////////////////

    /**
     * Checks to see if the view device header is displayed
     *
     * @param deviceName -the name of the device
     * @return true if the header is displayed, false otherwise
     */
    public boolean isViewDeviceHeaderDisplayed(String deviceName) {
        return isHeaderDisplayed(VIEW_DEVICE_HEADER_TEXT_PREFIX + deviceName);

    }

    /**
     * Checks to see if installed apps section is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInstalledAppsSectionDisplayed() {
        return isElementDisplayed(getInstalledAppsSection());
    }

    /**
     * Checks to see if app parameters section is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isAppParametersSectionDisplayed() {
        return isElementDisplayed(getAppParametersSection());
    }

    /**
     * Checks to see if the expected app is displayed in the Installed Apps
     * table
     *
     * @param app -the app expected
     * @return true if displayed, false otherwise
     */
    public boolean isInstalledAppDisplayed(EmbeddedApp app) {
        String appName = app.getName();
        return isInstalledAppDisplayed(appName);
    }

    /**
     * Checks to see if the expected app is displayed in the Installed Apps
     * table
     *
     * @param appName -the name of the app expected
     * @return true if displayed, false otherwise
     */
    public boolean isInstalledAppDisplayed(String appName) {
        return isElementDisplayed(getInstalledAppRow(appName));
    }

    /**
     * Checks to see if the expected app parameter is displayed in the App
     * Parameters table
     *
     * @param app -the app with parameter expected
     * @return true if displayed, false otherwise
     */
    public boolean isAppParameterDisplayed(EmbeddedApp app) {
        String parameterName = app.getParameters().get(0).getParameterName();
        return isAppParameterDisplayed(parameterName);
    }

    /**
     * Checks to see if the expected app parameter is displayed in the App
     * Parameters table
     *
     * @param parameterName -the name of the parameter expected
     * @return true if displayed, false otherwise
     */
    public boolean isAppParameterDisplayed(String parameterName) {
        return isElementDisplayed(getAppParameterRow(parameterName));
    }

    /**
     * Checks to see if the close button is displayed in window
     *
     * @param deviceName -the name of the device
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCloseBtnDisplayed(String deviceName) {
        return isElementDisplayed(getCloseButton(deviceName));
    }

    /////////////////
    // Utilities
    ////////////////

    /**
     * Click the Close button and waits for window to close
     *
     * @param deviceName -the name of the device
     */
    public void clickCloseButton(String deviceName) {
        getCloseButton(deviceName).click();
        waitForElementToBeInvisible(By.xpath(VIEW_DEVICE_HEADER_TEXT_PREFIX + deviceName));
    }

    /**
     * Verifies Installed Apps and App Parameters sections display
     */
    public void checkDeviceAppSections() {
        Assert.assertTrue("Installed Apps section could not be found.", isInstalledAppsSectionDisplayed());
        Assert.assertTrue("App Parameters section could not be found.", isAppParametersSectionDisplayed());
    }

    /**
     * Clicks the row for the given installed app
     *
     * @param app -the app to click
     */
    public void clickInstalledApp(EmbeddedApp app) {
        String appName = app.getName();
        clickInstalledApp(appName);
    }

    /**
     * Clicks the row for the given installed app
     *
     * @param appName -the name of the app to click
     */
    public void clickInstalledApp(String appName) {
        getInstalledAppRow(appName).click();
    }

    /////////
    // Waits
    /////////

    /**
     * Waits for list to be displayed in Installed Apps section
     */
    public void waitForInstalledAppList() {
        waitForElementToBePresent(By.xpath(INSTALLED_APPS_LIST_XPATH));
    }

    /**
     * Waits for list to be displayed in App Parameters section
     */
    public void waitForAppParamsList() {
        waitForElementToBePresent(By.xpath(APP_PARAMETERS_LIST_XPATH));
    }
}
