package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps;

import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.EmbeddedApp;
import com.harmonia.qa.ETEXASWebQATests.enums.DeviceType;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.AppsPage;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Partial page class representing the Built In Apps page
 *
 * @author llaroussini
 */
public class EmbeddedAppsPartialPage extends AppsPage {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public EmbeddedAppsPartialPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //Element ID's & Identifiers
    ///////////

    /**
     * Xpath constant for table row ancestor
     */
    private static final String ANCESTOR_TR_XPATH = "/ancestor::tr";

    /**
     * Xpath prefix for the Embedded Apps div container
     */
    private static final String EMBEDDED_APPS_TABLE_PREFIX = "//div[contains(@id, 'tableview')]";

    /**
     * Xpath of the Embedded Apps tables
     */
    private static final String EMBEDDED_APPS_TABLE_XPATH = EMBEDDED_APPS_TABLE_PREFIX + "//table";

    /**
     * Xpath prefix of the Embedded Application Row
     */
    private static final String EMBEDDED_APPS_ROW_XPATH_PREFIX = EMBEDDED_APPS_TABLE_XPATH + "//div[contains(text(),'";

    /**
     * Xpath of the embedded app row ID
     */
    private static final String ROW_APP_ID_XPATH = "/td[contains(@class, 'x-grid-cell-id-column')]";

    /**
     * Xpath of the Parameters button
     */
    private static final String PARAMETERS_BTN_XPATH = "//div[contains(@id, 'embedded-application-profile-toolbar')]//span[text()='Parameters']/ancestor::a";

    /**
     * Xpath prefix to column header cells
     */
    private static final String COLUMN_HEADER_XPATH_PREFIX = "//div[contains(@id, 'embedded-application-profile-grid')]//div[contains(@id, 'headercontainer')]//span[text()='";

    /**
     * Class of disabled button
     */
    protected static final String DISABLED_BTN_CLASS = "x-item-disabled";

    /**
     * String constant for 'class'
     */
    private static final String ATTR_CLASS = "class";

    /**
     * String constant for 'aria-selected'
     */
    private static final String ARIA_SELECTED = "aria-selected";

    /**
     * String constant for 'true'
     */
    private static final String TRUE = "true";

    ///////////
    //Element Getters
    ///////////

    /**
     * Gets the Embedded Apps table
     *
     * @return the table element
     */
    private El getEmbeddedAppsTable() {
        return el(By.xpath(EMBEDDED_APPS_TABLE_XPATH));
    }

    /**
     * Gets specific app row
     *
     * @return the row element
     */
    private El getEmbeddedAppRow(String appName) {
        return el(By.xpath(EMBEDDED_APPS_ROW_XPATH_PREFIX + appName + "')]" + ANCESTOR_TR_XPATH));
    }

    /**
     * Gets the ID element of the specified app
     *
     * @param appName - The String name of the app
     * @return the ID element for the app
     */
    private El getEmbeddedAppID(String appName) {
        El appRow = getEmbeddedAppRow(appName);
        return appRow.el(By.xpath("." + ROW_APP_ID_XPATH));
    }

    /**
     * Gets Parameters button
     *
     * @return the Parameters button element
     */
    private El getParametersBtn() {
        return el(By.xpath(PARAMETERS_BTN_XPATH));
    }

    /**
     * Gets given app column header
     *
     * @param header -the native app column header to get
     * @return the column header cell element
     */
    private El getColumnHeader(AppColumnHeader header) {
        return el(By.xpath(COLUMN_HEADER_XPATH_PREFIX + header.getLabel() + "']"));
    }

    ///////////
    //Element Checkers
    ///////////

    /**
     * Checks if Embedded Apps table is displayed (only displays if apps are
     * present)
     *
     * @return true if displayed, false otherwise
     */
    public boolean areEmbeddedAppsDisplayed() {
        return isElementDisplayed(getEmbeddedAppsTable());
    }

    /**
     * Checks if row for app with given name is displayed
     *
     * @param appName -the name of the app expected
     * @return true if displayed, false otherwise
     */
    public boolean isAppDisplayed(String appName) {
        return isElementDisplayed(getEmbeddedAppRow(appName));
    }

    /**
     * Checks if row for given app is displayed
     *
     * @param app -the Embedded App expected
     * @return true if displayed, false otherwise
     */
    public boolean isAppDisplayed(EmbeddedApp app) {
        String appName = app.getName();
        return isAppDisplayed(appName);
    }

    /**
     * Checks if info icon is displayed for given app
     *
     * @param appName -the name of the app expected
     * @return true if icon displayed, false otherwise
     */
    public boolean isInfoIconDisplayed(String appName) {
        return isElementDisplayed(getInfoIcon(getEmbeddedAppRow(appName)));
    }

    /**
     * Checks if the Parameters button is displayed
     *
     * @return true if the button is displayed, otherwise false
     */
    public boolean isParametersBtnDisplayed() {
        return isElementDisplayed(getParametersBtn());
    }

    /**
     * Checks if info icon is displayed for given app
     *
     * @param app -the built in app expected
     * @return true if icon displayed, false otherwise
     */
    public boolean isInfoIconDisplayed(EmbeddedApp app) {
        String appName = app.getName();
        return isInfoIconDisplayed(appName);
    }

    /**
     * Checks to see if the Parameters button is disabled
     *
     * @return true if the button is disabled, otherwise false
     */
    public boolean isParametersBtnDisabled() {
        String btnClass = getParametersBtn().getAttribute(ATTR_CLASS);
        return btnClass.contains(DISABLED_BTN_CLASS);
    }

    /**
     * Checks to see if the ID is displayed for an app given by the app name
     *
     * @param appName - the String name of the app being checked against
     * @return true if the element is displayed, otherwise false
     */
    public boolean isAppIDDisplayed(String appName) {
        String ID = getEmbeddedAppID(appName).getText();
        if (ID.equals("") || ID == null) {
            return false;
        }
        else {
            return true;
        }
    }

    /**
     * Checks if an app is selected
     *
     * @param appName - the String name of the app being checked
     * @return true if the app is selected, otherwise false
     */
    public boolean isAppSelected(String appName) {
        String ariaSelected = getEmbeddedAppRow(appName).getAttribute(ARIA_SELECTED);
        return ariaSelected.contains(TRUE);
    }

    /**
     * /** Checks if expected device type is displayed for given device
     *
     * @param appName -the name of the device expected
     * @param appType -the device type expected
     * @return true if type displayed, false otherwise
     */
    public boolean isAppTypeDisplayed(String appName, DeviceType appType) {
        return isElementDisplayed(getAppTypeDisplayed(getEmbeddedAppRow(appName), appType));
    }

    /**
     * Checks if info icon is displayed for given app
     *
     * @param app -the built in app expected
     * @return true if icon displayed, false otherwise
     */
    public boolean isAppTypeDisplayed(EmbeddedApp app) {
        String appName = app.getName();
        DeviceType appType = app.getDeviceType();
        return isAppTypeDisplayed(appName, appType);
    }

    /**
     * Checks to see if the given app column header cell is displayed
     *
     * @param header -the remote app column header expected
     * @return true if header cell displayed, false otherwise
     */
    public boolean isColumnHeaderCellDisplayed(AppColumnHeader header) {
        return isElementDisplayed(getColumnHeader(header));
    }

    //////////////
    // Interaction
    //////////////

    /**
     * Clicks the info icon associated with the given app
     *
     * @param appName -the name of the app whos info icon should be clicked
     */
    public ApplicationParametersModal clickInfoIcon(String appName) {
        getInfoIcon(getEmbeddedAppRow(appName)).click();
        return getPage(ApplicationParametersModal.class);
    }

    /**
     * Clicks the info icon associated with the given app
     *
     * @param app -the embedded app whos info icon should be clicked
     */
    public ApplicationParametersModal clickInfoIcon(EmbeddedApp app) {
        String appName = app.getName();
        return clickInfoIcon(appName);
    }

    /**
     * Selects an app from the table list based on the app name
     *
     * @param appName - the String name of the app being selected
     */
    public void selectApp(String appName) {
        getEmbeddedAppRow(appName).click();
    }

    /**
     * Selects the Parameters button based on which app was selected and returns
     * the Parameters Modal
     *
     * @param appName - the String name of the app being selected
     * @return a ViewingAppParametersWindow modal
     */
    public ApplicationParametersModal clickParametersBtn(String appName) {
        selectApp(appName);
        getParametersBtn().click();
        return getPage(ApplicationParametersModal.class);
    }

    ////////////
    // Utilities
    ///////////

    /**
     * Checks all apps in given list and ensures they display
     *
     * @param appList -the list of apps expected
     */
    public void checkAllEmbeddedAppsDisplayed(List<EmbeddedApp> appList) {
        for (EmbeddedApp app : appList) {
            String appName = app.getName();
            Assert.assertTrue("App with name, " + appName + ", not displayed as expected.", isAppDisplayed(appName));
        }
    }

    /**
     * Checks each app row and verifies and ID is set
     *
     * @param appList - the list of apps expected
     */
    public void checkAllEmbeddedAppIDsDisplayed(List<EmbeddedApp> appList) {
        for (EmbeddedApp app : appList) {
            String appName = app.getName();
            Assert.assertTrue("App ID was not displayed as expected", isAppIDDisplayed(appName));
        }
    }

    /**
     * Checks application list and verifies each row has a unique ID
     *
     * @param appList - the list of apps expected to have unique IDs
     */
    public void checkAllEmbeddedAppIDsUnique(List<EmbeddedApp> appList) {
        //Populates String ArrayList with the IDs of each application row
        List<String> IDList = new ArrayList<String>();
        int length;
        for (EmbeddedApp app : appList) {
            String appName = app.getName();
            String appID = getEmbeddedAppID(appName).getText();
            IDList.add(appID);
        }

        //Checks each application ID to ensure uniqueness
        length = IDList.size();
        for (int j = 0; j < length; j++) {
            for (int k = j + 1; k < length; k++) {
                Assert.assertFalse("Embedded App IDs were found to have duplicate values where unique values were expected.", IDList.get(j).equalsIgnoreCase(IDList.get(k)));
            }
        }
    }

    /**
     * Checks that info icons display for all apps in given list
     *
     * @param appList -the list of apps expected
     */
    public void checkAllEmbeddedAppsInfoIconsDisplayed(List<EmbeddedApp> appList) {
        for (EmbeddedApp app : appList) {
            String appName = app.getName();
            Assert.assertTrue("Embedded App with name, " + appName + ", does not have info icon displayed as expected.", isInfoIconDisplayed(appName));
        }
    }

    /**
     * Checks that expected device types display for all apps in given list
     *
     * @param appList -the list of apps expected
     */
    public void checkAllEmbeddedAppTypesDisplayed(List<EmbeddedApp> appList) {
        for (EmbeddedApp app : appList) {
            String appName = app.getName();
            DeviceType deviceType = app.getDeviceType();
            Assert.assertTrue("Embedded App with name, " + appName + ", does not have expected app type of, " + deviceType.getUILabel() + ", displayed as expected.", isAppTypeDisplayed(app));
        }
    }

    /**
     * Clicks (i) icon and checks parameters display as expected for all apps in
     * given list
     *
     * @param appList -the list of apps to check
     */
    public void checkAllEmbeddedAppsParameters(List<EmbeddedApp> appList) {
        for (EmbeddedApp app : appList) {
            String appName = app.getName();
            ApplicationParametersModal paramWindow = clickInfoIcon(appName);
            paramWindow.checkAppParameters(app);
            paramWindow.clickCloseButton();
        }
    }

    /**
     * Checks for each expected header column to display correctly based on a
     * test step number
     *
     * @param testStep - A String value of which test step the check is being
     *        performed for.
     */
    public void checkAllColumnHeaders(String testStep) {
        Assert.assertTrue("ID Header not displayed when checking test step " + testStep, isColumnHeaderCellDisplayed(AppColumnHeader.APP_ID));
        Assert.assertTrue("Name header not displayed when checking test step " + testStep, isColumnHeaderCellDisplayed(AppColumnHeader.APP_NAME));
        Assert.assertTrue("Device Type header not displayed when checking test step " + testStep, isColumnHeaderCellDisplayed(AppColumnHeader.DEVICE_TYPE));
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(EMBEDDED_APPS_TABLE_XPATH));
    }

}
