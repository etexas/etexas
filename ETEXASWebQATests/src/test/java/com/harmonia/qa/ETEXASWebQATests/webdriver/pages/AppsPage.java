package com.harmonia.qa.ETEXASWebQATests.webdriver.pages;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.enums.DeviceType;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.EmbeddedAppsPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.JARApplicationsPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.NativeAppsPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.RemoteApplicationsPartialPage;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the eTexas Apps Page
 *
 * @author llaroussini
 */
public class AppsPage extends DashboardPage {

    /**
     * Default constructor
     *
     * @param driver the web driver instance being used
     */
    public AppsPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    /**
     * Tabs on Apps page
     *
     * @author llaroussini
     */
    public enum AppTab {
        /**
         * The Native Applications tab
         */
        NATIVE_APPS("Native Applications"),
        /**
         * The Remote Applications tab
         */
        REMOTE_APPS("Remote Applications"),
        /**
         * The JAR Applications tab
         */
        JAR_APPS("JAR Applications"),
        /**
         * The Embedded Applications tab
         */
        EMBEDDED_APPS("Embedded Applications");

        /**
         * The label of the titles of the tabs as they appear in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        AppTab(String label) {
            this.label = label;
        }

        /**
         * Gets the label (title) associated with the tab as it is displayed in
         * the Web UI
         *
         * @return The label of the header cell
         */
        public String getLabel() {
            return this.label;
        }
    }

    /**
     * App Column headers
     *
     * @author llaroussini
     */
    public enum AppColumnHeader {
        /**
         * ID column header
         */
        APP_NAME("Name"),
        /**
         * App ID Column Header
         */
        APP_ID("ID"),
        /**
         * Device Type Column Header
         */
        DEVICE_TYPE("Device Type"),
        /**
         * Command Line Column Header
         */
        CMD_LINE("Command Line"),
        /**
         * Host Address Column Header
         */
        HOST_ADDR("Host Address"),
        /**
         * Port Column Header
         */
        PORT_HEADER("Port");

        /**
         * The label of the column headers as displayed in the UI
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        AppColumnHeader(String label) {
            this.label = label;
        }

        /**
         * Gets the label of the column header as displayed in the UI
         *
         * @return The label of the column header
         */
        public String getLabel() {
            return this.label;
        }
    }

    ///////////
    //Element ID's & Identifiers
    ///////////

    /**
     * Xpath assigned to the page header
     */
    private static final String APPS_HEADER_XPATH = "//div[contains(@class, 'x-title-text')][text()='Connected Vehicle Applications']";

    /**
     * Xpath prefix to tab on the Apps page
     */
    private static final String APPS_TAB_XPATH_PREFIX = "//span[text()='";

    /**
     * Xpath suffix to tab on the Apps page
     */
    private static final String APPS_TAB_XPATH_SUFFIX = "']/ancestor::a";

    /**
     * Class of selected tab
     */
    private static final String SELECTED_TAB_CLASS = "x-tab-active";

    /**
     * Xpath suffix (to be used with app row) to info icon
     */
    protected static final String INFO_ICON_XPATH_SUFFIX = ".//img";

    /**
     * Xpath suffix (to be used with app row) to get app type displayed
     */
    protected static final String APP_TYPE_XPATH_PREFIX = ".//div[@class='x-grid-cell-inner '][text()='";

    /**
     * Class of disabled button
     */
    protected static final String DISABLED_BTN_CLASS = "x-btn-disabled";

    /**
     * Xpath of check box associated with app in list
     */
    protected static final String CHECK_BOX_XPATH = ".//div[@class='x-grid-row-checker']";

    /**
     * Xpath of row associated with a check box element
     */
    protected static final String CHECK_BOX_ROW_XPATH = ".//div[@class='x-grid-row-checker']/ancestor::table";

    /**
     * Class of a selected row
     */
    protected static final String SELECTED_ROW_CLASS = "x-grid-item-selected";

    ///////////
    //Element Getters
    ///////////

    /**
     * Gets execution details page header
     *
     * @return the header element
     */
    private El getHeader() {
        return el(By.xpath(APPS_HEADER_XPATH));
    }

    /**
     * Gets the tab element for the given app tab
     *
     * @param tab -the app tab to get
     * @return the tab element
     */
    protected El getTab(AppTab tab) {
        return el(By.xpath(APPS_TAB_XPATH_PREFIX + tab.label + APPS_TAB_XPATH_SUFFIX));
    }

    /**
     * Gets the info icon associated with the given app
     *
     * @param appRow -the row in which the app is located
     * @return the info icon element
     */
    protected El getInfoIcon(El appRow) {
        return appRow.el(By.xpath(INFO_ICON_XPATH_SUFFIX));
    }

    /**
     * Gets the app type associated with the given device
     *
     * @param appRow -the row in which the device is located
     * @param expectedType -the device type expected
     * @return the device type cell
     */
    protected El getAppTypeDisplayed(El appRow, DeviceType expectedType) {
        return appRow.el(By.xpath(APP_TYPE_XPATH_PREFIX + expectedType.getUILabel() + "']"));
    }

    ///////////
    //Element Checkers
    ///////////

    /**
     * Checks to see if the Connected Vehicle Applications header is displayed
     *
     * @return true if header is displayed, false if it cannot be found or is
     *         not visible
     */
    public boolean isAppsHeaderDisplayed() {
        return isElementDisplayed(getHeader());
    }

    /**
     * Checks if the expected tab is displayed
     *
     * @param tab -the tab expected
     * @return true if displayed, false otherwise
     */
    public boolean isTabDisplayed(AppTab tab) {
        return isElementDisplayed(getTab(tab));
    }

    /**
     * Checks if the given tab is selected
     *
     * @param tab -the tab to check
     * @return true if selected, false otherwise
     */
    public boolean isTabSelected(AppTab tab) {
        String tabClass = getTab(tab).getAttribute("class");
        if (tabClass.contains(SELECTED_TAB_CLASS)) {
            return true;
        }
        else {
            return false;
        }
    }

    /**
     * Checks if the given button is disabled
     *
     * @param btn -the button to check
     * @return true if disabled, false if enabled
     */
    protected boolean isBtnDisabled(El btn) {
        String btnClass = btn.getAttribute("class");
        return btnClass.contains(DISABLED_BTN_CLASS);
    }

    ////////////
    // Utilities
    ///////////

    /**
     * Checks for display of all tabs (Embedded Applications, JAR Applications,
     * Native Applications, and Remote Applications)
     */
    public void checkAllTabs() {
        Assert.assertTrue("Embedded Applications tab could not be found.", isTabDisplayed(AppTab.EMBEDDED_APPS));
        Assert.assertTrue("JAR Applications tab could not be found.", isTabDisplayed(AppTab.JAR_APPS));
        Assert.assertTrue("Native Applications tab could not be found.", isTabDisplayed(AppTab.NATIVE_APPS));
        Assert.assertTrue("Remote Applications tab could not be found.", isTabDisplayed(AppTab.REMOTE_APPS));
    }

    /**
     * Clicks the JAR Applications tab
     *
     * @return the JAR Applications Partial Page
     */
    public JARApplicationsPartialPage clickJARAppsTab() {
        getTab(AppTab.JAR_APPS).click();
        return getPage(JARApplicationsPartialPage.class);
    }

    /**
     * Clicks the Remote Applications tab
     *
     * @return the Remote Applications Partial Page
     */
    public RemoteApplicationsPartialPage clickRemoteAppsTab() {
        getTab(AppTab.REMOTE_APPS).click();
        return getPage(RemoteApplicationsPartialPage.class);
    }

    /**
     * Clicks Native Applications tab
     *
     * @return the Native Applications Partial Page
     */
    public NativeAppsPartialPage clickNativeAppsTab() {
        getTab(AppTab.NATIVE_APPS).click();
        return getPage(NativeAppsPartialPage.class);
    }

    /**
     * Clicks Embedded Applications tab
     *
     * @return the Embedded Applications Partial Page
     */
    public EmbeddedAppsPartialPage clickEmbeddedAppsTab() {
        getTab(AppTab.EMBEDDED_APPS).click();
        return getPage(EmbeddedAppsPartialPage.class);
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(APPS_HEADER_XPATH));
    }
}
