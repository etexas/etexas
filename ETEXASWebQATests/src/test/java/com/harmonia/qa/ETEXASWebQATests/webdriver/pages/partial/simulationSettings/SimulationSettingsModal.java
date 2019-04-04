package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.parital.environment.ConfigureDetectorsPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.ConfigureCellTowersPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.ConfigureFixedCellularDevicePartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.ConfigureRSEDevicesPartialPage;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the configure detectors form
 *
 * @author llaroussini
 */
public class SimulationSettingsModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver
     */
    public SimulationSettingsModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    //////////////
    //Enumerations
    //////////////

    /**
     * Enumeration of tabs in environment window
     *
     * @author llaroussini
     */
    public enum SimSettingsTabs {
        /**
         * Cell Towers tab
         */
        CELL_TOWERS("Cell Towers"),
        /**
         * Detectors tab
         */
        DETECTORS("Detectors"),
        /**
         * Fixed Cellular Devices tab
         */
        FIXED_CELL("Fixed Cellular Devices"),
        /**
         * RSE Devices tab
         */
        RSE("RSE Devices");

        /**
         * The label of the tab as appears in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        SimSettingsTabs(String label) {
            this.label = label;
        }

        /**
         * Gets the label associated with the tab as it is displayed in the Web
         * UI
         *
         * @return The label of the column header
         */
        public String getLabel() {
            return this.label;
        }
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the simulation settings header
     */
    private static final String SIM_SETTINGS_HEADER_TEXT = "Simulation Settings";

    /**
     * Text displayed in the simulation settings help header
     */
    private static final String SIM_SETTINGS_HELP_HEADER_TEXT = "Simulation Settings Help";

    /**
     * Text displayed in the simulation settings help modal
     */
    private static final String SIM_SETTINGS_HELP_CONTENT_TEXT = "Simulations can be configured with cell towers, detectors, fixed cellular devices, and Road Side Equipment (RSE) devices. The capabilities of each are explained below: ";

    /**
     * Xpath prefix to tab
     */
    private static final String TAB_XPATH_PREFIX = "//div[contains(@id, 'simulation')][contains(@id, 'Settings')]//span[contains(@class, 'x-tab-inner-default')][text()='";

    /**
     * Xpath to the Simulation Settings close button
     */
    private static final String SIM_SETTINGS_CLOSE_BTN_XPATH = "//div[contains(@class, 'x-title-item')][@data-ref='textEl'][contains(text(), 'Simulation Settings')]/ancestor::div[contains(@class, 'x-border-box')]//span[@data-ref='btnInnerEl'][text()='Close']";

    /**
     * Xpath to the Close icon in the Simulation Settings modal
     */
    private static final String CLOSE_ICON_XPATH = "//div[contains(@id, 'simulation')][contains(@id, 'Settings')]//div[@data-qtip='Close dialog']";

    /**
     * Xpath prefix to a specific cell (assumes this will be used in conjunction
     * with a row)
     */
    protected static final String CELL_IN_DEVICE_ROW_XPATH_PREFIX = ".//td[contains(@class, '";

    ///////////
    // Getters
    ///////////

    /**
     * gets the given simulation settings devices tab
     *
     * @param tab -the tab to get
     * @return the tab element
     */
    private El getSimSettingsTab(SimSettingsTabs tab) {
        return el(By.xpath(TAB_XPATH_PREFIX + tab.label + "']"));
    }

    /**
     * The xpath to get from the sim settings header to the help icon
     *
     * @return the help icon
     */
    private El getSimSettingsHelpIcon() {
        return getHelpIcon(SIM_SETTINGS_HEADER_TEXT);
    }

    /**
     * Gets the close button
     *
     * @return the close button
     */
    protected El getCloseBtn() {
        return el(By.xpath(SIM_SETTINGS_CLOSE_BTN_XPATH));
    }

    /**
     * Gets the Close icon in the Simulation Settings modal
     *
     * @return the icon element
     */
    private El getCloseIcon() {
        return el(By.xpath(CLOSE_ICON_XPATH));
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the close button is displayed
     *
     * @return true if the close button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCloseBtnDisplayed() {
        return isElementDisplayed(getCloseBtn());
    }

    /**
     * Checks to see if the environment tab expected is displayed
     *
     * @param tab -the tab expected
     * @return true if displayed, false otherwise
     */
    public boolean isEnvironmentTabDisplayed(SimSettingsTabs tab) {
        return isElementDisplayed(getSimSettingsTab(tab));
    }

    /**
     * Checks to see if the simulation settings header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isSimSettingsHeaderDisplayed() {
        return isHeaderDisplayed(SIM_SETTINGS_HEADER_TEXT);
    }

    /**
     * Checks to see if the simulation settings help header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isSimSettingsHelpHeaderDisplayed() {
        return isHeaderDisplayed(SIM_SETTINGS_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the help content is displayed
     *
     * @return true if the help content is displayed, false otherwise
     */
    public boolean isSimSettingsHelpContentDisplayed() {
        return isContentDisplayed(SIM_SETTINGS_HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false otherwise
     */
    public boolean isSimSettingsHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(SIM_SETTINGS_HELP_HEADER_TEXT);
    }

    /**
     * Checks if the Close icon is displayed in the Simulation Settings modal
     *
     * @return true if displayed, false otherwise
     */
    public boolean isCloseIconDisplayed() {
        return isElementDisplayed(getCloseIcon());
    }

    ///////////
    // Interaction
    ///////////

    /**
     * Clicks the Detectors tab
     *
     * @return the Configure Detectors partial page
     */
    public ConfigureDetectorsPartialPage clickDetectorsTab() {
        getSimSettingsTab(SimSettingsTabs.DETECTORS).click();
        return getPage(ConfigureDetectorsPartialPage.class);
    }

    /**
     * Clicks the Cell Towers tab
     *
     * @return the Configure Cell Towers partial page
     */
    public ConfigureCellTowersPartialPage clickCellTowersTab() {
        getSimSettingsTab(SimSettingsTabs.CELL_TOWERS).click();
        return getPage(ConfigureCellTowersPartialPage.class);
    }

    /**
     * Clicks the Fixed Cellular Devices tab
     *
     * @return the Configure Fixed Cellular Devices partial page
     */
    public ConfigureFixedCellularDevicePartialPage clickFixedCellularDevicesTab() {
        getSimSettingsTab(SimSettingsTabs.FIXED_CELL).click();
        return getPage(ConfigureFixedCellularDevicePartialPage.class);
    }

    /**
     * Clicks the RSE Devices tab
     *
     * @return the Configure RSE Devices partial page
     */
    public ConfigureRSEDevicesPartialPage clickRSEDevicesTab() {
        getSimSettingsTab(SimSettingsTabs.RSE).click();
        return getPage(ConfigureRSEDevicesPartialPage.class);
    }

    /**
     * Clicks the Close button
     *
     * @return the newly loaded Simulations page
     */
    public SimulationsPage clickClose() {
        scrollTo(false);
        getCloseBtn().click();
        waitForElementToBeInvisible(By.xpath(SIM_SETTINGS_HEADER_TEXT));
        return getPage(SimulationsPage.class);
    }

    /**
     * Click the Simulation Settings Help icon
     */
    public void clickSimSettingsHelpIcon() {
        getSimSettingsHelpIcon().click();
    }

    /**
     * Clicks the Simulation Settings Close icon
     *
     * @return the newly loaded Simulations Page
     */
    public SimulationsPage clickCloseIcon() {
        getCloseIcon().click();
        return getPage(SimulationsPage.class);
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickSimSettingsHelpOKBtn() {
        clickHelpOKBtn(SIM_SETTINGS_HELP_HEADER_TEXT);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Verifies the Simulation Settings header is displayed
     */
    public void checkHeader() {
        Assert.assertTrue("Simulation Settings header not displayed as expected.", isSimSettingsHeaderDisplayed());
    }

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkHeaderIcons() {
        Assert.assertTrue("Help icon not displayed as expected in the Simulation Settings modal.", isHelpIconDisplayed(SIM_SETTINGS_HEADER_TEXT));
        Assert.assertTrue("Close icon not displayed as expected in the Simulation Settings modal.", isCloseIconDisplayed());
    }

    /**
     * Checks that help modal is displayed -- checks both the header and content
     * text
     */
    public void checkHelpModal() {
        Assert.assertTrue("Simulation setting help header not displayed as expected.", isSimSettingsHelpHeaderDisplayed());
        Assert.assertTrue("Simulation Settings help content not displayed as expected.", isSimSettingsHelpContentDisplayed());
    }

    /**
     * Checks for the presence of the Cell Towers, Detectors, Fixed Cellular
     * Devices, and RSE Devices tabs
     */
    public void checkTabs() {
        Assert.assertTrue("The Cell Towers tab was not found.", isEnvironmentTabDisplayed(SimSettingsTabs.CELL_TOWERS));
        Assert.assertTrue("The Detectors tab was not found.", isEnvironmentTabDisplayed(SimSettingsTabs.DETECTORS));
        Assert.assertTrue("The Fixed Cellular Devices tab was not found.", isEnvironmentTabDisplayed(SimSettingsTabs.FIXED_CELL));
        Assert.assertTrue("The RSE Devices tab was not found.", isEnvironmentTabDisplayed(SimSettingsTabs.RSE));
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(TAB_XPATH_PREFIX + SimSettingsTabs.CELL_TOWERS.label + "']"), 10);
    }
}
