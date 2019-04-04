package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.ConfigureCellularDevicePartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.ConfigureOBUDeviceProfilesPartialPage;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the configure devices form
 *
 * @author llaroussini
 * @author cbulloss
 */
public class CompositeSettingsModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public CompositeSettingsModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    //////////////
    //Enumerations
    //////////////

    /**
     * Enumeration of tabs in composite settings window
     *
     * @author llaroussini
     */
    public enum CompositeSettingsTab {
        /**
         * Cellular Device Profiles tab
         */
        CELLULAR("Cellular Device Profiles"),
        /**
         * OBU Device Profiles tab
         */
        OBU("OBU Device Profiles"),
        /**
         * Cellular tab
         */
        OPTIONS("Options");

        /**
         * The label of the column header as appears in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        CompositeSettingsTab(String label) {
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

    /**
     * Enumeration of tabs in devices window
     *
     * @author llaroussini
     */
    public enum ConfigureBtn {
        /**
         * Create button
         */
        CREATE("Create"),
        /**
         * Edit button
         */
        EDIT("Edit"),
        /**
         * Delete button
         */
        DELETE("Delete"),
        /**
         * Applications button
         */
        APPLICATIONS("Applications");

        /**
         * The label of the column header as appears in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        ConfigureBtn(String label) {
            this.label = label;
        }

        /**
         * Gets the label associated with the button as it is displayed in the
         * Web UI
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
     * Text displayed in the Composite Settings header
     */
    public static final String COMPOSITE_SETTINGS_HEADER_TEXT = "Composite Settings";

    /**
     * Text displayed in the Composite Settings help header
     */
    private static final String COMPOSITE_SETTINGS_HELP_HEADER_TEXT = "Composite Settings Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Composites can be configured with cellular device profiles, On Board Unit (OBU) device profiles, and options relating to the coordinates and communications model for the composite. The capabilities of each are explained below:";

    /**
     * Name of the Close button
     */
    private static final String CLOSE_BTN_NAME = "Close";

    /**
     * Xpath prefix to a Composite Settings tab
     */
    protected static final String COMPOSITE_SETTINGS_TAB_XPATH_PREFIX = "//div[contains(@id, 'ETexas-composite-view-Settings-window')]//span[text()='";

    /**
     * Xpath suffix to a Composite Settings tab
     */
    protected static final String COMPOSITE_SETTINGS_TAB_XPATH_SUFFIX = "']/ancestor::a";

    ///////////
    //Getters
    ///////////

    /**
     * Gets the close button
     *
     * @return the close button
     */
    private El getCloseBtn() {
        return getFormBtn(COMPOSITE_SETTINGS_HEADER_TEXT, CLOSE_BTN_NAME);
    }

    /**
     * Gets the given Composite Settings tab
     *
     * @param tab -the tab to get
     * @return the tab element
     */
    private El getCompositeSettingsTab(CompositeSettingsTab tab) {
        return el(By.xpath(COMPOSITE_SETTINGS_TAB_XPATH_PREFIX + tab.getLabel() + COMPOSITE_SETTINGS_TAB_XPATH_SUFFIX));
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the Composite Settings header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isCompositeSettingsHeaderDisplayed() {
        return isHeaderDisplayed(COMPOSITE_SETTINGS_HEADER_TEXT);
    }

    /**
     * Checks to see if the Composite Settings help header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isCompositeSettingsHelpHeaderDisplayed() {
        return isHeaderDisplayed(COMPOSITE_SETTINGS_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false otherwise
     */
    public boolean isCompositeSettingsHelpIconDisplayed() {
        return isHelpIconDisplayed(COMPOSITE_SETTINGS_HEADER_TEXT);
    }

    /**
     * Checks to see if the close icon is displayed
     *
     * @return true if the close icon is displayed, false otherwise
     */
    public boolean isCompositeSettingsCloseIconDisplayed() {
        return isCloseIconDisplayed(COMPOSITE_SETTINGS_HEADER_TEXT);
    }

    /**
     * Checks to see if the close button is displayed
     *
     * @return true if the close button is displayed, false otherwise
     */
    public boolean isCloseBtnDisplayed() {
        return isBtnDisplayed(COMPOSITE_SETTINGS_HEADER_TEXT, CLOSE_BTN_NAME);
    }

    /**
     * Checks to see if the help content is displayed
     *
     * @return true if the help content is displayed, false otherwise
     */
    public boolean isHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in help modal
     *
     * @return true if the button is displayed, false otherwise
     */
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(COMPOSITE_SETTINGS_HELP_HEADER_TEXT);
    }

    /**
     * Checks if given tab is displayed in the Composite Settings modal
     *
     * @param tab -the tab expected
     * @return true if the tab is displayed, false otherwise
     */
    public boolean isCompositeSettingsTabDisplayed(CompositeSettingsTab tab) {
        return isElementDisplayed(getCompositeSettingsTab(tab));
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Click the Composite Settings Help icon
     */
    public void clickCompositeSettingsHelp() {
        clickHelpIcon(COMPOSITE_SETTINGS_HEADER_TEXT);
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(COMPOSITE_SETTINGS_HELP_HEADER_TEXT);
    }

    /**
     * Click the Close button
     *
     * @return the newly loaded Simulation page
     */
    public SimulationsPage clickCloseBtn() {
        getCloseBtn().click();
        return getPage(SimulationsPage.class);
    }

    /**
     * Clicks the Close icon
     *
     * @return the newly loaded Simulations page
     */
    public SimulationsPage clickCloseIcon() {
        getCloseIcon(COMPOSITE_SETTINGS_HEADER_TEXT).click();
        return getPage(SimulationsPage.class);
    }

    /**
     * Clicks the Cellular tab
     *
     * @return Configure Cellular Device partial page
     */
    public ConfigureCellularDevicePartialPage clickCellularTab() {
        getCompositeSettingsTab(CompositeSettingsTab.CELLULAR).click();
        return getPage(ConfigureCellularDevicePartialPage.class);
    }

    /**
     * Clicks the OBU tab
     *
     * @return Configure OBU Device partial page
     */
    public ConfigureOBUDeviceProfilesPartialPage clickOBUTab() {
        getCompositeSettingsTab(CompositeSettingsTab.OBU).click();
        return getPage(ConfigureOBUDeviceProfilesPartialPage.class);
    }

    /**
     * Clicks the Options tab
     *
     * @return Composite Options partial page
     */
    public CompositeOptionsPartialPage clickOptionsTab() {
        getCompositeSettingsTab(CompositeSettingsTab.OPTIONS).click();
        return getPage(CompositeOptionsPartialPage.class);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkHeaderIcons() {
        checkHeaderIcons(COMPOSITE_SETTINGS_HEADER_TEXT);
    }

    /**
     * Checks for presence of Composite Settings Help header and content
     */
    public void checkHelpModal(){
        Assert.assertTrue("Composite Settings Help header is not displayed as expected.", isCompositeSettingsHelpHeaderDisplayed());
        Assert.assertTrue("Composite Settings Help content is not displayed as expected.", isHelpContentDisplayed());
    }

    /**
     * Checks for the presence of the Cellular Device Profiles, OBU Device
     * Profiles, and Options tabs
     */
    public void checkTabs() {
        Assert.assertTrue("The Cellular Device Profiles tab was not found.", isCompositeSettingsTabDisplayed(CompositeSettingsTab.CELLULAR));
        Assert.assertTrue("The OBU Device Profiles tab was not found.", isCompositeSettingsTabDisplayed(CompositeSettingsTab.OBU));
        Assert.assertTrue("The Options tab was not found.", isCompositeSettingsTabDisplayed(CompositeSettingsTab.OPTIONS));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(TITLE_XPATH_PREFIX + COMPOSITE_SETTINGS_HEADER_TEXT + "']"), 20);
    }
}
