package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.OBUDevice;
import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the add OBU rule form
 *
 * @author llaroussini
 */
public class CreateOBUDeviceProfileModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public CreateOBUDeviceProfileModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    //////////////////
    // Enumerations
    /////////////////

    /**
     * Enumeration of Create OBU Device Profile text boxes
     *
     * @author llaroussini
     */
    public enum CreateOBUDeviceProfileTextBox {
        /**
         * Name text box
         */
        NAME("Name:", "deviceName"),
        /**
         * Percentage text box
         */
        PERCENTAGE("Percentage:", "percentage");

        /**
         * The label of the text box as appears in the application
         */
        private String label;

        /**
         * The name attribute of the text box as appears in the application
         */
        private String name;

        /**
         * Default constructor
         *
         * @param label The string to set as the label
         * @param name The string to set at the name attribute
         */
        CreateOBUDeviceProfileTextBox(String label, String name) {
            this.label = label;
            this.name = name;
        }

        /**
         * Gets the label associated with the text box as it is displayed in the
         * Web UI
         *
         * @return The label of the text box
         */
        public String getLabel() {
            return this.label;
        }

        /**
         * Gets the name attribute associated with the text box as it is
         * displayed in the Web UI
         *
         * @return The name attribute of the text box
         */
        public String getName() {
            return this.name;
        }
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the create OBU device profile header
     */
    private static final String CREATE_OBU_HEADER_TEXT = "Create OBU Device Profile";

    /**
     * Xpath to the create OBU device profile header
     */
    private static final String CREATE_OBU_HEADER_XPATH = TITLE_XPATH_PREFIX + CREATE_OBU_HEADER_TEXT + "']";

    /**
     * Text displayed in the create OBU device profile help header
     */
    private static final String CREATE_OBU_HELP_HEADER_TEXT = "Create OBU Device Profile Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Create a new On Board Unit (OBU) device profile with the specified name and percentage of affected vehicles. The device profile name must be unique in the composite and the total percentage for all OBU device profiles cannot exceed 100 percent.";

    /**
     * The xpath prefix to all text boxes in the Create OBU Device Profiles
     * modal
     */
    private static final String OBU_TEXT_BOX_XPATH_PREFIX = "//input[contains(@id, 'obu')][@name='";

    /**
     * Xpath prefix to buttons in the Create OBU Device Profiles modal
     */
    private static final String CREATE_OBU_BTN_XPATH_PREFIX = "//a[contains(@id, 'obu-Create')]//span[contains(@id, 'button')][text()='";

    /**
     * Error text displayed when invalid device name used
     */
    private static final String INVALID_DEVICE_NAME_ERROR_TEXT = "Device profile names may contain only letters, digits, hyphens, underscores, and nonconsecutive spaces.";

    /**
     * Error text displayed when value is used in Percentage text box outside
     * the acceptable range
     */
    private static final String INVALID_PERCENTAGE_RANGE_ERROR_TEXT = "OBU device profile percentages must be in the range of 1 to 100 percent.";

    /**
     * Error text displayed when total percentage total for all OBU devices
     * exceeds 100%
     */
    private static final String MAX_TOTAL_PERCENTAGE_EXCEEDED_ERROR_TEXT = "The total for all OBU device profile percentages cannot exceed 100 percent.";

    ///////////
    //Getters
    ///////////

    /**
     * Gets the given button in the create OBU device profile modal
     *
     * @param btn - the button to get
     * @return the button element
     */
    private El getBtn(BtnNames btn) {
        return el(By.xpath(CREATE_OBU_BTN_XPATH_PREFIX + btn.getLabel() + "']"));
    }

    /**
     * Gets the given text box
     *
     * @param textBox -the text box to get
     * @return the text box element
     */
    private El getTextBox(CreateOBUDeviceProfileTextBox textBox) {
        return el(By.xpath(OBU_TEXT_BOX_XPATH_PREFIX + textBox.getName() + "']"));
    }

    /**
     * Gets the value displayed in the given text box
     *
     * @param textBox -the specific text box from which to get the value
     * @return the displayed value in the given text box
     */
    private String getTextBoxValue(CreateOBUDeviceProfileTextBox textBox) {
        return getTextBox(textBox).getAttribute("value");
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the create OBU device profile header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCreateOBUDeviceProfileHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_OBU_HEADER_TEXT);
    }

    /**
     * Checks to see if the create button is displayed
     *
     * @return true if the create button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCreateBtnDisplayed() {
        return isElementDisplayed(getBtn(BtnNames.CREATE));
    }

    /**
     * Checks to see if the reset button is displayed
     *
     * @return true if the reset button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isResetBtnDisplayed() {
        return isElementDisplayed(getBtn(BtnNames.RESET));
    }

    /**
     * Checks to see if the cancel button is displayed
     *
     * @return true if the cancel button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCancelBtnDisplayed() {
        return isElementDisplayed(getBtn(BtnNames.CANCEL));
    }

    /**
     * Checks to see if the given text box is displayed
     *
     * @param textBox -the text box expected
     * @return true if the text box is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isTextBoxDisplayed(CreateOBUDeviceProfileTextBox textBox) {
        return isElementDisplayed(getTextBox(textBox));
    }

    /**
     * Checks to see if the Create OBU Device Profile help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCreateOBUDeviceProfileHelpHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_OBU_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the Create OBU Device Profile help content is displayed
     *
     * @return true if the content text is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCreateOBUDeviceProfileHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in the help window
     *
     * @return true if the ok button is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(CREATE_OBU_HELP_HEADER_TEXT);
    }

    /**
     * Checks if field required error is displayed with OBU Name text box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isOBUNameFieldRequiredErrorDisplayed(){
        return isFieldRequiredErrorDisplayed(CreateOBUDeviceProfileTextBox.NAME.label);
    }

    /**
     * Checks if leading/trailing whitespace error is displayed with OBU Name
     * text box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isLeadingTrailingWhitespaceNameErrorDisplayed() {
        return isWhitespaceErrorDisplayed(CreateOBUDeviceProfileTextBox.NAME.label);
    }

    /**
     * Checks if invalid name error is displayed with OBU Name text box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidOBUNameErrorDisplayed() {
        return isErrorToolTipDisplayed(INVALID_DEVICE_NAME_ERROR_TEXT);
    }

    /**
     * Checks if duplicate OBU device error is displayed
     *
     * @param obu -the duplicated obu device
     * @return true if displayed, false otherwise
     */
    public boolean isDuplicateOBUNameErrorDisplayed(OBUDevice obu) {
        String name = obu.getName();
        return isDuplicateDeviceNameErrorDisplayed(name);
    }

    /**
     * Checks if field required error is displayed with OBU Percentage text box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isOBUPercentageFieldRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(CreateOBUDeviceProfileTextBox.PERCENTAGE.label);
    }

    /**
     * Checks if leading/trailing whitespace error is displayed with OBU
     * Percentage text box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isLeadingTrailingWhitespacePercentageErrorDisplayed() {
        return isWhitespaceErrorDisplayed(CreateOBUDeviceProfileTextBox.PERCENTAGE.label);
    }

    /**
     * Checks to see if Invalid Percentage Range is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidPercentageRangeErrorDisplayed() {
        return isElementDisplayed(getSpecificErrorToolTip(INVALID_PERCENTAGE_RANGE_ERROR_TEXT));
    }

    /**
     * Checks to see if Invalid Non-Numeric Percentage error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidNonNumericRulePercentageErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(CreateOBUDeviceProfileTextBox.PERCENTAGE.label);
    }

    /**
     * Checks if Max Total Percentage Exceeded error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isMaxTotalPercentageExceededErrorDisplayed() {
        return isErrorToolTipDisplayed(MAX_TOTAL_PERCENTAGE_EXCEEDED_ERROR_TEXT);
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Sets given OBU device name in name text box
     *
     * @param name -the name to enter
     */
    public void setName(String name) {
        getTextBox(CreateOBUDeviceProfileTextBox.NAME).setText(name);
    }

    /**
     * Sets name of given OBU device in name text box
     *
     * @param device -the device to set
     */
    public void setName(OBUDevice device) {
        String name = device.getName();
        setName(name);
    }

    /**
     * Sets given OBU device percentage in percentage text box
     *
     * @param percent -string value of the percent to set
     */
    public void setPercentage(String percent) {
        getTextBox(CreateOBUDeviceProfileTextBox.PERCENTAGE).setText(percent);
    }

    /**
     * Sets percentage of given OBU device in percentage text box
     *
     * @param device -the device to set
     */
    public void setPercentage(OBUDevice device) {
        String percent = device.getOBUPercent();
        setPercentage(percent);
    }

    /**
     * Click the Create button. Will load the Configure OBU Device Profiles
     * Partial Page
     *
     * @param success -true if success is expected, false if error is expected
     * @return the newly loaded Configure OBU Device Profiles Partial Page or
     *         null if error is expected
     */
    public ConfigureOBUDeviceProfilesPartialPage clickCreate(boolean success) {
        getBtn(BtnNames.CREATE).click();
        if (success) {
            waitForElementToBeInvisible(By.xpath(CREATE_OBU_HEADER_XPATH));
            return getPage(ConfigureOBUDeviceProfilesPartialPage.class);
        }
        else {
            return null;
        }
    }

    /**
     * Click the Reset button
     */
    public void clickReset() {
        getBtn(BtnNames.RESET).click();
    }

    /**
     * Click the Cancel button
     *
     * @return the newly loaded Configure OBU Devices Profile Partial Page
     */
    public ConfigureOBUDeviceProfilesPartialPage clickCancel() {
        getBtn(BtnNames.CANCEL).click();
        waitForElementToBeInvisible(By.xpath(CREATE_OBU_HEADER_XPATH));
        return getPage(ConfigureOBUDeviceProfilesPartialPage.class);
    }

    /**
     * Clicks the Create OBU Device Profile help icon
     */
    public void clickHelpIcon() {
        getHelpIcon(CREATE_OBU_HEADER_TEXT).click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(CREATE_OBU_HELP_HEADER_TEXT);
    }

    /**
     * Clicks the Close icon in the Create OBU Device Profile header
     */
    public void clickCloseIcon() {
        clickCloseIcon(CREATE_OBU_HEADER_TEXT);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkHeaderIcons() {
        checkHeaderIcons(CREATE_OBU_HEADER_TEXT);
    }

    /**
     * Checks that help modal is displayed -- checks both the header and content
     * text
     */
    public void checkHelpModal() {
        Assert.assertTrue("Create OBU Device Profile Help header not displayed as expected.", isCreateOBUDeviceProfileHelpHeaderDisplayed());
        Assert.assertTrue("Create OBU Device Profile Help content not displayed as expected.", isCreateOBUDeviceProfileHelpContentDisplayed());
    }

    /**
     * Checks for the presence of the Create, Reset, and Cancel buttons
     */
    public void checkBtns() {
        Assert.assertTrue("The Create button is not displayed.", isCreateBtnDisplayed());
        Assert.assertTrue("The Reset button is not displayed.", isResetBtnDisplayed());
        Assert.assertTrue("The Cancel button is not displayed.", isCancelBtnDisplayed());
    }

    /**
     * Checks for the presence of the Name and Percentage text boxes
     */
    public void checkFields() {
        Assert.assertTrue("The Name text box could not be found.", isTextBoxDisplayed(CreateOBUDeviceProfileTextBox.NAME));
        Assert.assertTrue("The Percentage text box could not be found.", isTextBoxDisplayed(CreateOBUDeviceProfileTextBox.PERCENTAGE));
    }

    /**
     * Sets the given name and percentage in the Name and Percentage text boxes
     *
     * @param name -the name to set
     * @param percent -the percentage to set
     */
    public void setAllFields(String name, String percent) {
        setName(name);
        setPercentage(percent);
    }

    /**
     * Sets the name and percentage associated with the given device in the Name
     * and Percentage text boxes
     *
     * @param device -the device to set
     */
    public void setAllFields(OBUDevice device) {
        setName(device);
        setPercentage(device);
    }

    /**
     * Checks Name and Percentage fields and verifies values displayed match
     * values expected
     *
     * @param device -the device expected
     */
    public void checkFieldValuesDisplayed(OBUDevice device) {
        String deviceName = device.getName();
        String rulePercentage = device.getOBUPercent();
        checkFieldValuesDisplayed(deviceName, rulePercentage);
    }

    /**
     * Checks Name and Percentage fields and verifies values displayed match
     * values expected
     *
     * @param name -the device name expected
     * @param percentage -the rule percentage expected
     */
    public void checkFieldValuesDisplayed(String name, String percentage) {
        Assert.assertEquals("The displayed name does not match the expected name.", name, getTextBoxValue(CreateOBUDeviceProfileTextBox.NAME));
        Assert.assertEquals("The displayed percentage does not match the expected percentage", percentage, getTextBoxValue(CreateOBUDeviceProfileTextBox.PERCENTAGE));
    }

    //////////
    // Waits
    //////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(CREATE_OBU_HEADER_XPATH));
    }
}
