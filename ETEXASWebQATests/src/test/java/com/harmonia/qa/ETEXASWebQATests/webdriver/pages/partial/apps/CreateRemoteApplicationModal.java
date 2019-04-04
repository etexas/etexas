package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.UserRemoteApp;
import com.harmonia.qa.ETEXASWebQATests.enums.DeviceType;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.AppsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the Create Remote Application Modal
 *
 * @author llaroussini
 */
public class CreateRemoteApplicationModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public CreateRemoteApplicationModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    /////////////////
    // Enumerations
    ////////////////

    /**
     * Remote App Field names
     *
     * @author llaroussini
     * @author rsmith
     */
    public enum RemoteAppField {
        /**
         * Remote App Name field name
         */
        NAME("applicationName", "Name:"),
        /**
         * Device Type field name
         */
        DEVICE_TYPE("deviceType", "Device Type");

        /**
         * The name attribute of the fields in the Create remote App window
         */
        private String name;

        /**
         * The label of the fields in the Create remote App window
         */
        private String label;

        /**
         * Default constructor
         *
         * @param name The string to set as the name attribute
         * @param label The string to set as the field label
         */
        RemoteAppField(String name, String label) {
            this.name = name;
            this.label = label;
        }

        /**
         * Gets the name attribute of the field in the Create remote App window
         *
         * @return The name attribute of the field
         */
        public String getName() {
            return this.name;
        }

        /**
         * Gets the label of the field in the Create App window
         *
         * @return The label of the field
         */
        public String getLabel() {
            return this.label;
        }
    }

    /////////////////
    // Idenitfiers
    ////////////////

    /**
     * Text displayed in the Create Remote Application header
     */
    private static final String CREATE_REMOTE_APP_HEADER_TEXT = "Create Remote Application";

    /**
     * Text displayed in the Create Remote Application Help header
     */
    private static final String CREATE_REMOTE_APP_HELP_HEADER_TEXT = "Create Remote Application Help";

    /**
     * Text displayed as the content of Create Remote Application Help
     */
    private static final String CREATE_REMOTE_APP_HELP_CONTENT_TEXT = "Create a remote application with the specified name and device type. The application name must be unique across all applications and, once created, the device type cannot be changed.";

    /**
     * Xpath prefix for all buttons in Create Remote Application modal
     */
    private static final String BTN_XPATH_PREFIX = "//div[contains(@id, 'create-remote-application-profile-form')]//div[contains(@id, 'toolbar')]//span[text()='";

    /**
     * Xpath suffix for all buttons in Create Remote Application modal
     */
    private static final String BTN_XPATH_SUFFIX = "']/ancestor::a";

    /**
     * Xpath to Create button
     */
    private static final String CREATE_BTN_XPATH = BTN_XPATH_PREFIX + "Create" + BTN_XPATH_SUFFIX;

    /**
     * Xpath to Edit button
     */
    private static final String RESET_BTN_XPATH = BTN_XPATH_PREFIX + "Reset" + BTN_XPATH_SUFFIX;

    /**
     * Xpath to Delete button
     */
    private static final String CANCEL_BTN_XPATH = BTN_XPATH_PREFIX + "Cancel" + BTN_XPATH_SUFFIX;

    /**
     * Xpath prefix to fields in the Create Remote Application modal
     */
    private static final String CREATE_REMOTE_APP_FIELD_XPATH_PREFIX = "//div[contains(@id, 'create-remote-application-profile-form')]//input[@name='";

    /**
     * The xpath for the Name text box
     */
    private static final String NAME_TEXT_BOX_XPATH = CREATE_REMOTE_APP_FIELD_XPATH_PREFIX + "applicationProfileName']";

    /**
     * The xpath for the Device Type dropdown
     */
    private static final String DEVICE_TYPE_DROPDOWN_XPATH = CREATE_REMOTE_APP_FIELD_XPATH_PREFIX + "deviceType']";

    /**
     * The xpath prefix of the Device Type dropdown
     */
    private static final String DEVICE_TYPE_DROPDOWN_OPTION_XPATH_PREFIX = "//li[text()='";

    /**
     * The xpath of the Device Type dropdown selector
     */
    private static final String DEVICE_TYPE_DROPDOWN_SELECTOR_XPATH = DEVICE_TYPE_DROPDOWN_XPATH + "/ancestor::div[3]//div[contains(@class,'x-form-arrow-trigger-default')]";

    /**
     * By locator suitable for locating the Alert displayed when uploading a
     * remote app
     */
    private static final By UPLOADING_APP_ALERT_LOCATOR = By.xpath(".//div[contains(@id,'messagebox')][contains(text(),'Uploading Remote App')]");

    /**
     * Xpath of the selected option in drop down
     */
    private static final String SELECTED_OPTION_XPATH_PREFIX = ".//li[contains(@class, 'x-boundlist-selected')][text()='";

    /**
     * Error text displayed when invalid app name is used
     */
    private static final String INVALID_APP_NAME_ERROR_TEXT = "Application names may contain only letters, digits, hyphens, underscores, and nonconsecutive spaces.";

    /**
     * Error text displayed when a duplicate app name is used.
     */
    private static final String UNIQUE_APP_NAME_ERROR_TEXT_PREFIX = "An application with the name \"";

    /**
     * Error text displayed when a duplicate app name is used suffix
     */
    private static final String UNIQUE_APP_NAME_ERROR_TEXT_SUFFIX = "\" already exists.";

    /**
     * Error text displayed when an application name is submitted blank.
     */
    private static final String INVALID_APP_NAME_REQUIRED_ERROR_TEXT = "A valid application name is required.";

    ///////////
    // Getters
    //////////

    /**
     * Gets the Create button
     *
     * @return the Create button
     */
    private El getCreateBtn() {
        return el(By.xpath(CREATE_BTN_XPATH));
    }

    /**
     * Gets the Reset button from this form
     *
     * @return the Reset button
     */
    private El getResetBtn() {
        return el(By.xpath(RESET_BTN_XPATH));
    }

    /**
     * Gets the Cancel button from this form
     *
     * @return the Cancel button
     */
    private El getCancelBtn() {
        return el(By.xpath(CANCEL_BTN_XPATH));
    }

    /**
     * Gets option from Device Type drop down list
     *
     * @param deviceType -the device type expected
     * @return the option element
     */
    private El getDeviceTypeDropdownOption(String deviceType) {
        return el(By.xpath(DEVICE_TYPE_DROPDOWN_OPTION_XPATH_PREFIX + deviceType + "']"));
    }

    /**
     * Gets option from Device Type drop down list
     *
     * @param deviceType -the device type expected
     * @return the option element
     */
    private El getDeviceTypeDropdownOption(DeviceType deviceType) {
        String type = deviceType.getUILabel();
        return getDeviceTypeDropdownOption(type);
    }

    /**
     * Gets the Device Type drop down selector
     *
     * @return the Device Type drop down selector
     */
    private El getDeviceTypeSelector() {
        return el(By.xpath(DEVICE_TYPE_DROPDOWN_SELECTOR_XPATH));
    }

    /**
     * Gets the value in the Remote App Name text box
     *
     * @return the displayed Remote App Name value
     */
    public String getDisplayedAppName() {
        return getNameTextBox().getAttribute("value");
    }

    /**
     * Gets the selected option within drop down
     *
     * @param option -the option selected
     * @return the selected option element
     */
    private El getSelectedOption(String option) {
        return el(By.xpath(SELECTED_OPTION_XPATH_PREFIX + option + "']"));
    }

    /**
     * Gets the Device Type menu
     *
     * @return the Device Type menu
     */
    private El getDeviceTypeDropdown() {
        return el(By.xpath(DEVICE_TYPE_DROPDOWN_XPATH));
    }

    /**
     * Gets the Name text box
     *
     * @return the Name text box
     */
    private El getNameTextBox() {
        return el(By.xpath(NAME_TEXT_BOX_XPATH));
    }

    /**
     * Gets the value in the Name text box
     *
     * @return the displayed Name value
     */
    public String getDisplayedName() {
        return getNameTextBox().getAttribute("value");
    }

    ///////////
    // Checkers
    ///////////

    /**
     * Checks to see if the Create button is displayed
     *
     * @return true if the Create button is displayed, false otherwise
     */
    public boolean isCreateBtnDisplayed() {
        return isElementDisplayed(getCreateBtn());
    }

    /**
     * Checks to see if the Reset button is displayed on the form
     *
     * @return true if the Reset button is displayed, false otherwise
     */
    public boolean isResetBtnDisplayed() {
        return isElementDisplayed(getResetBtn());
    }

    /**
     * Checks to see if the Cancel button is displayed on the form
     *
     * @return true if the Cancel button is displayed, false otherwise
     */
    public boolean isCancelBtnDisplayed() {
        return isElementDisplayed(getCancelBtn());
    }

    /**
     * Checks to see if the Create Remote Application header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_REMOTE_APP_HEADER_TEXT);
    }

    /**
     * Checks to see if the Create Remote Application Help header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isHelpHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_REMOTE_APP_HELP_HEADER_TEXT);

    }

    /**
     * Checks to see if the Create Remote Application Help content is displayed
     *
     * @return true if the content text is displayed, false otherwise
     */
    public boolean isHelpContentDisplayed() {
        return isContentDisplayed(CREATE_REMOTE_APP_HELP_CONTENT_TEXT);

    }

    /**
     * Checks to see if the Name text box is displayed
     *
     * @return true if the text box is displayed, false otherwise
     */
    public boolean isNameTextboxDisplayed() {
        return isElementDisplayed(getNameTextBox());
    }

    /**
     * Checks to see if the Device Type dropdownenu is displayed
     *
     * @return true if the dropdown is displayed, false otherwise
     */
    public boolean isDeviceTypeDropdownDisplayed() {
        return isElementDisplayed(getDeviceTypeDropdown());
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false otherwise
     */
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(CREATE_REMOTE_APP_HELP_HEADER_TEXT);
    }

    /**
     * Checks if given device type is selected -- method clicks device type
     * dropdown, checks for presence of selected type, then closes dropdown (or
     * returns null and closes dropdown)
     *
     * @param type -the device type expected to be selected
     * @return true if selected; false otherwise
     */
    public boolean isDeviceTypeSelected(String type) {
        clickDeviceTypeDropdown();
        try {
            El selectedOption = getSelectedOption(type);
            clickDeviceTypeDropdown();
            return selectedOption != null;
        }
        catch (NoSuchElementException e) {
            clickDeviceTypeDropdown();
            return false;
        }
    }

    /**
     * Checks to see if application name Whitespace error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isNameWhitespaceErrorDisplayed() {
        return isWhitespaceErrorDisplayed(RemoteAppField.NAME.getLabel());
    }

    /**
     * Checks to see if Field Required error icon/tooltip is displayed with the
     * Name text box
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isNameFieldRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(RemoteAppField.NAME.getLabel());
    }

    /**
     * Checks to see if Field Required error icon/tooltip is displayed with the
     * Device Type dropdown
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isDeviceTypeFieldRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(RemoteAppField.DEVICE_TYPE.getLabel());
    }

    /**
     * Checks to see if Invalid Name error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    @Override
    public boolean isInvalidNameErrorDisplayed() {
        return isElementDisplayed(getSpecificErrorToolTip(INVALID_APP_NAME_ERROR_TEXT));
    }

    /**
     * Checks to see if duplicate app name value error is displayed
     *
     * @param app the app to be selected
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isDuplicateAppNameErrorDisplayed(UserRemoteApp app) {
        String appName = app.getName();
        return isElementDisplayed(getSpecificErrorToolTip(UNIQUE_APP_NAME_ERROR_TEXT_PREFIX + appName + UNIQUE_APP_NAME_ERROR_TEXT_SUFFIX));
    }

    /**
     * Checks to see if valid app name error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isInvalidAppNameErrorDisplayed() {
        return isElementDisplayed(getSpecificErrorToolTip(INVALID_APP_NAME_REQUIRED_ERROR_TEXT));
    }

    ///////////
    // Interactions
    ///////////

    /**
     * Click the Create Remote Application Help icon
     */
    public void clickHelpIcon() {
        getHelpIcon(CREATE_REMOTE_APP_HEADER_TEXT).click();
    }

    /**
     * Click the Create Remote Application Close icon
     *
     * @return the newly loaded Apps Page
     */
    public AppsPage clickCloseIcon() {
        getCloseIcon(CREATE_REMOTE_APP_HEADER_TEXT).click();
        return getPage(AppsPage.class);
    }

    /**
     * Clicks the Create button
     *
     * @return the Create Remote Application Modal
     * @param success -true if success is expected, false if error expected
     */
    public RemoteApplicationsPartialPage clickCreate(boolean success) {
        getCreateBtn().click();
        if (success) {
            waitForElementToBeInvisible(By.xpath(NAME_TEXT_BOX_XPATH));
            return getPage(RemoteApplicationsPartialPage.class);
        }
        else {
            return null;
        }
    }

    /**
     * Clicks the Reset button
     */
    public void clickResetBtn() {
        getResetBtn().click();
    }

    /**
     * Clicks the Cancel button
     */
    public void clickCancelBtn() {
        getCancelBtn().click();
    }

    /**
     * Sets the text in the Name text box
     *
     * @param text the text to set in the text box
     */
    public void setNameText(String text) {
        getNameTextBox().setText(text);
    }

    /**
     * Sets the text in the Name text box
     *
     * @param app the app whose name should be set
     */
    public void setNameText(UserRemoteApp app) {
        String appName = app.getName();
        setNameText(appName);
    }

    /**
     * Clicks the Device Type dropdown
     */
    public void clickDeviceTypeDropdown() {
        getDeviceTypeSelector().click();
    }

    /**
     * Selects the given device type from the device Type drop down, also
     * handles clicking dropdown to expand
     *
     * @param deviceType -the app type to select
     */
    public void selectDeviceType(String deviceType) {
        clickDeviceTypeDropdown();
        getDeviceTypeDropdownOption(deviceType).click();
    }

    /**
     * Selects the given device type from the device Type drop down, also
     * handles clicking dropdown to expand
     *
     * @param deviceType -the app type to select
     */
    public void selectDeviceType(DeviceType deviceType) {
        clickDeviceTypeDropdown();
        getDeviceTypeDropdownOption(deviceType).click();
    }

    /**
     * Selects the device type of the given app in the Device Type drop down,
     * also handles clicking dropdown to expand
     *
     * @param app -the user remote app
     */
    public void selectDeviceType(UserRemoteApp app) {
        DeviceType type = app.getDeviceType();
        selectDeviceType(type);
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(CREATE_REMOTE_APP_HELP_HEADER_TEXT);
    }

    ///////////
    // Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkHeaderIcons() {
        checkHeaderIcons(CREATE_REMOTE_APP_HEADER_TEXT);
    }

    /**
     * Verifies expected header and content text is displayed in the help modal
     */
    public void checkHelpModal() {
        Assert.assertTrue("Create Remote Application Help header not displayed after clicking help icon.", isHelpHeaderDisplayed());
        Assert.assertTrue("Create Remote Application help text is not displayed. Expected: '" + CREATE_REMOTE_APP_HELP_CONTENT_TEXT + "'", isHelpContentDisplayed());
    }

    /**
     * Verifies the following fields display: Name text box, Device Type
     * dropdown
     */
    public void checkFieldsDisplayed() {
        Assert.assertTrue("The Name text box is not displayed in the Create Remote Application modal.", isNameTextboxDisplayed());
        Assert.assertTrue("The Device Type dropdown is not displayed in the Create Remote Application modal.", isDeviceTypeDropdownDisplayed());
    }

    /**
     * Verifies the following buttons display at the bottom of the modal:
     * register, reset, cancel
     */
    public void checkFormBtns() {
        Assert.assertTrue("The Create button is not displayed in the Create Remote Application modal.", isCreateBtnDisplayed());
        Assert.assertTrue("The Cancel button is not displayed in the Create Remote Application modal.", isCancelBtnDisplayed());
        Assert.assertTrue("The Reset button is not displayed in the Create Remote Application modal.", isResetBtnDisplayed());
    }

    /**
     * Sets the Name with info associated with the given values
     *
     * @param name -the app name
     * @param deviceType -the app device type
     */
    public void setAllFields(String name, String deviceType) {
        setNameText(name);
        selectDeviceType(deviceType);
    }

    /**
     * Sets the Name and Target Device fields with info associated with the
     * given app
     *
     * @param app -the User Remote App who values should be set
     */
    public void setAllFields(UserRemoteApp app) {
        setNameText(app);
        selectDeviceType(app);
    }

    /**
     * Checks that displayed values match values associated with the given app
     *
     * @param app - User Remote App expected
     */
    public void checkAllFields(UserRemoteApp app) {
        String name = app.getName();
        String type = app.getDeviceType().getUILabel();
        Assert.assertEquals("Value in the Name text box not displayed as expected.", name, getDisplayedName());
        Assert.assertTrue("Device Type drop down value not displayed as expected.", isDeviceTypeSelected(type));
    }

    /**
     * Checks that displayed values are reset.
     */
    public void checkDefaultValues() {
        Assert.assertEquals("The Name text box was not cleared as expected.", "", getDisplayedName());
        Assert.assertTrue("The Device Type drop down was not cleared as expected.", isDeviceTypeSelected(null));
    }

    /**
     * Verifies 'required field' error displays for all fields (checks Name and
     * Device Type fields)
     */
    public void checkRequiredFieldErrorAllFields() {
        Assert.assertTrue("Required field error is not displayed for the Name field.", isFieldRequiredErrorDisplayed(RemoteAppField.NAME.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Device Type field.", isFieldRequiredErrorDisplayed(RemoteAppField.DEVICE_TYPE.getLabel()));
    }

    //////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(DEVICE_TYPE_DROPDOWN_XPATH));
    }

    /**
     * Waits for the Uploading Remote App alert to disappear
     */
    public void waitForAlert() {
        waitForElementToBeInvisible(UPLOADING_APP_ALERT_LOCATOR);
    }

}
