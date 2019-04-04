package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.UserNativeApp;
import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.enums.DeviceType;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.AppsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the create native app form window
 *
 * @author llaroussini
 * @author rsmith
 */
public class CreateNativeApplicationModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver -web driver
     */
    public CreateNativeApplicationModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    /**
     * Native App Field names
     *
     * @author llaroussini
     */
    public enum NativeAppTextBox {
        /**
         * Native App Name field name
         */
        NATIVE_APP_NAME("applicationProfileName", "Name:", "name"),
        /**
         * Device Type field name
         */
        DEVICE_TYPE("deviceType", "Device Type:", "device type"),
        /**
         * Command Line field name
         */
        CMD_LINE("commandLine", "Command Line:", "command line"),
        /**
         * Host field name
         */
        HOST("hostAddress", "Host Address:", "host address"),
        /**
         * Port field name
         */
        PORT("portNumber", "Port Number:", "port number");

        /**
         * The name attribute of the fields in the Create Native App window
         */
        private String name;

        /**
         * The label of the fields in the Create Native App window
         */
        private String label;

        /**
         * The name used in the error messages for Create Native App
         */
        private String error;

        /**
         * Default constructor
         *
         * @param name The string to set as the name attribute
         * @param label The string to set as the field label
         * @param the name used in the error messages
         */
        NativeAppTextBox(String name, String label, String error) {
            this.name = name;
            this.label = label;
            this.error = error;
        }

        /**
         * Gets the name attribute of the field in the Create Native App window
         *
         * @return The name attribute of the field
         */
        public String getName() {
            return this.name;
        }

        /**
         * Gets the label of the field in the Create Native App window
         *
         * @return The label of the field
         */
        public String getLabel() {
            return this.label;
        }

        /**
         * Gets the error name of the field in the Create Native App Window
         *
         * @return the name attribute used in the error message
         */
        public String getErrorName() {
            return this.error;
        }
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the create native app form header
     */
    private static final String CREATE_NATIVE_APP_HEADER_TEXT = "Create Native Application";

    /**
     * xpath for Create Native Application form header
     */
    private static final String CREATE_NATIVE_APP_HEADER_XPATH = "//div[contains(@class, 'x-title-item')][@data-ref='textEl'][text()='Create Native Application']";

    /**
     * Text displayed in the create native app help header
     */
    private static final String CREATE_NATIVE_APP_HELP_HEADER_TEXT = "Create Native Application Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Create a native application with the specified name, command line, host address, port number, and device type. The application name must be unique across all applications and, once created, the device type cannot be changed.";

    /**
     * Xpath prefix to text box in Create Native App window
     */
    private static final String NATIVE_APP_TEXT_BOX_XPATH_PREFIX = "//input[@name='";

    /**
     * The xpath prefix of the Device Type dropdown
     */
    private static final String DEVICE_TYPE_DROPDOWN_OPTION_XPATH_PREFIX = "//li[text()='";

    /**
     * The xpath of the Device Type dropdown selector
     */
    private static final String DEVICE_TYPE_DROPDOWN_SELECTOR_XPATH = NATIVE_APP_TEXT_BOX_XPATH_PREFIX + "deviceType']/ancestor::div[3]//div[contains(@class,'x-form-arrow-trigger-default')]";

    /**
     * Xpath of the selected option in drop down
     */
    private static final String SELECTED_OPTION_XPATH_PREFIX = ".//li[contains(@class, 'x-boundlist-selected')][text()='";

    /**
     * Xpath to the Create button in Native Apps window
     */
    private static final String NATIVE_APP_CREATE_BTN_XPATH = CREATE_NATIVE_APP_HEADER_XPATH + "//ancestor::div[contains(@id, 'native')]//div[contains(@id, 'toolbar')]//span[text()='Create']";

    /**
     * Error text displayed when invalid app name is used
     */
    private static final String INVALID_APP_NAME_ERROR_TEXT = "Application names may contain only letters, digits, hyphens, underscores, and nonconsecutive spaces.";

    /**
     * Error text displayed when invalid command line is used
     */
    private static final String INVALID_CMD_LINE_ERROR_TEXT = "Command lines may contain only letters, digits, hyphens, periods, underscores, and nonconsecutive spaces.";

    /**
     * Error text displayed when invalid host is used
     */
    private static final String INVALID_HOST_ERROR_TEXT_PREFIX = "The value \"";

    /**
     * Error text displayed when invalid host is used
     */
    private static final String INVALID_HOST_ERROR_TEXT_SUFFIX = "\" is not a recognized IP address.";

    /**
     * Error text displayed when invalid port is used (exceeding max)
     */
    private static final String INVALID_PORT_ERROR_TEXT = "Port number values must be in the range of 1 to 65,535.";

    /**
     * Error text displayed when an application name is submitted blank.
     */
    private static final String INVALID_APP_NAME_REQUIRED_ERROR_TEXT = "A valid application name is required.";

    /**
     * Error text displayed when a field is submitted blank prefix.*
     */
    private static final String INVALID_BLANK_FIELD_REQUIRED_ERROR_TEXT_PREFIX = "A valid ";

    /**
     * Error text displayed when a field is submitted blank suffix.
     */
    private static final String INVALID_BLANK_FIELD_REQUIRED_ERROR_TEXT_SUFFIX = " is required.";

    /**
     * Error text displayed when an invalid character is copied into the Port
     * Number textbox.
     */
    private static final String INVALID_PORT_CHARACTER_ERROR_TEXT_PREFIX = " is not a valid number.";

    /**
     * Error text displayed when a duplicate app name is used.
     */
    private static final String UNIQUE_APP_NAME_ERROR_TEXT_PREFIX = "An application with the name \"";

    /**
     * Error text displayed when a duplicate app name is used suffix
     */
    private static final String UNIQUE_APP_NAME_ERROR_TEXT_SUFFIX = "\" already exists.";

    /////////////
    // Getters
    /////////////

    /**
     * Gets the Create button
     *
     * @return the Create button
     */
    private El getCreateBtn() {
        return el(By.xpath(NATIVE_APP_CREATE_BTN_XPATH));
    }

    /**
     * Gets the given text box element
     *
     * @param name -the name of the text box to get
     * @return the text box element
     */
    private El getTextBox(NativeAppTextBox name) {
        return el(By.xpath(NATIVE_APP_TEXT_BOX_XPATH_PREFIX + name.name + "']"));
    }

    /**
     * Gets the reset button
     *
     * @return the reset button
     */
    private El getResetBtn() {
        return getFormBtn(CREATE_NATIVE_APP_HEADER_TEXT, BtnNames.RESET.getLabel());
    }

    /**
     * Gets the cancel button
     *
     * @return the cancel button
     */
    private El getCancelBtn() {
        return getFormBtn(CREATE_NATIVE_APP_HEADER_TEXT, BtnNames.CANCEL.getLabel());
    }

    /**
     * Gets the create native apps help icon
     *
     * @return the help icon
     */
    private El getNativeAppHelpIcon() {
        return getHelpIcon(CREATE_NATIVE_APP_HEADER_TEXT);
    }

    /**
     * Gets option from Device Types drop down list
     *
     * @param deviceType -the app type expected
     * @return the option element
     */
    private El getTargetDeviceDropdownOption(DeviceType deviceType) {
        String type = deviceType.getUILabel();
        return el(By.xpath(DEVICE_TYPE_DROPDOWN_OPTION_XPATH_PREFIX + type + "']"));
    }

    /**
     * Gets option from Device Type drop down list
     *
     * @param deviceType -the app type (as string) expected
     * @return the option element
     */
    private El getDeviceTypeDropdownOption(String deviceType) {
        return el(By.xpath(DEVICE_TYPE_DROPDOWN_OPTION_XPATH_PREFIX + deviceType + "']"));
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
     * Gets the selected option within drop down
     *
     * @param option -the option selected
     * @return the selected option element
     */
    private El getSelectedOption(String option) {
        return el(By.xpath(SELECTED_OPTION_XPATH_PREFIX + option + "']"));
    }

    /**
     * Gets the value in the Native App Name text box
     *
     * @return the displayed Native App Name value
     */
    public String getDisplayedAppName() {
        return getTextBox(NativeAppTextBox.NATIVE_APP_NAME).getAttribute("value");
    }

    /**
     * Gets the value in the Command Line text box
     *
     * @return the displayed Command Line value
     */
    public String getDisplayedCommandLine() {
        return getTextBox(NativeAppTextBox.CMD_LINE).getAttribute("value");
    }

    /**
     * Gets the value in the Host text box
     *
     * @return the displayed Host value
     */
    public String getDisplayedHost() {
        return getTextBox(NativeAppTextBox.HOST).getAttribute("value");
    }

    /**
     * Gets the value in the Port text box
     *
     * @return the displayed Port value
     */
    public String getDisplayedPort() {
        return getTextBox(NativeAppTextBox.PORT).getAttribute("value");
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the create native app form header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCreateNativeAppHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_NATIVE_APP_HEADER_TEXT);
    }

    /**
     * Checks to see if the create native app help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCreateNativeAppHelpHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_NATIVE_APP_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isCreateNativeAppHelpIconDisplayed() {
        return isHelpIconDisplayed(CREATE_NATIVE_APP_HEADER_TEXT);
    }

    /**
     * Checks to see if the close icon is displayed
     *
     * @return true if the close icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isCreateNativeAppCloseIconDisplayed() {
        return isCloseIconDisplayed(CREATE_NATIVE_APP_HEADER_TEXT);
    }

    /**
     * Checks if the given field is displayed
     *
     * @param field -the field expected
     * @return true if displayed, false otherwise
     */
    public boolean isTextBoxDisplayed(NativeAppTextBox field) {
        return isElementDisplayed(getTextBox(field));
    }

    /**
     * Checks if Device Type dropdown is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isTargetDeviceDropdownDisplayed() {
        return isElementDisplayed(getDeviceTypeSelector());
    }

    /**
     * Checks to see if the Create button is displayed
     *
     * @return true if the Create button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCreateBtnDisplayed() {
        return isElementDisplayed(getCreateBtn());
    }

    /**
     * Checks to see if the reset button is displayed
     *
     * @return true if the reset button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isResetBtnDisplayed() {
        return isElementDisplayed(getResetBtn());
    }

    /**
     * Checks to see if the cancel button is displayed
     *
     * @return true if the cancel button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCancelBtnDisplayed() {
        return isElementDisplayed(getCancelBtn());
    }

    /**
     * Checks to see if the help content is displayed
     *
     * @return true if the help content is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(CREATE_NATIVE_APP_HELP_HEADER_TEXT);
    }

    /**
     * Checks if given app type is selected -- method clicks app type dropdown,
     * checks for presence of selected type, then closes dropdown (or returns
     * null and closes dropdown)
     *
     * @param type -the app type expected to be selected
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
     * Checks to see if Invalid App Name error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    @Override
    public boolean isInvalidNameErrorDisplayed() {
        return isElementDisplayed(getSpecificErrorToolTip(INVALID_APP_NAME_ERROR_TEXT));
    }

    /**
     * Checks to see if Duplicate App Name error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isDuplicateAppNameErrorDisplayed(UserNativeApp app) {
        String appName = app.getName();
        return isElementDisplayed(getSpecificErrorToolTip(UNIQUE_APP_NAME_ERROR_TEXT_PREFIX + appName + UNIQUE_APP_NAME_ERROR_TEXT_SUFFIX));
    }

    /**
     * Checks to see if Invalid Command Line error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isInvalidCommandLineErrorDisplayed() {
        return isElementDisplayed(getSpecificErrorToolTip(INVALID_CMD_LINE_ERROR_TEXT));
    }

    /**
     * Checks to see if Invalid Host error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isInvalidHostErrorDisplayed() {
        return isElementDisplayed(getSpecificErrorToolTip(INVALID_HOST_ERROR_TEXT_PREFIX));
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

    /**
     * Checks to see if leading/trailing whitespace error icon/tooltop is
     * displayed for Command Line
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCommandLineLeadingTrailingWhiteSpaceErrorDisplayed() {
        return isWhitespaceErrorDisplayed(NativeAppTextBox.CMD_LINE.getLabel());
    }

    /**
     * Checks to see if leading/trailing whitespace error icon/tooltop is
     * displayed for App Name
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isAppNameLeadingTrailingWhiteSpaceErrorDisplayed() {
        return isWhitespaceErrorDisplayed(NativeAppTextBox.NATIVE_APP_NAME.getLabel());
    }

    /**
     * Checks to see if valid device type error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isBlankDeviceTypeErrorDisplayed() {
        return isElementDisplayed(getSpecificErrorToolTip(INVALID_BLANK_FIELD_REQUIRED_ERROR_TEXT_PREFIX + NativeAppTextBox.DEVICE_TYPE.getLabel() + INVALID_BLANK_FIELD_REQUIRED_ERROR_TEXT_SUFFIX));
    }

    /**
     * checks to see if valid host address error icon/tooltip is displayed
     *
     * @return true if icon/tooltip is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isInvalidHostAddressErrorDisplayed(String value) {
        return isElementDisplayed(getSpecificErrorToolTip(INVALID_HOST_ERROR_TEXT_PREFIX + value + INVALID_HOST_ERROR_TEXT_SUFFIX));
    }

    /**
     * Checks to see if valid command line error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed,false if it is not or
     *         cannot be found
     */
    public boolean isBlankCommandLineErrorDisplayed() {
        return isElementDisplayed(getSpecificErrorToolTip(INVALID_BLANK_FIELD_REQUIRED_ERROR_TEXT_PREFIX + NativeAppTextBox.CMD_LINE.getErrorName() + INVALID_BLANK_FIELD_REQUIRED_ERROR_TEXT_SUFFIX));
    }

    /**
     * Checks to see if valid host address error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed,false if it is not or
     *         cannot be found
     */
    public boolean isBlankHostAddressErrorDisplayed() {
        return isElementDisplayed(getSpecificErrorToolTip(INVALID_BLANK_FIELD_REQUIRED_ERROR_TEXT_PREFIX + NativeAppTextBox.HOST.getErrorName() + INVALID_BLANK_FIELD_REQUIRED_ERROR_TEXT_SUFFIX));
    }

    /**
     * Checks to see if valid port number error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed,false if it is not or
     *         cannot be found
     */
    public boolean isBlankPortNumberErrorDisplayed() {
        return isElementDisplayed(getSpecificErrorToolTip(INVALID_BLANK_FIELD_REQUIRED_ERROR_TEXT_PREFIX + NativeAppTextBox.PORT.getLabel() + INVALID_BLANK_FIELD_REQUIRED_ERROR_TEXT_SUFFIX));
    }

    /**
     * Checks to see if Invalid Number error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isInvalidNonNumericPortErrorDisplayed(String text) {
        return isElementDisplayed(getSpecificErrorToolTip(text + INVALID_PORT_CHARACTER_ERROR_TEXT_PREFIX));
    }

    /**
     * Checks to see if number exceeding max error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isPortMaxExceededErrorDisplayed() {
        return isElementDisplayed(getSpecificErrorToolTip(INVALID_PORT_ERROR_TEXT));
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Click the Create button
     *
     * @return the newly loaded AppsPage
     */
    public AppsPage clickCreate() {
        getCreateBtn().click();
        return getPage(AppsPage.class);
    }

    /**
     * Click the Reset button
     */
    public void clickReset() {
        getResetBtn().click();
    }

    /**
     * Clicks the Close icon
     */
    public void clickCloseIcon() {
        clickCloseIcon(CREATE_NATIVE_APP_HEADER_TEXT);
    }

    /**
     * Click the Cancel button
     *
     * @return the newly loaded/updated Native Apps Partial Page
     */
    public NativeAppsPartialPage clickCancel() {
        getCancelBtn().click();
        return getPage(NativeAppsPartialPage.class);
    }

    /**
     * Click the Create button
     *
     * @param success - true if success expected, false otherwise
     * @return the newly loaded/updated Native Applications page if success is
     *         true, otherwise return null
     */
    public NativeAppsPartialPage clickCreate(boolean success) {
        getCreateBtn().click();
        if (success) {
            return getPage(NativeAppsPartialPage.class);
        }
        else {
            return null;
        }
    }

    /**
     * Click the Native App Help icon
     */
    public void clickCreateNativeAppHelpIcon() {
        getNativeAppHelpIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(CREATE_NATIVE_APP_HELP_HEADER_TEXT);
    }

    /**
     * Sets the given id in the Native App Name text box
     *
     * @param name -the app name to set
     */
    public void setNativeAppName(String name) {
        getTextBox(NativeAppTextBox.NATIVE_APP_NAME).setText(name);
    }

    /**
     * Sets the name of the given app in the Native App Name text box
     *
     * @param app -the user native app
     */
    public void setNativeAppName(UserNativeApp app) {
        String name = app.getName();
        setNativeAppName(name);
    }

    /**
     * Clicks the Device Type dropdown
     */
    public void clickDeviceTypeDropdown() {
        getDeviceTypeSelector().click();
    }

    /**
     * Selects the given app type from the Device Type drop down, also handles
     * clicking dropdown to expand
     *
     * @param deviceType -the app type to select
     */
    public void selectTargetDevice(DeviceType deviceType) {
        clickDeviceTypeDropdown();
        getTargetDeviceDropdownOption(deviceType).click();
    }

    /**
     * Selects the given app type from the Device Type drop down, also handles
     * clicking dropdown to expand
     *
     * @param deviceType -the app type (as string) to select
     */
    public void selectTargetDevice(String deviceType) {
        clickDeviceTypeDropdown();
        getDeviceTypeDropdownOption(deviceType).click();
    }

    /**
     * Selects the type of the given app in the Device Type drop down, also
     * handles clicking dropdown to expand
     *
     * @param app -the user native app
     */
    public void selectTargetDevice(UserNativeApp app) {
        DeviceType type = app.getDeviceType();
        selectTargetDevice(type);
    }

    /**
     * Sets the given command in the Command Line text box
     *
     * @param cmdLine -the command to set
     */
    public void setCommandLine(String cmdLine) {
        getTextBox(NativeAppTextBox.CMD_LINE).setText(cmdLine);
    }

    /**
     * Sets the Command Line associated with the given app in the Command Line
     * text box
     *
     * @param app -the user native app
     */
    public void setCommandLine(UserNativeApp app) {
        String cmdLine = app.getCommandLine();
        setCommandLine(cmdLine);
    }

    /**
     * Sets the given host in the Host text box
     *
     * @param app -the host name to set
     */
    public void setHost(String host) {
        getTextBox(NativeAppTextBox.HOST).setText(host);
    }

    /**
     * Sets the host associated with the given app in the Host text box
     *
     * @param app -the user native app
     */
    public void setHost(UserNativeApp app) {
        String host = app.getHost();
        setHost(host);
    }

    /**
     * Sets the given port in the Port text box
     *
     * @param port -the port name to set
     */
    public void setPort(String port) {
        getTextBox(NativeAppTextBox.PORT).setText(port);
    }

    /**
     * Sets the port associated with the given app in the Host text box
     *
     * @param app -the user native app
     */
    public void setPort(UserNativeApp app) {
        String port = app.getPort();
        setPort(port);
    }

    /**
     * Copy and paste value from Command Line in Port text box
     */
    public void copyAndPasteInPort() {
        copyPaste(getTextBox(NativeAppTextBox.CMD_LINE), getTextBox(NativeAppTextBox.PORT));
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkCreateNativeAppHeaderIcons() {
        checkHeaderIcons(CREATE_NATIVE_APP_HEADER_TEXT);
    }

    /**
     * Checks for the presence of the Create, Reset, and Cancel buttons in
     * Create Native App window
     */
    public void checkBtns() {
        Assert.assertTrue("The Create button is not displayed.", isCreateBtnDisplayed());
        Assert.assertTrue("The Reset button is not displayed.", isResetBtnDisplayed());
        Assert.assertTrue("The Cancel button is not displayed.", isCancelBtnDisplayed());
    }

    /**
     * Checks for presence of all fields (Name text box, Device Type drop down,
     * Command Line text box, Host text box, and Port Number text box)
     */
    public void checkAllFields() {
        Assert.assertTrue("Native App Name text box is not displayed.", isTextBoxDisplayed(NativeAppTextBox.NATIVE_APP_NAME));
        Assert.assertTrue("Device Type drop down is not displayed.", isTargetDeviceDropdownDisplayed());
        Assert.assertTrue("Command Line text box is not displayed.", isTextBoxDisplayed(NativeAppTextBox.CMD_LINE));
        Assert.assertTrue("Host text box is not displayed.", isTextBoxDisplayed(NativeAppTextBox.HOST));
        Assert.assertTrue("Port text box is not displayed.", isTextBoxDisplayed(NativeAppTextBox.PORT));
    }

    /**
     * Sets the Native App Name, Device Type, Command Line, Host, and Port
     * fields with info associated with the given app
     *
     * @param app2 -the User Native App who values should be set
     */
    public void setAllFields(UserNativeApp app2) {
        setNativeAppName(app2);
        selectTargetDevice(app2);
        setCommandLine(app2);
        setHost(app2);
        setPort(app2);
    }

    /**
     * Sets the given App Name, Device Type, Command Line, Host, and Port fields
     *
     * @param appName - the app name (as string) to set
     * @param devType - the device type (as string) to set
     * @param cmdLine - the command (as string) to set
     * @param host - the host (as string) to set
     * @param port - the port (as string) to set
     */
    public void setAllFields(String appName, String devType, String cmdLine, String host, String port) {
        setNativeAppName(appName);
        selectTargetDevice(devType);
        setCommandLine(cmdLine);
        setHost(host);
        setPort(port);
    }

    /**
     * Checks that displayed values match values associated with the given app
     *
     * @param app - User Native App expected
     */
    public void checkAllFields(UserNativeApp app) {
        String id = app.getName();
        String devType = app.getDeviceType().getUILabel();
        String cmdLine = app.getCommandLine();
        String host = app.getHost();
        String port = app.getPort();
        Assert.assertEquals("Native App Name value not displayed as expected.", id, getDisplayedAppName());
        Assert.assertTrue("Device Type selection not displayed as expected.", isDeviceTypeSelected(devType));
        Assert.assertEquals("Command Line value not displayed as expected.", cmdLine, getDisplayedCommandLine());
        Assert.assertEquals("Host value not displayed as expected.", host, getDisplayedHost());
        Assert.assertEquals("Port value not displayed as expected.", port, getDisplayedPort());
    }

    /**
     * Checks that displayed values are cleared/reset.
     */
    public void checkClearedFields() {
        Assert.assertEquals("Native App Name text box was not cleared as expected.", "", getDisplayedAppName());
        Assert.assertTrue("Device Type drop down was not cleared as expected.", isDeviceTypeSelected(null));
        Assert.assertEquals("Command Line text box was not cleared as expected.", "", getDisplayedCommandLine());
        Assert.assertEquals("Host text box was not cleared as expected.", "", getDisplayedHost());
        Assert.assertEquals("Port text box was not cleared as expected.", "", getDisplayedPort());
    }

    /**
     * Verifies 'required field' error displays for all fields (checks Native
     * App Name, Device Type, Command Line, Host, and Port)
     */
    public void checkRequiredFieldErrorAllFields() {
        Assert.assertTrue("Required field error is not displayed for the Native App Name field.", isFieldRequiredErrorDisplayed(NativeAppTextBox.NATIVE_APP_NAME.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Device Type field.", isFieldRequiredErrorDisplayed(NativeAppTextBox.DEVICE_TYPE.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Command Line field.", isFieldRequiredErrorDisplayed(NativeAppTextBox.CMD_LINE.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Host field.", isFieldRequiredErrorDisplayed(NativeAppTextBox.HOST.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Port field.", isFieldRequiredErrorDisplayed(NativeAppTextBox.PORT.getLabel()));
    }

    //////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(CREATE_NATIVE_APP_HEADER_XPATH));
    }
}
