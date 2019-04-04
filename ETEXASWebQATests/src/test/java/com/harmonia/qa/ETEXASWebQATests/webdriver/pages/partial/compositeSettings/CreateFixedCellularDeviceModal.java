package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.FixedCellularDevice;
import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the add fixed cellular device form
 *
 * @author llaroussini
 * @author rsmith
 */
public class CreateFixedCellularDeviceModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver
     */
    public CreateFixedCellularDeviceModal(WebDriver driver) {
        super(driver);
    }

    //////////////////
    // Enumerations
    /////////////////

    /**
     * Enumeration of Create Fixed Cellular text boxes
     *
     * @author rsmith
     */
    public enum CreateFixedCellularTextBox {
        /**
         * Name text box
         */
        NAME("Name:", "deviceName"),
        /**
         * X Coordinate text box
         */
        X_COORDINATE("X (cm):", "x"),
        /**
         * Y Coordinate text box
         */
        Y_COORDINATE("Y (cm):", "y"),
        /**
         * Z Coordinate text box
         */
        Z_COORDINATE("Z (cm):", "z"),

        /**
         * MAC Address text box
         */
        MAC_ADDRESS("MAC Address:", "macAddress");

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
        CreateFixedCellularTextBox(String label, String name) {
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
     * Text displayed in the add fixed cellular device header
     */
    private static final String CREATE_FIXED_CELLULAR_DEVICE_HEADER_TEXT = "Create Fixed Cellular Device";

    /**
     * Text displayed in the add fixed cellular device help header
     */
    private static final String CREATAE_FIXED_CELLULAR_DEVICE_HELP_HEADER_TEXT = "Create Fixed Cellular Device Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Add a new fixed cellular device with the specified name, mac address, and coordinates to the simulation. Once created, the applications that are hosted on the device can be configured.";

    /**
     * The xpath of the fixed cellular device name text box
     */
    private static final String FIXED_CELLULAR_DEVICE_NAME_TEXT_BOX_XPATH = "//input[@name='name']";

    /**
     * The xpath of the MAC address text box
     */
    private static final String MAC_ADDRESS_TEXT_BOX_XPATH = "//input[@name='macAddress']";

    /**
     * The xpath of the x coordinate text box
     */
    private static final String X_COORDINATE_TEXT_BOX_XPATH = "//input[@name='x']";

    /**
     * The xpath of the y coordinate text box
     */
    private static final String Y_COORDINATE_TEXT_BOX_XPATH = "//input[@name='y']";

    /**
     * The xpath of the z coordinate text box
     */
    private static final String Z_COORDINATE_TEXT_BOX_XPATH = "//input[@name='z']";

    /**
     * Xpath prefix to device buttons
     */
    private static final String ADD_FIXED_CELLULAR_DEVICE_BTN_XPATH_PREFIX = "//div[contains(@id, 'cell')][contains(@id, 'device')][contains(@id, 'form')]//div[contains(@id, 'toolbar')]//span[contains(@id, 'button')][text()='";

    /**
     * Text displayed as create RSE help content
     */
    private static final String CREATE_FIXED_CELLULAR_DEVICE_HELP_CONTENT_TEXT = "Create a new fixed cellular device with the specified name, MAC address, and coordinates. The device name and MAC address must each be unique in the composite. The x (cm), y (cm), and z (cm) coordinates are specified relative to the center point of the simulation.";

    /**
     * The xpath prefix to all text boxes in the Create Fixed Cellular modal
     */
    private static final String TEXT_BOX_XPATH_PREFIX = "//input[@name='";

    /**
     * The error text prefix for duplicate MAC address error
     */
    private static final String DUPLICATE_MAC_ADDRESS_ERROR_TEXT_PREFIX = "A device with MAC address \"";

    /**
     * The error text suffix for duplicate MAC address error
     */
    private static final String DUPLICATE_MAC_ADDRESS_ERROR_TEXT_SUFFIX = "\" already exists in the composite.";

    ///////////
    //Getters
    ///////////

    /**
     * Gets the given button in the Add cellular device window
     *
     * @param btn - the button to get
     * @return the button element
     */
    private El getBtn(BtnNames btn) {
        return el(By.xpath(ADD_FIXED_CELLULAR_DEVICE_BTN_XPATH_PREFIX + btn.getLabel() + "']"));
    }

    /**
     * Gets the given text box in the Create RSE modal
     *
     * @param textBox -the specific text box to get
     * @return the text box element
     */
    private El getTextBox(CreateFixedCellularTextBox textBox) {
        return el(By.xpath(TEXT_BOX_XPATH_PREFIX + textBox.getName() + "']"));
    }

    /**
     * Gets the Fixed Cellular Device Name text box
     *
     * @return the text box element
     */
    private El getFixedCellularNameTextBox() {
        return el(By.xpath(FIXED_CELLULAR_DEVICE_NAME_TEXT_BOX_XPATH));
    }

    /**
     * Gets the Mac Address text box
     *
     * @return the text box element
     */
    private El getMacAddressTextBox() {
        return el(By.xpath(MAC_ADDRESS_TEXT_BOX_XPATH));
    }

    /**
     * Gets the X Coordinate text box
     *
     * @return the text box element
     */
    private El getXCoordinateTextBox() {
        return el(By.xpath(X_COORDINATE_TEXT_BOX_XPATH));
    }

    /**
     * Gets the Y Coordinate text box
     *
     * @return the text box element
     */
    private El getYCoordinateTextBox() {
        return el(By.xpath(Y_COORDINATE_TEXT_BOX_XPATH));
    }

    /**
     * Gets the Z Coordinate text box
     *
     * @return the text box element
     */
    private El getZCoordinateTextBox() {
        return el(By.xpath(Z_COORDINATE_TEXT_BOX_XPATH));
    }

    /**
     * Gets the value displayed in the given text box in the Create Fixed
     * Cellular Device modal
     *
     * @param textBox -the specific text box from which to get the value
     * @return the displayed value in the given text box
     */
    private String getTextBoxValue(CreateFixedCellularTextBox textBox) {
        return getTextBox(textBox).getAttribute("value");
    }

    /**
     * Gets the value in the fixed cellular device name text box
     *
     * @return the displayed Name value
     */
    public String getDisplayedFixedCellularDeviceName() {
        return getFixedCellularNameTextBox().getAttribute("value");
    }

    /**
     * Gets the value in the mac address text box
     *
     * @return the displayed mac address value
     */
    public String getDisplayedMacAddress() {
        return getMacAddressTextBox().getAttribute("value");
    }

    /**
     * Gets the value in the x coordinate text box
     *
     * @return the displayed x coordinate value
     */
    public String getDisplayedXCoordinate() {
        return getXCoordinateTextBox().getAttribute("value");
    }

    /**
     * Gets the value in the y coordinate text box
     *
     * @return the displayed y coordinate value
     */
    public String getDisplayedYCoordinate() {
        return getYCoordinateTextBox().getAttribute("value");
    }

    /**
     * Gets the value in the z coordinate text box
     *
     * @return the displayed z coordinate value
     */
    public String getDisplayedZCoordinate() {
        return getZCoordinateTextBox().getAttribute("value");
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the add fixed cellular device header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCreateFixedCellularDeviceHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_FIXED_CELLULAR_DEVICE_HEADER_TEXT);
    }

    /**
     * Checks to see if the add fixed cellular device help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCreateFixedCellularDeviceHelpHeaderDisplayed() {
        return isHeaderDisplayed(CREATAE_FIXED_CELLULAR_DEVICE_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the Create FIxed Cellular Devce help content is
     * displayed
     *
     * @return true if the content text is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCreateFixedCellularDeviceHelpContentDisplayed() {
        return isContentDisplayed(CREATE_FIXED_CELLULAR_DEVICE_HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isCreateFixedCellularDeviceHelpIconDisplayed() {
        return isHelpIconDisplayed(CREATE_FIXED_CELLULAR_DEVICE_HEADER_TEXT);
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
        return isHelpOKBtnDisplayed(CREATAE_FIXED_CELLULAR_DEVICE_HELP_HEADER_TEXT);
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
     * Checks to see if the fixed cellular device name text box is displayed
     *
     * @return true if the text box is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isFixedCellularNameTextBoxDisplayed() {
        return isElementDisplayed(getFixedCellularNameTextBox());
    }

    /**
     * Checks to see if the mac address text box is displayed
     *
     * @return true if the text box is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isMacAddressTextBoxDisplayed() {
        return isElementDisplayed(getMacAddressTextBox());
    }

    /**
     * Checks to see if the x coordinate text box is displayed
     *
     * @return true if the text box is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isXCoordinateTextBoxDisplayed() {
        return isElementDisplayed(getXCoordinateTextBox());
    }

    /**
     * Checks to see if the y coordinate text box is displayed
     *
     * @return true if the text box is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isYCoordinateTextBoxDisplayed() {
        return isElementDisplayed(getYCoordinateTextBox());
    }

    /**
     * Checks to see if the z coordinate text box is displayed
     *
     * @return true if the text box is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isZCoordinateTextBoxDisplayed() {
        return isElementDisplayed(getZCoordinateTextBox());
    }

    /**
     * Checks to see if the given text box is displayed
     *
     * @param textBox -the text box expected
     * @return true if the text box is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isTextBoxDisplayed(CreateFixedCellularTextBox textBox) {
        return isElementDisplayed(getTextBox(textBox));
    }

    /**
     * Checks if Name required error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isNameRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(CreateFixedCellularTextBox.NAME.label);
    }

    /**
     * Checks if leading/trailing whitespace error is displayed associated with
     * Name text box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isLeadingTrailingWhitespaceNameErrorDisplayed() {
        return isWhitespaceErrorDisplayed(CreateFixedCellularTextBox.NAME.label);
    }

    /**
     * Checks if invalid name error is displayed associated with the Name text
     * box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidFixedCellNameErrorDisplayed(){
        return isInvalidNameErrorDisplayed(CreateFixedCellularTextBox.NAME.label);
    }

    /**
     * Checks if duplicate device name error is displayed
     *
     * @param device -the duplicated device
     * @return true if displayed, false otherwise
     */
    public boolean isDuplicateDeviceNameErrorDisplayed(FixedCellularDevice device) {
        String name = device.getName();
        return isDuplicateDeviceNameErrorDisplayed(name);
    }

    /**
     * Checks if MAC Address required error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isMACAddressRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(CreateFixedCellularTextBox.MAC_ADDRESS.label);
    }

    /**
     * Checks if non-numeric error is displayed associated with MAC Address text
     * box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isNonNumericMACAddressErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(CreateFixedCellularTextBox.MAC_ADDRESS.label);
    }

    /**
     * Checks if duplicate MAC address error is displayed
     *
     * @param device the duplicated device
     * @return true if displayed, false otherwise
     */
    public boolean isDuplicateMACAddressErrorDisplayed(FixedCellularDevice device) {
        String mac = device.getMacAddress();
        return isErrorToolTipDisplayed(DUPLICATE_MAC_ADDRESS_ERROR_TEXT_PREFIX + mac + DUPLICATE_MAC_ADDRESS_ERROR_TEXT_SUFFIX);
    }

    /**
     * Checks if X Coordinate required error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isXCoordinateRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(CreateFixedCellularTextBox.X_COORDINATE.label);
    }

    /**
     * Checks if non-numeric error is displayed associated with X Coordinate
     * text box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isNonNumericXCoordinateErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(CreateFixedCellularTextBox.X_COORDINATE.label);
    }

    /**
     * Checks if Y Coordinate required error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isYCoordinateRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(CreateFixedCellularTextBox.Y_COORDINATE.label);
    }

    /**
     * Checks if non-numeric error is displayed associated with Y Coordinate
     * text box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isNonNumericYCoordinateErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(CreateFixedCellularTextBox.Y_COORDINATE.label);
    }

    /**
     * Checks if Z Coordinate required error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isZCoordinateRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(CreateFixedCellularTextBox.Z_COORDINATE.label);
    }

    /**
     * Checks if non-numeric error is displayed associated with Z Coordinate
     * text box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isNonNumericZCoordinateErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(CreateFixedCellularTextBox.Z_COORDINATE.label);
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Clicks the Create Fixed Cellular help icon
     */
    public void clickHelpIcon() {
        getHelpIcon(CREATE_FIXED_CELLULAR_DEVICE_HEADER_TEXT).click();
    }

    /**
     * Sets name from given fixed cellular device in fixed cellular device name
     * text box
     *
     * @param device -the fixed cellular Device
     */
    public void setFixedCellularDeviceName(FixedCellularDevice device) {
        String name = device.getName();
        setName(name);
    }

    /**
     * Sets given fixed cellular device name in Fixed Cellular Device Name text
     * box
     *
     * @param name -the name to set
     */
    public void setFixedCellularDeviceName(String name) {
        getTextBox(CreateFixedCellularTextBox.NAME).setText(name);
    }

    /**
     * Sets mac address from given fixed cellular device in mac address text box
     *
     * @param device -the fixed cellular Device
     */
    public void setMacAddress(FixedCellularDevice device) {
        String mac = device.getMacAddress();
        setMacAddress(mac);
    }

    /**
     * Sets given fixed cellular device mac address in Mac Address text box
     *
     * @param mac -the mac address to set
     */
    public void setMacAddress(String mac) {
        getMacAddressTextBox().setText(mac);
    }

    /**
     * Sets x coordinate from given fixed cellular device in x coordinate text
     * box
     *
     * @param device -the fixed cellular Device
     */
    public void setXCoordinate(FixedCellularDevice device) {
        String x = device.getXCoordinate();
        setXCoordinate(x);
    }

    /**
     * Sets given x coordinate in X Coordinate text box
     *
     * @param x -the x coordinate to set
     */
    public void setXCoordinate(String x) {
        getXCoordinateTextBox().setText(x);
    }

    /**
     * Sets y coordinate from given fixed cellular device in y coordinate text
     * box
     *
     * @param device -the fixed cellular Device
     */
    public void setYCoordinate(FixedCellularDevice device) {
        String y = device.getYCoordinate();
        setYCoordinate(y);
    }

    /**
     * Sets given y coordinate in Y Coordinate text box
     *
     * @param y -the y coordinate to set
     */
    public void setYCoordinate(String y) {
        getYCoordinateTextBox().setText(y);
    }

    /**
     * Sets z coordinate from given fixed cellular device in z coordinate text
     * box
     *
     * @param device -the fixed cellular Device
     */
    public void setZCoordinate(FixedCellularDevice device) {
        String z = device.getZCoordinate();
        setZCoordinate(z);
    }

    /**
     * Sets given z coordinate in z Coordinate text box
     *
     * @param z -the z coordinate to set
     */
    public void setZCoordinate(String z) {
        getZCoordinateTextBox().setText(z);
    }

    /**
     * Sets name value from given fixed cellular device in name text box
     *
     * @param device -the fixed cellular device
     */
    public void setName(FixedCellularDevice device) {
        String text = device.getName();
        setName(text);
    }

    /**
     * Sets given text in Name text box
     *
     * @param text -the text to set
     */
    public void setName(String text) {
        getTextBox(CreateFixedCellularTextBox.NAME).setText(text);
    }

    /**
     * Sets given z coordinate in Z Coordinate text box
     *
     * @param z -the z coordinate to set
     */
    public void setZCoordinate(int z) {
        String zStr = Integer.toString(z);
        getZCoordinateTextBox().setText(zStr);
    }

    /**
     * Click the Create button
     *
     * @return the newly loaded Configure Devices Form
     */
    public SimulationSettingsModal clickCreate() {
        getBtn(BtnNames.CREATE).click();
        waitForElementToBeInvisible(By.xpath(TITLE_XPATH_PREFIX + CREATE_FIXED_CELLULAR_DEVICE_HEADER_TEXT + "')]"));
        return getPage(SimulationSettingsModal.class);
    }

    /**
     * Clicks the Close icon in the Create Fixed Cellular header
     */
    public void clickCloseIcon() {
        clickCloseIcon(CREATE_FIXED_CELLULAR_DEVICE_HEADER_TEXT);
    }

    /**
     * Click the Create button when errors are expected
     */
    public void clickCreateNoRtrn() {
        getBtn(BtnNames.CREATE).click();
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
     * @return the newly loaded Configure Devices Form
     */
    public SimulationSettingsModal clickCancel() {
        getBtn(BtnNames.CANCEL).click();
        return getPage(SimulationSettingsModal.class);
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(CREATAE_FIXED_CELLULAR_DEVICE_HELP_HEADER_TEXT);
    }

    /**
     * Click the Create button
     *
     * @param success -true if success is expected, false if error expected
     * @return the newly loaded Configure Fixed Cellular Device Partial Page (if
     *         succes)
     */
    public ConfigureFixedCellularDevicePartialPage clickCreate(boolean success) {
        getBtn(BtnNames.CREATE).click();
        if (success) {
            waitForElementToBeInvisible(By.xpath(TITLE_XPATH_PREFIX + CREATE_FIXED_CELLULAR_DEVICE_HEADER_TEXT + "']"));
            return getPage(ConfigureFixedCellularDevicePartialPage.class);
        }
        else {
            return null;
        }
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkAddFixedCellularDeviceHeaderIcons() {
        checkHeaderIcons(CREATE_FIXED_CELLULAR_DEVICE_HEADER_TEXT);
    }

    /**
     * Checks for the presence of the Create, Reset, and Cancel buttons
     */
    public void checkBtns() {
        Assert.assertTrue("The Create button was not found.", isCreateBtnDisplayed());
        Assert.assertTrue("The Reset button was not found.", isResetBtnDisplayed());
        Assert.assertTrue("The Cancel button was not found.", isCancelBtnDisplayed());
    }

    /**
     * Checks for presence of Fixed Cellular Device Name, Mac Address, X
     * Coordinate, Y Coordinate, and Z Coordinate text boxes
     */
    public void checkFieldsDisplayed() {
        Assert.assertTrue("Cellular Device Name text box was not found.", isTextBoxDisplayed(CreateFixedCellularTextBox.NAME));
        Assert.assertTrue("Mac Address text box was not found.", isTextBoxDisplayed(CreateFixedCellularTextBox.MAC_ADDRESS));
        Assert.assertTrue("X Coordinate text box was not found.", isTextBoxDisplayed(CreateFixedCellularTextBox.X_COORDINATE));
        Assert.assertTrue("Y Coordinate text box was not found.", isTextBoxDisplayed(CreateFixedCellularTextBox.Y_COORDINATE));
        Assert.assertTrue("Z Coordinate text box was not found.", isTextBoxDisplayed(CreateFixedCellularTextBox.Z_COORDINATE));
    }

    /**
     * Sets values from given cellular device into Cellular Device Name, Mac
     * Address, X Coordinate, Y Coordinate, and Z Coordinate text boxes
     *
     * @param deviceName -the fixed cellular device name to set
     * @param mac -the mac address to set
     * @param x -the x coordinate to set
     * @param y -the y coordinate to set
     * @param z -the z coordinate to set
     */
    public void setAllFields(String deviceName, String mac, String x, String y, String z) {
        setFixedCellularDeviceName(deviceName);
        setMacAddress(mac);
        setXCoordinate(x);
        setYCoordinate(y);
        setZCoordinate(z);
    }

    /**
     * Sets values from given cellular device into Cellular DeviceName, Mac
     * Address, X Coordinate, Y Coordinate, and Z Coordinate text boxes
     *
     * @param device -the fixed cellular device to set
     */
    public void setAllFields(FixedCellularDevice device) {
        setFixedCellularDeviceName(device);
        setMacAddress(device);
        setXCoordinate(device);
        setYCoordinate(device);
        setZCoordinate(device);
    }

    /**
     * Checks values displayed in all fields and verifies they match the values
     * for the given fixed cellular device
     *
     * @param device -the device expected
     */
    public void checkFieldValues(FixedCellularDevice device) {
        String name = device.getName();
        String mac = device.getMacAddress();
        String x = device.getXCoordinate();
        String y = device.getYCoordinate();
        String z = device.getZCoordinate();
        checkFieldValues(name, mac, x, y, z);

    }

    /**
     * Checks that help modal is displayed -- checks both the header and content
     * text
     */
    public void checkHelpModal() {
        Assert.assertTrue("Create Fixed Cellular Device Help header not displayed as expected.", isCreateFixedCellularDeviceHelpHeaderDisplayed());
        Assert.assertTrue("Create Fixed Cellular Device Help content not displayed as expected.", isCreateFixedCellularDeviceHelpContentDisplayed());
    }

    /**
     * Verifies 'required field' error displays for all fields (checks Name, X
     * Coordinate, Y Coordinate, and Z Coordinate text boxes)
     */
    public void checkRequiredFieldErrorAllFields() {
        Assert.assertTrue("Required field error is not displayed for the RSE Device Name field.", isFieldRequiredErrorDisplayed(CreateFixedCellularTextBox.NAME.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the MAC address field.", isFieldRequiredErrorDisplayed(CreateFixedCellularTextBox.MAC_ADDRESS.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the X Coordinate field.", isFieldRequiredErrorDisplayed(CreateFixedCellularTextBox.X_COORDINATE.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Y Coordinate field.", isFieldRequiredErrorDisplayed(CreateFixedCellularTextBox.Y_COORDINATE.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Z Coordinate field.", isFieldRequiredErrorDisplayed(CreateFixedCellularTextBox.Z_COORDINATE.getLabel()));
    }

    /**
     * Checks values displayed in all fields and verifies they match the given
     * string values
     *
     * @param deviceName -the fixed cellular device name expected
     * @param mac -the mac address expected
     * @param x -the x coordinate value expected as string
     * @param y -the y coordinate value expected as string
     * @param z -the z coordinate value expected as string
     */
    public void checkFieldValues(String deviceName, String mac, String x, String y, String z) {
        Assert.assertEquals("The cellular device name value," + deviceName + ", is not displayed as expected.", deviceName, getTextBoxValue(CreateFixedCellularTextBox.NAME));
        Assert.assertEquals("The mac address value," + mac + ", is not displayed as expected.", mac, getTextBoxValue(CreateFixedCellularTextBox.MAC_ADDRESS));
        Assert.assertEquals("The x coordinate value," + x + ", is not displayed as expected.", x, getTextBoxValue(CreateFixedCellularTextBox.X_COORDINATE));
        Assert.assertEquals("The y coordinate value," + y + ", is not displayed as expected.", y, getTextBoxValue(CreateFixedCellularTextBox.Y_COORDINATE));
        Assert.assertEquals("The z coordinate value," + z + ", is not displayed as expected.", z, getTextBoxValue(CreateFixedCellularTextBox.Z_COORDINATE));
    }

    /**
     * Checks that all fields are reset to default values
     */
    public void checkFieldsReset() {
        checkFieldValues("", "", "", "", "");
    }

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkCreateFixedCellularHeaderIcons() {
        checkHeaderIcons(CREATE_FIXED_CELLULAR_DEVICE_HEADER_TEXT);
    }
}