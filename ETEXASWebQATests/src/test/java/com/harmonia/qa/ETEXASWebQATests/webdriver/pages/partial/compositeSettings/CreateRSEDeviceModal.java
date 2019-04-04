package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.RSEDevice;
import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the add RSE device form
 *
 * @author llaroussini
 */
public class CreateRSEDeviceModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public CreateRSEDeviceModal(WebDriver driver) {
        super(driver);
    }

    //////////////////
    // Enumerations
    /////////////////

    /**
     * Enumeration of Add Cell Tower text boxes
     *
     * @author llaroussini
     */
    public enum CreateRSETextBox {
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
        Z_COORDINATE("Z (cm):", "z");

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
        CreateRSETextBox(String label, String name) {
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
     * Text displayed in the create RSE header
     */
    private static final String CREATE_RSE_HEADER_TEXT = "Create RSE Device";

    /**
     * The xpath prefix to all text boxes in the Create RSE modal
     */
    private static final String TEXT_BOX_XPATH_PREFIX = "//input[@name='";

    /**
     * Xpath prefix to buttons in Create RSE modal
     */
    private static final String CREATE_RSE_BTN_XPATH_PREFIX = "//a[contains(@id, 'rse')][contains(@id, 'Create')]//span[contains(@id, 'button')][text()='";

    /**
     * Error text displayed when invalid name is used
     */
    private static final String INVALID_RSE_NAME_TEXT = "Device names may contain only letters, digits, hyphens, underscores, and nonconsecutive spaces.";

    /**
     * Text displayed in the create RSE help header
     */
    private static final String CREATE_RSE_HELP_HEADER_TEXT = "Create RSE Device Help";

    /**
     * Text displayed as create RSE help content
     */
    private static final String CREATE_RSE_HELP_CONTENT_TEXT = "Create a new Road Side Equipment (RSE) device with the specified name and coordinates. The device name must be unique in the composite. The x (cm), y (cm), and z (cm) coordinates are specified relative to the center point of the simulation.";

    ///////////
    //Getters
    ///////////

    /**
     * Gets the given button in the Create RSE modal
     *
     * @param btn - the button to get
     * @return the button element
     */
    private El getBtn(BtnNames btn) {
        return el(By.xpath(CREATE_RSE_BTN_XPATH_PREFIX + btn.getLabel() + "']"));
    }

    /**
     * Gets the given text box in the Create RSE modal
     *
     * @param textBox -the specific text box to get
     * @return the text box element
     */
    private El getTextBox(CreateRSETextBox textBox) {
        return el(By.xpath(TEXT_BOX_XPATH_PREFIX + textBox.getName() + "']"));
    }

    /**
     * Gets the value displayed in the given text box in the Create RSE modal
     *
     * @param textBox -the specific text box from which to get the value
     * @return the displayed value in the given text box
     */
    private String getTextBoxValue(CreateRSETextBox textBox) {
        return getTextBox(textBox).getAttribute("value");
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the create RSE header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCreateRSEHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_RSE_HEADER_TEXT);
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
    public boolean isTextBoxDisplayed(CreateRSETextBox textBox) {
        return isElementDisplayed(getTextBox(textBox));
    }

    /**
     * Checks if Name required error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isNameRequiredErrorDisplayed(){
        return isFieldRequiredErrorDisplayed(CreateRSETextBox.NAME.getLabel());
    }

    /**
     * Checks to see if RSE Name Whitespace error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isRSENameWhitespaceErrorDisplayed() {
        return isWhitespaceErrorDisplayed(CreateRSETextBox.NAME.getLabel());
    }

    /**
     * Checks to see if Invalid RSE Name error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidRSENameErrorDisplayed() {
        return isErrorToolTipDisplayed(INVALID_RSE_NAME_TEXT);
    }

    /**
     * Checks if duplicate device error is displayed
     *
     * @param device -the duplicated device
     * @return true if displayed, false otherwise
     */
    public boolean isDuplicateRSENameErrorDisplayed(RSEDevice device) {
        String name = device.getName();
        return isDuplicateDeviceNameErrorDisplayed(name);
    }

    /**
     * Checks if X Coordinate required error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isXCoordinateRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(CreateRSETextBox.X_COORDINATE.getLabel());
    }

    /**
     * Checks to see if Invalid X Coordinate error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidXCoordinateErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(CreateRSETextBox.X_COORDINATE.getLabel());
    }

    /**
     * Checks if Y Coordinate required error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isYCoordinateRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(CreateRSETextBox.Y_COORDINATE.getLabel());
    }

    /**
     * Checks to see if Invalid Y Coordinate error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidYCoordinateErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(CreateRSETextBox.Y_COORDINATE.getLabel());
    }

    /**
     * Checks if Z Coordinate required error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isZCoordinateRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(CreateRSETextBox.Z_COORDINATE.getLabel());
    }

    /**
     * Checks to see if Invalid Z Coordinate error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidZCoordinateErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(CreateRSETextBox.Z_COORDINATE.getLabel());
    }

    /**
     * Checks to see if the Create RSE help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCreateRSEHelpHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_RSE_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the Create RSE help content is displayed
     *
     * @return true if the content text is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCreateRSEHelpContentDisplayed() {
        return isContentDisplayed(CREATE_RSE_HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in the help window
     *
     * @return true if the ok button is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(CREATE_RSE_HELP_HEADER_TEXT);
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Sets name value from given RSE device in name text box
     *
     * @param rse -the RSE device
     */
    public void setName(RSEDevice rse) {
        String text = rse.getName();
        setName(text);
    }

    /**
     * Sets given text in Name text box
     *
     * @param text -the text to set
     */
    public void setName(String text) {
        getTextBox(CreateRSETextBox.NAME).setText(text);
    }

    /**
     * Sets x coordinate from given RSE device in x coordinate text box
     *
     * @param rse -the RSE device
     */
    public void setXCoordinate(RSEDevice rse) {
        String x = rse.getXCoordinate();
        setXCoordinate(x);
    }

    /**
     * Sets given text in X Coordinate text box
     *
     * @param x -the coordinate to set
     */
    public void setXCoordinate(String x) {
        getTextBox(CreateRSETextBox.X_COORDINATE).setText(x);
    }

    /**
     * Sets y coordinate from given RSE device in y coordinate text box
     *
     * @param rse -the RSE device
     */
    public void setYCoordinate(RSEDevice rse) {
        String y = rse.getYCoordinate();
        setYCoordinate(y);
    }

    /**
     * Sets given text in Y Coordinate text box
     *
     * @param y -the coordinate to set
     */
    public void setYCoordinate(String y) {
        getTextBox(CreateRSETextBox.Y_COORDINATE).setText(y);
    }

    /**
     * Sets z coordinate from given RSE device in z coordinate text box
     *
     * @param rse -the RSE device
     */
    public void setZCoordinate(RSEDevice rse) {
        String z = rse.getZCoordinate();
        setZCoordinate(z);
    }

    /**
     * Sets given text in Z Coordinate text box
     *
     * @param z -the coordinate to set
     */
    public void setZCoordinate(String z) {
        getTextBox(CreateRSETextBox.Z_COORDINATE).setText(z);
    }

    /**
     * Click the Create button when error is expected
     */
    public void clickCreateErrorExpected() {
        getBtn(BtnNames.CREATE).click();
    }

    /**
     * Click the Create button
     *
     * @param success -true if success is expected, false if error expected
     * @return Configure RSE Devices Partial Page if success, otherwise return
     *         null
     */
    public ConfigureRSEDevicesPartialPage clickCreate(boolean success) {
        getBtn(BtnNames.CREATE).click();
        if (success) {
            waitForElementToBeInvisible(By.xpath(CREATE_RSE_BTN_XPATH_PREFIX));
            return getPage(ConfigureRSEDevicesPartialPage.class);
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
     * @return the newly loaded Configure RSE Devices Partial Page
     */
    public ConfigureRSEDevicesPartialPage clickCancel() {
        getBtn(BtnNames.CANCEL).click();
        return getPage(ConfigureRSEDevicesPartialPage.class);
    }

    /**
     * Clicks the Create RSE help icon
     */
    public void clickHelpIcon() {
        getHelpIcon(CREATE_RSE_HEADER_TEXT).click();
    }

    /**
     * Clicks the Close icon in the Create RSE header
     */
    public void clickCloseIcon() {
        clickCloseIcon(CREATE_RSE_HEADER_TEXT);
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(CREATE_RSE_HELP_HEADER_TEXT);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkCreateRSEHeaderIcons() {
        checkHeaderIcons(CREATE_RSE_HEADER_TEXT);
    }

    /**
     * Checks that help modal is displayed -- checks both the header and content
     * text
     */
    public void checkHelpModal() {
        Assert.assertTrue("Create RSE Help header not displayed as expected.", isCreateRSEHelpHeaderDisplayed());
        Assert.assertTrue("Create RSE Help content not displayed as expected.", isCreateRSEHelpContentDisplayed());
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
     * Checks for presence of Name, X Coordinate, Y Coordinate, and Z Coordinate
     * text boxes
     */
    public void checkFieldsDisplayed() {
        Assert.assertTrue("Name text box was not found.", isTextBoxDisplayed(CreateRSETextBox.NAME));
        Assert.assertTrue("X Coordinate text box was not found.", isTextBoxDisplayed(CreateRSETextBox.X_COORDINATE));
        Assert.assertTrue("Y Coordinate text box was not found.", isTextBoxDisplayed(CreateRSETextBox.Y_COORDINATE));
        Assert.assertTrue("Z Coordinate text box was not found.", isTextBoxDisplayed(CreateRSETextBox.Z_COORDINATE));
    }

    /**
     * Sets values from given RSE device into Name, X Coordinate, Y Coordinate,
     * and Z Coordinate text boxes
     *
     * @param rse -the RSE device to set
     */
    public void setAllFields(RSEDevice rse) {
        setName(rse);
        setXCoordinate(rse);
        setYCoordinate(rse);
        setZCoordinate(rse);
    }

    /**
     * Sets all given values into Name, X Coordinate, Y Coordinate, and Z
     * Coordinate text boxes
     *
     * @param name -the name of the RSE device
     * @param x -the x coordinate
     * @param y -the y coordinate
     * @param z -the z coordinate
     */
    public void setAllFields(String name, String x, String y, String z) {
        setName(name);
        setXCoordinate(x);
        setYCoordinate(y);
        setZCoordinate(z);
    }

    /**
     * Checks values displayed in all fields and verifies they match the values
     * for the given RSE device
     *
     * @param rse -the RSE device expected
     */
    public void checkFieldValues(RSEDevice rse) {
        String name = rse.getName();
        String x = rse.getXCoordinate();
        String y = rse.getYCoordinate();
        String z = rse.getZCoordinate();
        checkFieldValues(name, x, y, z);

    }

    /**
     * Checks values displayed in all fields and verifies they match the given
     * string values
     *
     * @param name -the RSE device name expected
     * @param x -the x coordinate expected
     * @param y -the y coordinate expected
     * @param z -the z coordinate expected
     */
    public void checkFieldValues(String name, String x, String y, String z) {
        Assert.assertEquals("The RSE device name value," + name + ", is not displayed as expected.", name, getTextBoxValue(CreateRSETextBox.NAME));
        Assert.assertEquals("The RSE x coordinate value," + x + ", is not displayed as expected.", x, getTextBoxValue(CreateRSETextBox.X_COORDINATE));
        Assert.assertEquals("The RSE y coordinate value," + y + ", is not displayed as expected.", y, getTextBoxValue(CreateRSETextBox.Y_COORDINATE));
        Assert.assertEquals("The RSE z coordinate value," + z + ", is not displayed as expected.", z, getTextBoxValue(CreateRSETextBox.Z_COORDINATE));
    }

    /**
     * Verifies 'required field' error displays for all fields (checks Name, X
     * Coordinate, Y Coordinate, and Z Coordinate text boxes)
     */
    public void checkRequiredFieldErrorAllFields() {
        Assert.assertTrue("Required field error is not displayed for the RSE Device Name field.", isFieldRequiredErrorDisplayed(CreateRSETextBox.NAME.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the X Coordinate field.", isFieldRequiredErrorDisplayed(CreateRSETextBox.X_COORDINATE.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Y Coordinate field.", isFieldRequiredErrorDisplayed(CreateRSETextBox.Y_COORDINATE.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Z Coordinate field.", isFieldRequiredErrorDisplayed(CreateRSETextBox.Z_COORDINATE.getLabel()));
    }
}
