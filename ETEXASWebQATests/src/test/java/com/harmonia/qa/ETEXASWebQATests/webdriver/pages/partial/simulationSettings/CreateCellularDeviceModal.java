package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.CellularDevice;
import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeSettingsModal;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the Create cellular device form
 *
 * @author llaroussini
 */
public class CreateCellularDeviceModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver
     */
    public CreateCellularDeviceModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    /**
     * Enumeration of Create Cellular Device text boxes
     *
     * @author llaroussini
     */
    public enum CreateCellDeviceTextBox {
        /**
         * Name text box
         */
        NAME("Name:", "deviceName"),
        /**
         * Min Quantity text box
         */
        MIN_QUANTITY("Min Quantity:", "minQuantity"),
        /**
         * Max Quantity text box
         */
        MAX_QUANTITY("Max Quantity:", "maxQuantity"),
        /**
         * Percentage text box
         */
        PERCENTAGE("Percentage:", "percentage");

        /**
         * The label of the text box as appears in the UI
         */
        private String label;

        /**
         * The name attribute of the text box
         */
        private String name;

        /**
         * Default constructor; sets the label
         *
         * @param label -the label of the text box as appears in the UI
         * @param name -the name attribute of the text box
         */
        CreateCellDeviceTextBox(String label, String name) {
            this.label = label;
            this.name = name;
        }

        /**
         * Gets the label of the text box as appears in the UI
         *
         * @return The label of the text box
         */
        public String getLabel() {
            return this.label;
        }

        /**
         * Gets the name attribute of the text box
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
     * Text displayed in the Create cellular device header
     */
    private static final String CREATE_CELLULAR_DEVICE_HEADER_TEXT = "Create Cellular Device Profile";

    /**
     * Text displayed in the Create cellular device help header
     */
    private static final String CREATE_CELLULAR_DEVICE_HELP_HEADER_TEXT = "Create Cellular Device Profile Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Create a new cellular device profile with the specified name, minimum number of cellular devices per affected vehicle, maximum number of cellular devices per affected vehicle, and percentage of affected vehicles. The device profile name must be unique in the composite and the total percentage for all cellular device profiles cannot exceed 100 percent.";

    /**
     * Xpath prefix for the input fields
     */
    private static final String CELLULAR_DEVICE_INPUT_PREFIX = "//input[contains(@id, 'ETexas-device-view-cellular-Create')][@name='";

    /**
     * The xpath prefix for all text boxes in the Create Cellular Device modal
     */
    private static final String CREATE_CELL_DEVICE_TEXT_BOX_XPATH_PREFIX = "//input[@name='";

    /**
     * Xpath prefix to create device buttons
     */
    private static final String CREATE_CELLULAR_DEVICE_BTN_XPATH_PREFIX = "//span[contains(@id, 'ETexas-device-view-cellular-Create')][text()= '";

    /**
     * Xpath for ancestor anchor
     */
    private static final String ANCESTOR_A_XPATH = "/ancestor::a";

    /**
     * Error text displayed when an invalid device name is used
     */
    private static final String INVALID_DEVICE_NAME_ERROR_TEXT = "Device profile names may contain only letters, digits, hyphens, underscores, and nonconsecutive spaces.";

    /**
     * Error text displayed when value is used in Min Quantity text box that is
     * outside the accepted range
     */
    private static final String INVALID_MIN_QUANTITY_RANGE_ERROR_TEXT = "The minimum number of cellular devices per vehicle must be in the range of 0 to 8 devices.";

    /**
     * Error text displayed when value is used in Max Quantity text box that is
     * outside the accepted range
     */
    private static final String INVALID_MAX_QUANTITY_RANGE_ERROR_TEXT = "The maximum number of cellular devices per vehicle must be in the range of 0 to 8 devices.";

    /**
     * Error text displayed when Min/Max range is invalid (max quantity is less
     * than min quantity)
     */
    private static final String INVALID_OVERALL_QUANTITY_RANGE_ERROR_TEXT = "The minimum number of cellular devices per vehicle cannot exceed the maximum number of cellular devices per vehicle.";

    /**
     * Error text displayed when value is used in Percentage text box that is
     * outside the accepted range
     */
    private static final String INVALID_PRECENTAGE_RANGE_ERROR_TEXT = "Cellular device profile percentages must be in the range of 1 to 100 percent.";

    /**
     * Error text displayed when combined percentage total of all cellular
     * devices exceeds 100
     */
    private static final String COMBINED_PERCENTAGE_EXCEEDS_MAX_ERROR_TEXT = "The total for all cellular device profile percentages cannot exceed 100 percent.";

    ///////////
    //Getters
    ///////////

    /**
     * The xpath to get from the form content window to the help icon
     *
     * @return the help icon
     */
    private El getCreateCellularDeviceHelpIcon() {
        return getHelpIcon(CREATE_CELLULAR_DEVICE_HEADER_TEXT);
    }

    /**
     * Gets the given button in the Create cellular device window
     *
     * @param btn - the button to get
     * @return the button element
     */
    private El getBtn(BtnNames btn) {
        return el(By.xpath(CREATE_CELLULAR_DEVICE_BTN_XPATH_PREFIX + btn.getLabel() + "']" + ANCESTOR_A_XPATH));
    }

    /**
     * Gets the El input text box for the given input (Name, Min Quantity, Max
     * Quantity, Percentage)
     *
     * @param inputName - the String name of the text box element to return
     * @return the text box El
     */
    private El getCellularInputTextBox(String inputName) {
        return el(By.xpath(CELLULAR_DEVICE_INPUT_PREFIX + inputName + "']"));
    }

    /**
     * Gets the Cellular Device Name text box
     *
     * @return the text box element
     */
    private El getCellularNameTextBox() {
        return el(By.xpath(CREATE_CELL_DEVICE_TEXT_BOX_XPATH_PREFIX + CreateCellDeviceTextBox.NAME.name + "']"));
    }

    /**
     * Gets the Rule Percentage text box
     *
     * @return the text box element
     */
    private El getPercentageTextBox() {
        return el(By.xpath(CREATE_CELL_DEVICE_TEXT_BOX_XPATH_PREFIX + CreateCellDeviceTextBox.PERCENTAGE.name + "']"));
    }

    /**
     * Gets the Min Number of Cells per Vehicle text box
     *
     * @return the text box element
     */
    private El getMinCellsPerVehicleTextBox() {
        return el(By.xpath(CREATE_CELL_DEVICE_TEXT_BOX_XPATH_PREFIX + CreateCellDeviceTextBox.MIN_QUANTITY.name + "']"));
    }

    /**
     * Gets the Max Number of Cells per Vehicle text box
     *
     * @return the text box element
     */
    private El getMaxCellsPerVehicleTextBox() {
        return el(By.xpath(CREATE_CELL_DEVICE_TEXT_BOX_XPATH_PREFIX + CreateCellDeviceTextBox.MAX_QUANTITY.name + "']"));
    }

    /**
     * Gets the value in the cellular device name text box
     *
     * @return the displayed Name value
     */
    public String getDisplayedCellularDeviceName() {
        return getCellularNameTextBox().getAttribute("value");
    }

    /**
     * Gets the value in the rule percentage text box
     *
     * @return the displayed percentage value
     */
    public String getDisplayedPercentage() {
        return getPercentageTextBox().getAttribute("value");
    }

    /**
     * Gets the value in the min cells per vehicle text box
     *
     * @return the displayed min cells per vehicle value
     */
    public String getDisplayedMinCellsPerVehicle() {
        return getMinCellsPerVehicleTextBox().getAttribute("value");
    }

    /**
     * Gets the value in the max cells per vehicle text box
     *
     * @return the displayed max cells per vehicle value
     */
    public String getDisplayedMaxCellsPerVehicle() {
        return getMaxCellsPerVehicleTextBox().getAttribute("value");
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the Create cellular device header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCreateCellularDeviceHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_CELLULAR_DEVICE_HEADER_TEXT);
    }

    /**
     * Checks to see if the Create cellular device help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCreateCellularDeviceHelpHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_CELLULAR_DEVICE_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isCreateCellularDeviceHelpIconDisplayed() {
        return isHelpIconDisplayed(CREATE_CELLULAR_DEVICE_HEADER_TEXT);
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
        return isHelpOKBtnDisplayed(CREATE_CELLULAR_DEVICE_HELP_HEADER_TEXT);
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
     * Checks to see if the text box of the given input name is displayed
     *
     * @param inputName - the String name of the text box being checked
     * @return true if the element is displayed, otherwise false
     */
    public boolean isTextBoxDisplayed(String inputName) {
        return isElementDisplayed(getCellularInputTextBox(inputName));
    }

    /**
     * Checks to see if the cellular device name text box is displayed
     *
     * @return true if the text box is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isCellularNameTextBoxDisplayed() {
        return isElementDisplayed(getCellularNameTextBox());
    }


    /**
     * Checks to see if the rule percentage text box is displayed
     *
     * @return true if the text box is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isPercentageTextBoxDisplayed() {
        return isElementDisplayed(getPercentageTextBox());
    }

    /**
     * Checks to see if the min cells per vehicle text box is displayed
     *
     * @return true if the text box is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isMinCellsPerVehicleTextBoxDisplayed() {
        return isElementDisplayed(getMinCellsPerVehicleTextBox());
    }

    /**
     * Checks to see if the max cells per vehicle text box is displayed
     *
     * @return true if the text box is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isMaxCellsPerVehicleTextBoxDisplayed() {
        return isElementDisplayed(getMaxCellsPerVehicleTextBox());
    }

    /**
     * Checks if field required error is displayed with the Name text box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isNameFieldRequiredErrorDisplayed(){
        return isFieldRequiredErrorDisplayed(CreateCellDeviceTextBox.NAME.label);
    }

    /**
     * Checks if leading/trailing whitespace error is displayed with the Name
     * text box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isNameLeadingTrailingWhitespaceErrorDisplayed() {
        return isWhitespaceErrorDisplayed(CreateCellDeviceTextBox.NAME.label);
    }

    /**
     * Checks if invalid name error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidDeviceNameErrorDisplayed() {
        return isErrorToolTipDisplayed(INVALID_DEVICE_NAME_ERROR_TEXT);
    }

    /**
     * Checks if duplicate device error is displayed
     *
     * @param device -the duplicated device
     * @return true if error displayed, false otherwise
     */
    public boolean isDuplicateDeviceErrorDisplayed(CellularDevice device) {
        String name = device.getName();
        return isDuplicateDeviceNameErrorDisplayed(name);
    }

    /**
     * Checks if field required error is displayed with the Min Quantity text
     * box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isMinQuantityFieldRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(CreateCellDeviceTextBox.MIN_QUANTITY.label);
    }

    /**
     * Checks if non-numeric error is displayed with Min Quantity text box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isNonNumericMinQuantityErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(CreateCellDeviceTextBox.MIN_QUANTITY.label);
    }

    /**
     * Checks if Min Quantity invalid range error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isMinQuantityInvalidRangeErrorDisplayed() {
        return isErrorToolTipDisplayed(INVALID_MIN_QUANTITY_RANGE_ERROR_TEXT);
    }

    /**
     * Checks if field required error is displayed with the Max Quantity text
     * box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isMaxQuantityFieldRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(CreateCellDeviceTextBox.MAX_QUANTITY.label);
    }

    /**
     * Checks if non-numeric error is displayed with Max Quantity text box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isNonNumericMaxQuantityErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(CreateCellDeviceTextBox.MAX_QUANTITY.label);
    }

    /**
     * Checks if Max Quantity invalid range error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isMaxQuantityInvalidRangeErrorDisplayed() {
        return isErrorToolTipDisplayed(INVALID_MAX_QUANTITY_RANGE_ERROR_TEXT);
    }

    /**
     * Checks if overall invalid min/max range error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidMinMaxRangeErrorDisplayed() {
        return isErrorToolTipDisplayed(INVALID_OVERALL_QUANTITY_RANGE_ERROR_TEXT);
    }

    /**
     * Checks if field required error is displayed with the Percentage text box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isPercentageFieldRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(CreateCellDeviceTextBox.PERCENTAGE.label);
    }

    /**
     * Checks if non-numeric error is displayed with Percentage text box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isNonNumericPercentageErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(CreateCellDeviceTextBox.PERCENTAGE.label);
    }

    /**
     * Checks if invalid range error is displayed with Percentage text box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidPercentageRangeErrorDisplayed() {
        return isErrorToolTipDisplayed(INVALID_PRECENTAGE_RANGE_ERROR_TEXT);
    }

    /**
     * Checks if combined percentage exceeds max error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isCombinedPercentageExceedsMaxErrorText() {
        return isErrorToolTipDisplayed(COMBINED_PERCENTAGE_EXCEEDS_MAX_ERROR_TEXT);
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Click the Create cellular device Help icon
     */
    public void clickCreateCellularDeviceHelp() {
        getCreateCellularDeviceHelpIcon().click();
    }

    /**
     * Click the Close icon
     */
    public void clickCloseIcon() {
        getCloseIcon(CREATE_CELLULAR_DEVICE_HEADER_TEXT).click();
    }

    /**
     * Sets name from given cellular device in cellular device name text box
     *
     * @param device -the cellular Device
     */
    public void setCellularDeviceName(CellularDevice device) {
        String name = device.getName();
        setCellularDeviceName(name);
    }

    /**
     * Sets given cellular device name in Cellular Device Name text box
     *
     * @param name -the name to set
     */
    public void setCellularDeviceName(String name) {
        getCellularNameTextBox().setText(name);
    }

    /**
     * Sets percentage from given cellular device in rule percentage text box
     *
     * @param device -the cellular device
     */
    public void setCellularPercentage(CellularDevice device) {
        String percent = device.getCellularPercent();
        setCellularPercentage(percent);
    }

    /**
     * Sets given percentage in rule percentage text box
     *
     * @param percent -the percentage to set
     */
    public void setCellularPercentage(String percent) {
        getPercentageTextBox().setText(percent);
    }

    /**
     * Sets min cells per vehicle value from given cellular device in min cells
     * per vehicle text box
     *
     * @param device -the cellular device
     */
    public void setMinCellsPerVehicle(CellularDevice device) {
        String min = device.getMinCellsPerVehicle();
        setMinCellsPerVehicle(min);
    }

    /**
     * Sets given value in min cells per vehicle text box
     *
     * @param min -the min cells per vehicle value to set
     */
    public void setMinCellsPerVehicle(String min) {
        String minString = min;
        getMinCellsPerVehicleTextBox().setText(minString);
    }

    /**
     * Sets max cells per vehicle value from given cellular device in max cells
     * per vehicle text box
     *
     * @param device -the cellular device
     */
    public void setMaxCellsPerVehicle(CellularDevice device) {
        String max = device.getMaxCellsPerVehicle();
        setMaxCellsPerVehicle(max);
    }

    /**
     * Sets given value in max cells per vehicle text box
     *
     * @param max -the max cells per vehicle value to set
     */
    public void setMaxCellsPerVehicle(String max) {
        String maxString = max;
        getMaxCellsPerVehicleTextBox().setText(maxString);
    }

    /**
     * Click the Create button
     *
     * @param success - true if success is expected, false otherwise
     * @return the newly loaded Composite Settings Modal if success, otherwise
     *         no return
     */
    public CompositeSettingsModal clickCreate(boolean success) {
        getBtn(BtnNames.CREATE).click();
        if (success) {
            waitForElementToBeInvisible(By.xpath(TITLE_XPATH_PREFIX + CREATE_CELLULAR_DEVICE_HEADER_TEXT + "']"), 10);
            return getPage(CompositeSettingsModal.class);
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
     * @return the newly loaded Configure Devices Form
     */
    public CompositeSettingsModal clickCancel() {
        getBtn(BtnNames.CANCEL).click();
        return getPage(CompositeSettingsModal.class);
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(CREATE_CELLULAR_DEVICE_HELP_HEADER_TEXT);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkCreateCellularDeviceHeaderIcons() {
        checkHeaderIcons(CREATE_CELLULAR_DEVICE_HEADER_TEXT);
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
     * Checks for presence of Cellular Device Name, Rule Percentage, Min
     * Quantity, and Max Quantity text boxes
     */
    public void checkFieldsDisplayed() {
        Assert.assertTrue("Name text box was not found.", isTextBoxDisplayed("deviceName"));
        Assert.assertTrue("Min Quantity text box was not found.", isTextBoxDisplayed("minQuantity"));
        Assert.assertTrue("Max Quantity text box was not found.", isTextBoxDisplayed("maxQuantity"));
        Assert.assertTrue("Percentage text box was not found.", isTextBoxDisplayed("percentage"));
    }

    /**
     * Sets values from given cellular device into Cellular DeviceName, Rule
     * Percentage, Min Cells per Vehicle, and Max Cells per Vehicle text boxes
     *
     * @param name -the name to set
     * @param percent -the percentage to set
     * @param minCells -the minimum number of cells per vehicle to set
     * @param maxCells -the maximum number of cells per vehicle to set
     */
    public void setAllFields(String name, String percent, String minCells, String maxCells) {
        setCellularDeviceName(name);
        setCellularPercentage(percent);
        setMinCellsPerVehicle(minCells);
        setMaxCellsPerVehicle(maxCells);
    }

    /**
     * Sets values from given cellular device into Cellular DeviceName, Rule
     * Percentage, Min Cells per Vehicle, and Max Cells per Vehicle text boxes
     *
     * @param device -the cellular device to set
     */
    public void setAllFields(CellularDevice device) {
        setCellularDeviceName(device);
        setCellularPercentage(device);
        setMinCellsPerVehicle(device);
        setMaxCellsPerVehicle(device);
    }

    /**
     * Checks values displayed in all fields and verifies they match the values
     * for the given cellular device
     *
     * @param device -the device expected
     */
    public void checkFieldValues(CellularDevice device) {
        String name = device.getName();
        String percent = device.getCellularPercent();
        String min = device.getMinCellsPerVehicle();
        String max = device.getMaxCellsPerVehicle();
        checkFieldValues(name, percent, min, max);

    }

    /**
     * Checks values displayed in all fields and verifies they match the given
     * string values
     *
     * @param deviceName -the cellular device name expected
     * @param percent -the percentage expected
     * @param min -the min value of cells per vehicle value expected
     * @param max -the max value of cells per vehicle value expected
     */
    public void checkFieldValues(String deviceName, int percent, int min, int max) {
        Assert.assertEquals("The cellular device name value," + deviceName + ", is not displayed as expected.", deviceName, getDisplayedCellularDeviceName());
        Assert.assertEquals("The percentage value," + percent + ", is not displayed as expected.", Integer.toString(percent), getDisplayedPercentage());
        Assert.assertEquals("The min cells per vehicle value," + min + ", is not displayed as expected.", Integer.toString(min), getDisplayedMinCellsPerVehicle());
        Assert.assertEquals("The max cells per vehicle value," + max + ", is not displayed as expected.", Integer.toString(max), getDisplayedMaxCellsPerVehicle());
    }

    /**
     * Checks values displayed in all fields and verifies they match the given
     * string values
     *
     * @param deviceName -the cellular device name expected
     * @param percent -the percentage expected
     * @param min -the min value of cells per vehicle value expected
     * @param max -the max value of cells per vehicle value expected
     */
    public void checkFieldValues(String deviceName, String percent, String min, String max) {
        Assert.assertEquals("The cellular device name value," + deviceName + ", is not displayed as expected.", deviceName, getDisplayedCellularDeviceName());
        Assert.assertEquals("The percentage value," + percent + ", is not displayed as expected.", percent, getDisplayedPercentage());
        Assert.assertEquals("The min cells per vehicle value," + min + ", is not displayed as expected.", min, getDisplayedMinCellsPerVehicle());
        Assert.assertEquals("The max cells per vehicle value," + max + ", is not displayed as expected.", max, getDisplayedMaxCellsPerVehicle());
    }

    //////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(CREATE_CELL_DEVICE_TEXT_BOX_XPATH_PREFIX + CreateCellDeviceTextBox.MAX_QUANTITY.name + "']"), 20);
    }
}
