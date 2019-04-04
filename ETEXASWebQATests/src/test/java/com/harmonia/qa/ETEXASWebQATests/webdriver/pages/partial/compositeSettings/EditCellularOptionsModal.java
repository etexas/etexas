package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.CellularOptionsConfiguration;
import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Partial page class representing the Advanced Cellular Options form
 *
 * @author llaroussini
 */
public class EditCellularOptionsModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver
     */
    public EditCellularOptionsModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    // Enumerated elements
    ///////////

    /**
     * Field Names
     *
     * @author llaroussini
     */
    public enum CellularOptionFields {
        /**
         * Uplink Bandwidth field
         */
        UPLINK_BANDWIDTH("Uplink Bandwidth:", "uplinkBandwidth", "25"),
        /**
         * Uplink Carrier Frequency field
         */
        UPLINK_CARRIER_FREQUENCY("Uplink Carrier Frequency:", "uplinkCarrierFrequency", "18100"),
        /**
         * Downlink Bandwidth field
         */
        DOWNLINK_BANDWIDTH("Downlink Bandwidth:", "downlinkBandwidth", "25"),
        /**
         * Downlink Carrier Frequency field
         */
        DOWNLINK_CARRIER_FREQUENCY("Downlink Carrier Frequency:", "downlinkCarrierFrequency", "100"),
        /**
         * Cell Device Power field
         */
        CELL_POWER("Cellular Device Power (dBm):", "cellularDevicePower", "10"),
        /**
         * Cell Noise Figure field
         */
        CELL_NOISE("Cellular Device Noise (dB):", "cellularDeviceNoise", "9"),
        /**
         * Cell Tower Power field
         */
        CELL_TOWER_POWER("Cell Tower Power (dBm):", "cellTowerPower", "30"),
        /**
         * Cell Tower Noise field
         */
        CELL_TOWER_NOISE("Cell Tower Noise (dB):", "cellTowerNoise", "5");

        /**
         * The label associated with the field
         */
        private String label;

        /**
         * The name attribute associated with the field
         */
        private String name;

        /**
         * The default value associated with the field
         */
        private String defaultValue;

        /**
         * Constructor for this enum
         *
         * @param label the label of the field
         * @param name the name attribute associated with the field
         * @param defaultValue the default value associated with the field
         */
        private CellularOptionFields(String label, String name, String defaultValue) {
            this.label = label;
            this.name = name;
            this.defaultValue = defaultValue;
        }

        /**
         * Gets the label of the field
         *
         * @return the label of the field
         */
        public String getLabel() {
            return this.label;
        }

        /**
         * Gets the name associated with the field
         *
         * @return the name associated with the field
         */
        public String getName() {
            return this.name;
        }

        /**
         * Gets the default value associated with the field
         *
         * @return the default value associated with the field
         */
        public String getDefaultValue() {
            return this.defaultValue;
        }
    }

    /**
     * Bandwidth Values
     *
     * @author llaroussini
     */
    public enum BandwidthValues {
        /**
         * 6
         */
        SIX("6"),
        /**
         * 15
         */
        FIFTEEN("15"),
        /**
         * 25
         */
        TWENTY_FIVE("25"),
        /**
         * 50
         */
        FIFTY("50"),
        /**
         * 75
         */
        SEVENTY_FIVE("75"),
        /**
         * 100
         */
        ONE_HUNDRED("100");

        /**
         * The bandwidth value
         */
        private String value;

        /**
         * Constructor for this enum
         *
         * @param value the bandwidth value
         */
        private BandwidthValues(String value) {
            this.value = value;
        }

        /**
         * Gets the bandwidth value
         *
         * @return the bandwidth value
         */
        public String getValue() {
            return this.value;
        }
    }

    /////////////////////
    // ID's and Locators
    /////////////////////

    /**
     * Text displayed in Edit Options header
     */
    private static final String EDIT_CELL_OPTIONS_HEADER_TEXT = "Edit Cellular Options";

    /**
     * Text displayed in the Edit Options help header
     */
    private static final String EDIT_CELL_OPTIONS_HELP_HEADER_TEXT = "Edit Cellular Options Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Edit the uplink bandwidth, the downlink bandwidth, the uplink carrier frequency, the downlink carrier frequency, the cell tower noise (dB), the cell tower transmission power (dBm), the cellular device noise (dB), and the cellular device transmission power (dBm) for the selected composite. Note that these settings will only influence communication when a cellular communications model (NS3) is selected.";

    /**
     * Xpath prefix to text box fields
     */
    private static final String FORM_TEXT_BOX_XPATH_PREFIX = "//input[@name='";

    /**
     * Xpath to the Uplink Bandwidth dropdown
     */
    private static final String UPLINK_BANDWIDTH_DROPDOWN_XPATH = "//div[contains(@id, 'uplink')][contains(@id, 'trigger')]";

    /**
     * Xpath to the Downlink Bandwidth dropdown
     */
    private static final String DOWNLINK_BANDWIDTH_DROPDOWN_XPATH = "//div[contains(@id, 'downlink')][contains(@id, 'trigger')]";

    /**
     * Xpath to an option in the uplink bandwidth dropdown menu
     */
    private static final String UPLINK_BANDWIDTH_OPTION_XPATH = "//div[contains(@id, 'uplink')][contains(@class, 'x-boundlist')]//li[text()='";

    /**
     * Xpath to an option in the downlink bandwidth dropdown menu
     */
    private static final String DOWNLINK_BANDWIDTH_OPTION_XPATH = "//div[contains(@id, 'downlink')][contains(@class, 'x-boundlist')]//li[text()='";

    /**
     * Xpath prefix to buttons in Edit Cellular Options form
     */
    private static final String EDIT_CELLULAR_OPTIONS_BTN_XPATH_PREFIX = "//div[contains(@id, 'options')][contains(@id, 'Cellular')]//span[text()='";

    /**
     * Error text displayed when invalid uplink range is used
     */
    private static final String UPLINK_INVALID_RANGE_ERROR_TEXT = "Uplink carrier frequencies must be in the range of 18000 to 24599.";

    /**
     * Error text displayed when invalid downlink range is used
     */
    private static final String DOWNLINK_INVALID_RANGE_ERROR_TEXT = "Downlink carrier frequencies must be in the range of 0 to 6599.";

    /**
     * Error text displayed when invalid cell device power range is used
     */
    private static final String CELL_POWER_INVALID_RANGE_ERROR_TEXT = "Cellular device power must be in the range of -200 to 200 dBm.";

    /**
     * Error text displayed when invalid cell tower power range is used
     */
    private static final String CELL_TOWER_POWER_INVALID_RANGE_ERROR_TEXT = "Cell tower power must be in the range of -200 to 200 dBm.";

    /**
     * Error text displayed when invalid cell device noise range is used
     */
    private static final String CELL_NOISE_INVALID_RANGE_ERROR_TEXT = "Cellular device noise must be in the range of 0 to 194 dB.";

    /**
     * Error text displayed when invalid cell tower noise range is used
     */
    private static final String CELL_TOWER_NOISE_INVALID_RANGE_ERROR_TEXT = "Cell tower noise must be in the range of 0 to 194 dB.";

    ////////////
    // Getters
    ////////////

    /**
     * The xpath to get from the edit cellular options header to the help icon
     *
     * @return the help icon
     */
    private El getEditCellOptionsHelpIcon() {
        return getHelpIcon(EDIT_CELL_OPTIONS_HEADER_TEXT);
    }

    /**
     * Gets the given form text box
     *
     * @param field -the field to get
     * @return the text box element
     */
    private El getFormTextBox(CellularOptionFields field) {
        return el(By.xpath(FORM_TEXT_BOX_XPATH_PREFIX + field.getName() + "']"));
    }

    /**
     * Gets the Uplink Bandwidth dropdown
     *
     * @return the dropdown element
     */
    private El getUplinkBandwidthDropdown() {
        return el(By.xpath(UPLINK_BANDWIDTH_DROPDOWN_XPATH));
    }

    /**
     * Gets the Downlink Bandwidth dropdown
     *
     * @return the dropdown element
     */
    private El getDownlinkBandwidthDropdown() {
        return el(By.xpath(DOWNLINK_BANDWIDTH_DROPDOWN_XPATH));
    }

    /**
     * Gets the given button
     *
     * @param btn -the button to get
     * @return the button element
     */
    private El getCellularOptionsBtn(BtnNames btn) {
        return el(By.xpath(EDIT_CELLULAR_OPTIONS_BTN_XPATH_PREFIX + btn.getLabel() + "']"));
    }

    /**
     * Gets option with given value in Uplink Bandwidth dropdown list
     *
     * @param value -the value expected
     * @return the option element
     */
    private El getSpecificUplinkOptionInDropdown(String value) {
        return el(By.xpath(UPLINK_BANDWIDTH_OPTION_XPATH + value + "']"));
    }

    /**
     * Gets option with given value in Downlink Bandwidth dropdown list
     *
     * @param value -the value expected
     * @return the option element
     */
    private El getSpecificDownlinkOptionInDropdown(String value) {
        return el(By.xpath(DOWNLINK_BANDWIDTH_OPTION_XPATH + value + "']"));
    }

    /**
     * Gets the displayed value in the Uplink Bandwidth text box
     *
     * @return the value as string
     */
    public String getUplinkBandwidthValue() {
        return getFormTextBox(CellularOptionFields.UPLINK_BANDWIDTH).getAttribute("value");
    }

    /**
     * Gets the displayed value in the Uplink Carrier Frequency text box
     *
     * @return the value as string
     */
    public String getUplinkCarrierFrequencyValue() {
        return getFormTextBox(CellularOptionFields.UPLINK_CARRIER_FREQUENCY).getAttribute("value");
    }

    /**
     * Gets the displayed value in the Downlink Bandwidth text box
     *
     * @return the value as string
     */
    public String getDownlinkBandwidthValue() {
        return getFormTextBox(CellularOptionFields.DOWNLINK_BANDWIDTH).getAttribute("value");
    }

    /**
     * Gets the displayed value in the Downlink Carrier Frequency text box
     *
     * @return the value as string
     */
    public String getDownlinkCarrierFrequencyValue() {
        return getFormTextBox(CellularOptionFields.DOWNLINK_CARRIER_FREQUENCY).getAttribute("value");
    }

    /**
     * Gets the displayed value in the Cell Device Power text box
     *
     * @return the value as string
     */
    public String getCellPowerValue() {
        return getFormTextBox(CellularOptionFields.CELL_POWER).getAttribute("value");
    }

    /**
     * Gets the displayed value in the Cell Noise Figure text box
     *
     * @return the value as string
     */
    public String getCellNoiseValue() {
        return getFormTextBox(CellularOptionFields.CELL_NOISE).getAttribute("value");
    }

    /**
     * Gets the displayed value in the Cell Tower Power text box
     *
     * @return the value as string
     */
    public String getCellTowerPowerValue() {
        return getFormTextBox(CellularOptionFields.CELL_TOWER_POWER).getAttribute("value");
    }

    /**
     * Gets the displayed value in the Cell Tower Noise Figure text box
     *
     * @return the value as string
     */
    public String getCellTowerNoiseValue() {
        return getFormTextBox(CellularOptionFields.CELL_TOWER_NOISE).getAttribute("value");
    }

    /////////////
    // Checkers
    ////////////

    /**
     * Checks to see if the edit cellular options header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditCellOptionsHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_CELL_OPTIONS_HEADER_TEXT);
    }

    /**
     * Checks to see if the edit cellular options help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditCellOptionsHelpHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_CELL_OPTIONS_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the edit cellular options help content is displayed
     *
     * @return true if the content is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditCellOptionsHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isEditCellOptionsHelpIconDisplayed() {
        return isHelpIconDisplayed(EDIT_CELL_OPTIONS_HEADER_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(EDIT_CELL_OPTIONS_HELP_HEADER_TEXT);
    }

    /**
     * Checks if given form text box is displayed
     *
     * @param field -the form text box expected
     * @return true if text box is displayed, false otherwise
     */
    public boolean isFormTextBoxDisplayed(CellularOptionFields field) {
        return isElementDisplayed(getFormTextBox(field));
    }

    /**
     * Checks if uplink bandwidth dropdown is displayed
     *
     * @return true if dropdown is displayed, false otherwise
     */
    public boolean isUplinkBandwidthDropdownDisplayed() {
        return isElementDisplayed(getUplinkBandwidthDropdown());
    }

    /**
     * Checks if downlink bandwidth dropdown is displayed
     *
     * @return true if dropdown is displayed, false otherwise
     */
    public boolean isDownlinkBandwidthDropdownDisplayed() {
        return isElementDisplayed(getDownlinkBandwidthDropdown());
    }

    /**
     * Checks if given button is displayed
     *
     * @param btnName -name of the button expected
     * @return true if button is displayed, false otherwise
     */
    public boolean isBtnDisplayed(BtnNames btnName) {
        return isBtnDisplayed(EDIT_CELL_OPTIONS_HEADER_TEXT, btnName.getLabel());
    }

    /**
     * Checks if Invalid Number error is displayed with the Uplink Carrier
     * Frequency field
     *
     * @return true if error is displayed, false otherwise
     */
    public boolean isUplinkCarrierFreqInvalidNumberErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(CellularOptionFields.UPLINK_CARRIER_FREQUENCY.getLabel());
    }

    /**
     * Checks if Invalid Range error is displayed with the Uplink Carrier
     * Frequency field
     *
     * @return true if error is displayed, false otherwise
     */
    public boolean isUplinkCarrierFreqInvalidRangeErrorDisplayed() {
        return isErrorToolTipDisplayed(UPLINK_INVALID_RANGE_ERROR_TEXT);
    }

    /**
     * Checks if Invalid Number error is displayed with the Downlink Carrier
     * Frequency field
     *
     * @return true if error is displayed, false otherwise
     */
    public boolean isDownlinkCarrierFreqInvalidNumberErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(CellularOptionFields.DOWNLINK_CARRIER_FREQUENCY.getLabel());
    }

    /**
     * Checks if Invalid Range error is displayed with the Downlink Carrier
     * Frequency field
     *
     * @return true if error is displayed, false otherwise
     */
    public boolean isDownlinkCarrierFreqInvalidRangeErrorDisplayed() {
        return isErrorToolTipDisplayed(DOWNLINK_INVALID_RANGE_ERROR_TEXT);
    }

    /**
     * Checks if Invalid Number error is displayed with the Cellular Device
     * Power field
     *
     * @return true if error is displayed, false otherwise
     */
    public boolean isCellPowerInvalidNumberErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(CellularOptionFields.CELL_POWER.getLabel());
    }

    /**
     * Checks if Invalid Range error is displayed with the Cell Power field
     *
     * @return true if error is displayed, false otherwise
     */
    public boolean isCellPowerInvalidRangeErrorDisplayed() {
        return isErrorToolTipDisplayed(CELL_POWER_INVALID_RANGE_ERROR_TEXT);
    }

    /**
     * Checks if Invalid Number error is displayed with the Cell Noise field
     *
     * @return true if error is displayed, false otherwise
     */
    public boolean isCellNoiseInvalidNumberErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(CellularOptionFields.CELL_NOISE.getLabel());
    }

    /**
     * Checks if Invalid Range error is displayed with the Cell Noise field
     *
     * @return true if error is displayed, false otherwise
     */
    public boolean isCellNoiseInvalidRangeErrorDisplayed() {
        return isErrorToolTipDisplayed(CELL_NOISE_INVALID_RANGE_ERROR_TEXT);
    }

    /**
     * Checks if Invalid Number error is displayed with the Cell Tower Power
     * field
     *
     * @return true if error is displayed, false otherwise
     */
    public boolean isCellTowerPowerInvalidNumberErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(CellularOptionFields.CELL_TOWER_POWER.getLabel());
    }

    /**
     * Checks if Invalid Range error is displayed with the Cell Tower Power
     * field
     *
     * @return true if error is displayed, false otherwise
     */
    public boolean isCellTowerPowerInvalidRangeErrorDisplayed() {
        return isErrorToolTipDisplayed(CELL_TOWER_POWER_INVALID_RANGE_ERROR_TEXT);
    }

    /**
     * Checks if Invalid Number error is displayed with the Cell Noise field
     *
     * @return true if error is displayed, false otherwise
     */
    public boolean isCellTowerNoiseInvalidNumberErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(CellularOptionFields.CELL_TOWER_NOISE.getLabel());
    }

    /**
     * Checks if Invalid Range error is displayed with the Cell Tower Noise
     * field
     *
     * @return true if error is displayed, false otherwise
     */
    public boolean isCellTowerNoiseInvalidRangeErrorDisplayed() {
        return isErrorToolTipDisplayed(CELL_TOWER_NOISE_INVALID_RANGE_ERROR_TEXT);
    }

    ///////////////
    // Verification
    ///////////////

    /**
     * Verifies the Uplink Bandwidth dropdown is displayed
     */
    public void checkUplinkBandwidthFieldDisplayed() {
        Assert.assertTrue("The Uplink Bandwidth field is not displayed as expected.", isUplinkBandwidthDropdownDisplayed());
    }

    /**
     * Verifies the Uplink Carrier Frequency text box is displayed
     */
    public void checkUplinkCarrierFrequencyFieldDisplayed() {
        Assert.assertTrue("The Uplink Carrier Frequency field is not displayed as expected.", isFormTextBoxDisplayed(CellularOptionFields.UPLINK_CARRIER_FREQUENCY));
    }

    /**
     * Verifies the Downlink Bandwidth dropdown is displayed
     */
    public void checkDownlinkBandwidthFieldDisplayed() {
        Assert.assertTrue("The Downlink Bandwidth field is not displayed as expected.", isDownlinkBandwidthDropdownDisplayed());
    }

    /**
     * Verifies the Downlink Carrier Frequency text box is displayed
     */
    public void checkDownlinkCarrierFrequencyFieldDisplayed() {
        Assert.assertTrue("The Downlink Carrier Frequency field is not displayed as expected.", isFormTextBoxDisplayed(CellularOptionFields.DOWNLINK_CARRIER_FREQUENCY));
    }

    /**
     * Verifies the Cell Power text box is displayed
     */
    public void checkCellPowerFieldDisplayed() {
        Assert.assertTrue("The Cell Power field is not displayed as expected.", isFormTextBoxDisplayed(CellularOptionFields.CELL_POWER));
    }

    /**
     * Verifies the Cell Noise text box is displayed
     */
    public void checkCellNoiseFieldDisplayed() {
        Assert.assertTrue("The Cell Noise field is not displayed as expected.", isFormTextBoxDisplayed(CellularOptionFields.CELL_NOISE));
    }

    /**
     * Verifies the Cell Tower Power text box is displayed
     */
    public void checkCellTowerPowerFieldDisplayed() {
        Assert.assertTrue("The Cell Tower Power field is not displayed as expected.", isFormTextBoxDisplayed(CellularOptionFields.CELL_TOWER_POWER));
    }

    /**
     * Verifies the Cell Tower Noise text box is displayed
     */
    public void checkCellTowerNoiseFieldDisplayed() {
        Assert.assertTrue("The Cell Tower Noise field is not displayed as expected.", isFormTextBoxDisplayed(CellularOptionFields.CELL_TOWER_NOISE));
    }

    /**
     * Verifies the Update button is displayed
     */
    public void checkUpdateBtnDisplayed() {
        Assert.assertTrue("Update button is not displayed as expected.", isBtnDisplayed(BtnNames.UPDATE));
    }

    /**
     * Verifies the Reset button is displayed
     */
    public void checkResetBtnDisplayed() {
        Assert.assertTrue("Reset button is not displayed as expected.", isBtnDisplayed(BtnNames.RESET));
    }

    /**
     * Verifies the Cancel button is displayed
     */
    public void checkCancelBtnDisplayed() {
        Assert.assertTrue("Cancel button is not displayed as expected.", isBtnDisplayed(BtnNames.CANCEL));
    }

    ///////////////
    // Interaction
    ///////////////

    /**
     * Click the Edit Cell Options Help icon
     */
    public void clickEditCellOptionsHelp() {
        getEditCellOptionsHelpIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(EDIT_CELL_OPTIONS_HELP_HEADER_TEXT);
        waitForElementToBeInvisible(By.xpath(TITLE_XPATH_PREFIX + EDIT_CELL_OPTIONS_HELP_HEADER_TEXT + "')]"));
    }

    /**
     * Clicks the Reset button
     */
    public void clickResetBtn() {
        getCellularOptionsBtn(BtnNames.RESET).click();
    }

    /**
     * Clicks the Cancel button
     *
     * @return the newly loaded Composite Options Partial Page
     */
    public CompositeOptionsPartialPage clickCancelBtn() {
        getCellularOptionsBtn(BtnNames.CANCEL).click();
        return getPage(CompositeOptionsPartialPage.class);
    }

    /**
     * Clicks the Update button
     *
     * @return the newly loaded Composite Options Partial Page
     */
    public CompositeOptionsPartialPage clickUpdateBtn() {
        getCellularOptionsBtn(BtnNames.UPDATE).click();
        return getPage(CompositeOptionsPartialPage.class);
    }

    /**
     * Clicks the Close icon
     *
     * @return the newly loaded Composite Options Partial Page
     */
    public CompositeOptionsPartialPage clickCloseIcon() {
        getCloseIcon(EDIT_CELL_OPTIONS_HEADER_TEXT).click();
        return getPage(CompositeOptionsPartialPage.class);
    }

    /**
     * Selects the given value to set as Uplink Bandwidth
     *
     * @param bandwidth -bandwidth value to select
     */
    public void selectUplinkBandwidth(BandwidthValues bandwidth) {
        El dropdown = getUplinkBandwidthDropdown();
        selectFromDropdownList(dropdown, getSpecificUplinkOptionInDropdown(bandwidth.getValue()));
    }

    /**
     * Selects the Uplink Bandwidth of the given configuration
     *
     * @param config -the cellular configuration
     */
    public void selectUplinkBandwidth(CellularOptionsConfiguration config) {
        BandwidthValues bandwidth = config.getUplinkBandwidth();
        selectUplinkBandwidth(bandwidth);
    }

    /**
     * Sets the given text in the Uplink Carrier Frequency text box
     *
     * @param value -the value to set
     */
    public void setUplinkCarrierFrequency(String value) {
        getFormTextBox(CellularOptionFields.UPLINK_CARRIER_FREQUENCY).setText(value);
    }

    /**
     * Sets the Uplink Carrier Frequency of the given configuration
     *
     * @param config -the cellular configuration
     */
    public void setUplinkCarrierFrequency(CellularOptionsConfiguration config) {
        String value = Integer.toString(config.getUplinkCarrierFrequency());
        setUplinkCarrierFrequency(value);
    }

    /**
     * Selects the given value to set as Downlink Bandwidth
     *
     * @param bandwidth -bandwidth value to select
     */
    public void selectDownlinkBandwidth(BandwidthValues bandwidth) {
        El dropdown = getDownlinkBandwidthDropdown();
        selectFromDropdownList(dropdown, getSpecificDownlinkOptionInDropdown(bandwidth.getValue()));
    }

    /**
     * Selects the Downlink Bandwidth of the given configuration
     *
     * @param config -the cellular configuration
     */
    public void selectDownlinkBandwidth(CellularOptionsConfiguration config) {
        BandwidthValues bandwidth = config.getDownlinkBandwidth();
        selectDownlinkBandwidth(bandwidth);
    }

    /**
     * Sets the given text in the Downlink Carrier Frequency text box
     *
     * @param value -the value to set
     */
    public void setDownlinkCarrierFrequency(String value) {
        getFormTextBox(CellularOptionFields.DOWNLINK_CARRIER_FREQUENCY).setText(value);
    }

    /**
     * Sets the Downlink Carrier Frequency of the given configuration
     *
     * @param config -the advanced cellular configuration
     */
    public void setDownlinkCarrierFrequency(CellularOptionsConfiguration config) {
        String value = Integer.toString(config.getDownlinkCarrierFrequency());
        setDownlinkCarrierFrequency(value);
    }

    /**
     * Sets the given text in the Cell Device Power text box
     *
     * @param value -the value to set
     */
    public void setCellDevicePower(String value) {
        getFormTextBox(CellularOptionFields.CELL_POWER).setText(value);
    }

    /**
     * Sets the Cell Power of the given configuration
     *
     * @param config -the advanced cellular configuration
     */
    public void setCellPower(CellularOptionsConfiguration config) {
        String value = Integer.toString(config.getCellPower());
        setCellDevicePower(value);
    }

    /**
     * Sets the given text in the Cell Noise text box
     *
     * @param value -the value to set
     */
    public void setCellDeviceNoise(String value) {
        getFormTextBox(CellularOptionFields.CELL_NOISE).setText(value);
    }

    /**
     * Sets the Cell Noise of the given configuration
     *
     * @param config -the advanced cellular configuration
     */
    public void setCellDeviceNoise(CellularOptionsConfiguration config) {
        String value = Integer.toString(config.getCellNoise());
        setCellDeviceNoise(value);
    }

    /**
     * Sets the given text in the Cell Tower Power text box
     *
     * @param value -the value to set
     */
    public void setCellTowerPower(String value) {
        getFormTextBox(CellularOptionFields.CELL_TOWER_POWER).setText(value);
    }

    /**
     * Sets the Cell Tower Power of the given configuration
     *
     * @param config -the advanced cellular configuration
     */
    public void setCellTowerPower(CellularOptionsConfiguration config) {
        String value = Integer.toString(config.getCellTowerPower());
        setCellTowerPower(value);
    }

    /**
     * Sets the given text in the Cell Tower Noise text box
     *
     * @param value -the value to set
     */
    public void setCellTowerNoise(String value) {
        getFormTextBox(CellularOptionFields.CELL_TOWER_NOISE).setText(value);
    }

    /**
     * Sets the Cell Tower Noise of the given configuration
     *
     * @param config -the advanced cellular configuration
     */
    public void setCellTowerNoise(CellularOptionsConfiguration config) {
        String value = Integer.toString(config.getCellTowerNoise());
        setCellTowerNoise(value);
    }

    /**
     * Copies the value from the Cell Power text box and pastes into Uplink
     * Carrier Frequency text box
     */
    public void copyCellPowerValueAndPasteInUplinkCarrierFreqTextBox() {
        copyPaste(getFormTextBox(CellularOptionFields.CELL_POWER), getFormTextBox(CellularOptionFields.UPLINK_CARRIER_FREQUENCY));
    }

    /**
     * Copies the value from the Cell Power text box and pastes into Downlink
     * Carrier Frequency text box
     */
    public void copyCellPowerValueAndPasteInDownlinkCarrierFreqTextBox() {
        copyPaste(getFormTextBox(CellularOptionFields.CELL_POWER), getFormTextBox(CellularOptionFields.DOWNLINK_CARRIER_FREQUENCY));
    }

    /**
     * Copies the value from the Cell Power text box and pastes into Cell Noise
     * Figure text box
     */
    public void copyCellPowerValueAndPasteInCellNoiseFigureTextBox() {
        copyPaste(getFormTextBox(CellularOptionFields.CELL_POWER), getFormTextBox(CellularOptionFields.CELL_NOISE));
    }

    /**
     * Copies the value from the Cell Tower Power text box and pastes into Cell
     * Noise Figure text box
     */
    public void copyCellPowerValueAndPasteInCellTowerNoiseFigureTextBox() {
        copyPaste(getFormTextBox(CellularOptionFields.CELL_POWER), getFormTextBox(CellularOptionFields.CELL_TOWER_NOISE));
    }

    /////////////
    // Utilities
    /////////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkEditCellOptionsHeaderIcons() {
        checkHeaderIcons(EDIT_CELL_OPTIONS_HEADER_TEXT);
    }

    /**
     * Checks that expected help header and help content are displayed
     */
    public void checkHelpModal() {
        Assert.assertTrue("Edit Cellular Options Help header is not displayed as expected.", isEditCellOptionsHelpHeaderDisplayed());
        Assert.assertTrue("Edit Cellular Options Help content is not displayed as expected.", isEditCellOptionsHelpContentDisplayed());
    }

    /**
     * Verifies all expected fields are displayed (Uplink Bandwidth, Uplink
     * Carrier Frequency, Downlink Bandwidth, Downlink Carrier Frequency, Cell
     * Power, Cell Noise, Cell Tower Power, Cell Tower Noise)
     */
    public void checkAllFields() {
        checkUplinkBandwidthFieldDisplayed();
        checkUplinkCarrierFrequencyFieldDisplayed();
        checkDownlinkBandwidthFieldDisplayed();
        checkDownlinkCarrierFrequencyFieldDisplayed();
        checkCellPowerFieldDisplayed();
        checkCellNoiseFieldDisplayed();
        checkCellTowerPowerFieldDisplayed();
        checkCellTowerNoiseFieldDisplayed();
    }

    /**
     * Verifies all expected buttons are displayed (Update, Reset, and Close)
     */
    public void checkAllBtns() {
        checkUpdateBtnDisplayed();
        checkResetBtnDisplayed();
        checkCancelBtnDisplayed();
    }

    /**
     * Sets given values in all Advanced Cellular Options fields
     *
     * @param uplinkBandwidth -the uplink bandwidth to select
     * @param uplinkCarrierFreq -the uplink carrier frequency value to set
     * @param downlinkBandwidth -the downlink bandwidth to select
     * @param downlinkCarrierFreq -the downlink carrier frequency value to set
     * @param cellPower -the cell tx power value to set
     * @param cellNoise -the cell noise value to set
     * @param cellTowerPower -the cell tower tx power value to set
     * @param cellTowerNoise -the cell tower noise value to set
     */
    public void setAllFields(BandwidthValues uplinkBandwidth, String uplinkCarrierFreq, BandwidthValues downlinkBandwidth, String downlinkCarrierFreq, String cellPower, String cellNoise,
            String cellTowerPower, String cellTowerNoise) {
        selectUplinkBandwidth(uplinkBandwidth);
        setUplinkCarrierFrequency(uplinkCarrierFreq);
        setDownlinkCarrierFrequency(downlinkCarrierFreq);
        selectDownlinkBandwidth(downlinkBandwidth);
        setCellDevicePower(cellPower);
        setCellDeviceNoise(cellNoise);
        setCellTowerPower(cellTowerPower);
        setCellTowerNoise(cellTowerNoise);
    }

    /**
     * Sets given values in all Advanced Cellular Options fields
     *
     * @param config - the Advanced Cellular Option Configuration to use to
     *        populate fields
     */
    public void setAllFields(CellularOptionsConfiguration config) {
        BandwidthValues uplinkBandwidth = config.getUplinkBandwidth();
        String uplinkCarrierFreq = Integer.toString(config.getUplinkCarrierFrequency());
        BandwidthValues downlinkBandwidth = config.getDownlinkBandwidth();
        String downlinkCarrierFreq = Integer.toString(config.getDownlinkCarrierFrequency());
        String cellPower = Integer.toString(config.getCellPower());
        String cellNoise = Integer.toString(config.getCellNoise());
        String cellTowerPower = Integer.toString(config.getCellTowerPower());
        String cellTowerNoise = Integer.toString(config.getCellTowerNoise());
        setAllFields(uplinkBandwidth, uplinkCarrierFreq, downlinkBandwidth, downlinkCarrierFreq, cellPower, cellNoise, cellTowerPower, cellTowerNoise);
    }

    /**
     * Verifies all values in the Advanced Cellular Options fields display
     * default values
     *
     * @param uplinkBandwidth -the uplink bandwidth expected
     * @param uplinkCarrierFrequency -the uplink carrier frequency expected
     * @param downlinkBandwidth -the downlink bandwidth expected
     * @param downlinkCarrierFrequency -the downlink carrier frequency expected
     * @param cellPower -the cell power value expected
     * @param cellNoise -the cell noise value expected
     * @param cellTowerPower -the cell tower power value expected
     * @param cellTowerNoise -the cell tower noise value expected
     */
    public void checkDisplayedValues(String uplinkBandwidth, String uplinkCarrierFrequency, String downlinkBandwidth, String downlinkCarrierFrequency, String cellPower, String cellNoise,
            String cellTowerPower, String cellTowerNoise) {
        Assert.assertEquals("The displayed Uplink Bandwidth value does not match the expected value.", uplinkBandwidth, getUplinkBandwidthValue());
        Assert.assertEquals("The displayed Uplink Carrier Frequency value does not match the expected value.", uplinkCarrierFrequency, getUplinkCarrierFrequencyValue());
        Assert.assertEquals("The displayed Downlink Bandwidth value does not match the expected value.", downlinkBandwidth, getDownlinkBandwidthValue());
        Assert.assertEquals("The displayed Downlink Carrier Frequency value does not match the expected value.", downlinkCarrierFrequency, getDownlinkCarrierFrequencyValue());
        Assert.assertEquals("The displayed Cell Tx Power value does not match the expected value.", cellPower, getCellPowerValue());
        Assert.assertEquals("The displayed Cell Noise Figure value does not match the expected value.", cellNoise, getCellNoiseValue());
        Assert.assertEquals("The displayed Cell Tower Tx Power value does not match the expected value.", cellTowerPower, getCellTowerPowerValue());
        Assert.assertEquals("The displayed Cell Tower Noise Figure value does not match the expected value.", cellTowerNoise, getCellTowerNoiseValue());
    }

    /**
     * Verifies the values associated with the given advanced cellular options
     * configuration match the displayed values
     *
     * @param config -the advanced cellular options configuration expected
     */
    public void checkDisplayedValues(CellularOptionsConfiguration config) {
        String uplinkBandwidth = config.getUplinkBandwidth().getValue();
        String uplinkCarrierFrequency = Integer.toString(config.getUplinkCarrierFrequency());
        String downlinkBandwidth = config.getDownlinkBandwidth().getValue();
        String downlinkCarrierFrequency = Integer.toString(config.getDownlinkCarrierFrequency());
        String cellPower = Integer.toString(config.getCellPower());
        String cellNoise = Integer.toString(config.getCellNoise());
        String cellTowerPower = Integer.toString(config.getCellTowerPower());
        String cellTowerNoise = Integer.toString(config.getCellTowerNoise());
        checkDisplayedValues(uplinkBandwidth, uplinkCarrierFrequency, downlinkBandwidth, downlinkCarrierFrequency, cellPower, cellNoise, cellTowerPower, cellTowerNoise);
    }

    /**
     * Verifies the expected default values are displayed in all fields
     */
    public void checkDefaultValuesDisplayed() {
        checkDisplayedValues(getUplinkBandwidthValue(), getUplinkCarrierFrequencyValue(), getDownlinkBandwidthValue(), getDownlinkCarrierFrequencyValue(), getCellPowerValue(), getCellNoiseValue(),
                getCellTowerPowerValue(), getCellTowerNoiseValue());
    }

    /**
     * Verifies 'required field' error displays for all expected fields (checks
     * Uplink Carrier Frequency, Downlink Carrier Frequency, Cell Power, Cell
     * Noise, Cell Tower Power, and Cell Tower Noise)
     */
    public void checkRequiredFieldErrorAllFields() {
        Assert.assertTrue("Required field error is not displayed for the Uplink Carrier Frequency field.", isFieldRequiredErrorDisplayed(CellularOptionFields.UPLINK_CARRIER_FREQUENCY.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Downlink Carrier Frequency field.", isFieldRequiredErrorDisplayed(CellularOptionFields.DOWNLINK_CARRIER_FREQUENCY.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Cell Power field.", isFieldRequiredErrorDisplayed(CellularOptionFields.CELL_POWER.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Cell Noise field.", isFieldRequiredErrorDisplayed(CellularOptionFields.CELL_NOISE.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Cell Tower Power field.", isFieldRequiredErrorDisplayed(CellularOptionFields.CELL_TOWER_POWER.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Cell Tower Noise field.", isFieldRequiredErrorDisplayed(CellularOptionFields.CELL_TOWER_NOISE.getLabel()));
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(FORM_TEXT_BOX_XPATH_PREFIX + CellularOptionFields.CELL_NOISE.getName() + "']"));
    }

}
