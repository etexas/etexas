package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeSettingsModal;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the Edit Cellular Device form
 *
 * @author llaroussini
 */
public class EditCellularDeviceModal extends CreateCellularDeviceModal {

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public EditCellularDeviceModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the edit cellular device header
     */
    private static final String EDIT_CELLULAR_DEVICE_HEADER_TEXT = "Edit Cellular Device Profile";

    /**
     * Xpath to the edit cellular device
     */
    private static final String EDIT_CELLULAR_DEVICE_HEADER_XPATH = TITLE_XPATH_PREFIX + EDIT_CELLULAR_DEVICE_HEADER_TEXT + "']";

    /**
     * Text displayed in the edit cellular help header
     */
    private static final String EDIT_CELLULAR_DEVICE_HELP_HEADER_TEXT = "Edit Cellular Device Profile Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Edit a cellular device configuration in the simulation. The rule percentage determines the percent of vehicles that will have matching cellular devices on board. The minimum/maximum number of cells per vehicle determines the number of cellular devices on board affected vehicles.";

    /**
     * Xpath prefix to edit cellular device buttons
     */
    private static final String EDIT_CELLULAR_DEVICE_BTN_XPATH_PREFIX = "//a[contains(@id, 'ETexas-device-view-cellular-Edit')]//span[text()='";

    /**
     * Xpath prefix for the input fields
     */
    private static final String CELLULAR_DEVICE_INPUT_PREFIX = "//input[contains(@id, 'ETexas-device-view-cellular-Edit')][@name='";

    /**
     * Xpath for ancestor anchor
     */
    private static final String ANCESTOR_A_XPATH = "/ancestor::a";

    ///////////
    //Getters
    ///////////

    /**
     * The xpath to get from the form content window to the help icon
     *
     * @return the help icon
     */
    private El getEditCellularDeviceHelpIcon() {
        return getHelpIcon(EDIT_CELLULAR_DEVICE_HEADER_TEXT);
    }

    /**
     * Gets the given button in the edit cellular device window
     *
     * @param btn - the button to get
     * @return the button element
     */
    private El getBtn(BtnNames btn) {
        return el(By.xpath(EDIT_CELLULAR_DEVICE_BTN_XPATH_PREFIX + btn.getLabel() + "']" + ANCESTOR_A_XPATH));
    }

    /**
     * Gets the El input text box for the given input (Name, Min Quantity, Max
     * Quantity, Percentage)
     *
     * @param inputName - the String name of the text box element to return
     * @return the text box El
     */
    private El getCellularEditInputTextBox(String inputName) {
        return el(By.xpath(CELLULAR_DEVICE_INPUT_PREFIX + inputName + "']"));
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the edit cellular device header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditCellularDeviceHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_CELLULAR_DEVICE_HEADER_TEXT);
    }

    /**
     * Checks to see if the edit cellular device help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditCellularDeviceHelpHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_CELLULAR_DEVICE_HELP_HEADER_TEXT);

    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isEditCellularDeviceHelpIconDisplayed() {
        return isHelpIconDisplayed(EDIT_CELLULAR_DEVICE_HEADER_TEXT);
    }

    /**
     * Checks to see if the help content is displayed
     *
     * @return true if the help content is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isEditCellularDeviceHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditCellularDeviceHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(EDIT_CELLULAR_DEVICE_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the update button is displayed
     *
     * @return true if the update button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isUpdateBtnDisplayed() {
        return isElementDisplayed(getBtn(BtnNames.UPDATE));
    }

    /**
     * Checks to see if the reset button is displayed
     *
     * @return true if the reset button is displayed, false if it is not or
     *         cannot be found
     */
    @Override
    public boolean isResetBtnDisplayed() {
        return isElementDisplayed(getBtn(BtnNames.RESET));
    }

    /**
     * Checks to see if the cancel button is displayed
     *
     * @return true if the cancel button is displayed, false if it is not or
     *         cannot be found
     */
    @Override
    public boolean isCancelBtnDisplayed() {
        return isElementDisplayed(getBtn(BtnNames.CANCEL));
    }

    /**
     * Checks to see if the text box of the given input name is displayed
     *
     * @param inputName - the String name of the text box being checked
     * @return true if the element is displayed, otherwise false
     */
    @Override
    public boolean isTextBoxDisplayed(String inputName) {
        return isElementDisplayed(getCellularEditInputTextBox(inputName));
    }

    ////////////////
    // Interaction
    ///////////////


    /**
     * Click the edit cellular device help icon
     */
    public void clickEditCellularDeviceHelp() {
        getEditCellularDeviceHelpIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickEditCellularDeviceHelpOKBtn() {
        clickHelpOKBtn(EDIT_CELLULAR_DEVICE_HELP_HEADER_TEXT);
    }

    /**
     * Click the Close icon
     */
    @Override
    public void clickCloseIcon() {
        getCloseIcon(EDIT_CELLULAR_DEVICE_HEADER_TEXT).click();
    }

    /**
     * Click the update button
     *
     * @param success -true if success expected, false otherwise
     * @return the newly loaded Configure Devices Form if success, otherwise no
     *         return
     */
    public ConfigureCellularDevicePartialPage clickUpdate(boolean success) {
        getBtn(BtnNames.UPDATE).click();
        if(success){
            waitForElementToBeInvisible(By.xpath(EDIT_CELLULAR_DEVICE_HEADER_XPATH));
            return getPage(ConfigureCellularDevicePartialPage.class);
        }
        else{
            return null;
        }
    }

    /**
     * Click the Reset button
     */
    @Override
    public void clickReset() {
        getBtn(BtnNames.RESET).click();
    }

    /**
     * Click the Cancel button
     *
     * @return CompositeSettingsModal
     */
    @Override
    public CompositeSettingsModal clickCancel() {
        getBtn(BtnNames.CANCEL).click();
        return getPage(CompositeSettingsModal.class);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkEditCellularDeviceHeaderIcons() {
        checkHeaderIcons(EDIT_CELLULAR_DEVICE_HEADER_TEXT);
    }

    /**
     * Checks for the presence of the Update, Reset, and Cancel buttons
     */
    public void checkEditCellularDeviceBtns() {
        Assert.assertTrue("The Update button is not displayed.", isUpdateBtnDisplayed());
        Assert.assertTrue("The Reset button is not displayed.", isResetBtnDisplayed());
        Assert.assertTrue("The Cancel button is not displayed.", isCancelBtnDisplayed());
    }

    /**
     * Checks for presence of Cellular Device Name, Rule Percentage, Min
     * Quantity, and Max Quantity text boxes
     */
    @Override
    public void checkFieldsDisplayed() {
        Assert.assertTrue("Name text box was not found.", isTextBoxDisplayed("deviceName"));
        Assert.assertTrue("Min Quantity text box was not found.", isTextBoxDisplayed("minQuantity"));
        Assert.assertTrue("Max Quantity text box was not found.", isTextBoxDisplayed("maxQuantity"));
        Assert.assertTrue("Percentage text box was not found.", isTextBoxDisplayed("percentage"));
    }

    //////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(EDIT_CELLULAR_DEVICE_HEADER_XPATH), 20);
    }
}
