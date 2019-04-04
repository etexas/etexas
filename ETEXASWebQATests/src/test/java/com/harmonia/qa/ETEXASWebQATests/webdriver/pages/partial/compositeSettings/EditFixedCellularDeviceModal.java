package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the Edit Fixed Cellular Device form
 *
 * @author llaroussini
 * @author rsmith
 */
public class EditFixedCellularDeviceModal extends CreateFixedCellularDeviceModal {

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public EditFixedCellularDeviceModal(WebDriver driver) {
        super(driver);
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the edit fixed cellular device header
     */
    private static final String EDIT_FIXED_CELLULAR_DEVICE_HEADER_TEXT = "Edit Fixed Cellular Device";

    /**
     * Xpath to the edit fixed cellular device
     */
    private static final String EDIT_FIXED_CELLULAR_DEVICE_HEADER_XPATH = TITLE_XPATH_PREFIX + EDIT_FIXED_CELLULAR_DEVICE_HEADER_TEXT + "')]";

    /**
     * Text displayed in the edit fixed cellular help header
     */
    private static final String EDIT_FIXED_CELLULAR_DEVICE_HELP_HEADER_TEXT = "Edit Fixed Cellular Device Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Edit the name, MAC address, and coordinates for the selected fixed cellular device.";

    /**
     * Xpath prefix to edit fixed cellular device buttons
     */
    private static final String EDIT_FIXED_CELLULAR_DEVICE_BTN_XPATH_PREFIX = "//div[contains(@id, 'cell')][contains(@id, 'device')]//div[contains(@id, 'toolbar')]//span[contains(@id, 'button')][text()='";

    ///////////
    //Getters
    ///////////

    /**
     * The xpath to get from the form content window to the help icon
     *
     * @return the help icon
     */
    private El getEditFixedCellularDeviceHelpIcon() {
        return getHelpIcon(EDIT_FIXED_CELLULAR_DEVICE_HEADER_TEXT);
    }

    /**
     * Gets the given button in the edit fixed cellular device window
     *
     * @param btn - the button to get
     * @return the button element
     */
    private El getBtn(BtnNames btn) {
        return el(By.xpath(EDIT_FIXED_CELLULAR_DEVICE_BTN_XPATH_PREFIX + btn.getLabel() + "']"));
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the edit fixed cellular device header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditFixedCellularDeviceHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_FIXED_CELLULAR_DEVICE_HEADER_TEXT);
    }

    /**
     * Checks to see if the edit fixed cellular device help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditFixedCellularDeviceHelpHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_FIXED_CELLULAR_DEVICE_HELP_HEADER_TEXT);

    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isEditFixedCellularDeviceHelpIconDisplayed() {
        return isHelpIconDisplayed(EDIT_FIXED_CELLULAR_DEVICE_HEADER_TEXT);
    }

    /**
     * Checks to see if the help content is displayed
     *
     * @return true if the help content is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isEditFixedCellularDeviceHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditFixedCellularDeviceHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(EDIT_FIXED_CELLULAR_DEVICE_HELP_HEADER_TEXT);
    }

    /**
     * Click the edit fixed cellular device help icon
     */
    public void clickEditFixedCellularDeviceHelp() {
        getEditFixedCellularDeviceHelpIcon().click();
    }

    /**
     * Clicks the Close icon associated with the Edit Fixed Cellular Device
     * modal
     */
    @Override
    public void clickCloseIcon() {
        clickCloseIcon(EDIT_FIXED_CELLULAR_DEVICE_HEADER_TEXT);
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickEditFixedCellularDeviceHelpOKBtn() {
        clickHelpOKBtn(EDIT_FIXED_CELLULAR_DEVICE_HELP_HEADER_TEXT);
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

    ////////////////
    // Interaction
    ///////////////

    /**
     * Click the update button
     *
     * @param success -true if success is expected, false if error expected
     */
    public void clickUpdate(boolean success) {
        getBtn(BtnNames.UPDATE).click();
        if (success) waitForElementToBeInvisible(By.xpath(EDIT_FIXED_CELLULAR_DEVICE_HEADER_XPATH));
        ETexasCommonUtils.sleep(500); //ZZZ -allows time for Detector list to refresh
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkEditFixedCellularDeviceHeaderIcons() {
        checkHeaderIcons(EDIT_FIXED_CELLULAR_DEVICE_HEADER_TEXT);
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
     * Checks that help modal is displayed -- checks both the header and content
     * text
     */
    public void checkEdiFixedCellularDeviceHelpModal() {
        Assert.assertTrue("Edit Fixed Cellular Device Help header not displayed as expected.", isEditFixedCellularDeviceHelpHeaderDisplayed());
        Assert.assertTrue("Edit Fixed Cellular Device Help content not displayed as expected.", isEditFixedCellularDeviceHelpContentDisplayed());
    }
}