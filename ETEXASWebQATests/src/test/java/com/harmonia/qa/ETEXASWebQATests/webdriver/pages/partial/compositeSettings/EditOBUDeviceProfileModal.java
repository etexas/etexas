package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.ConfigureOBUDeviceProfilesPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.CreateOBUDeviceProfileModal;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the Edit OBU Device Profile Modal
 *
 * @author llaroussini
 */
public class EditOBUDeviceProfileModal extends CreateOBUDeviceProfileModal {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public EditOBUDeviceProfileModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the edit OBU header
     */
    private static final String EDIT_OBU_HEADER_TEXT = "Edit OBU Device Profile";

    /**
     * Xpath to the edit OBU header
     */
    private static final String EDIT_OBU_HEADER_XPATH = TITLE_XPATH_PREFIX + EDIT_OBU_HEADER_TEXT + "']";

    /**
     * Text displayed in the edit OBU help header
     */
    private static final String EDIT_OBU_HELP_HEADER_TEXT = "Edit OBU Device Profile Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Edit the name and percentage of affected vehicles for the selected On Board Unit (OBU) device profile. The device profile name must be unique in the composite and the total percentage for all OBU device profiles cannot exceed 100 percent.";

    /**
     * Xpath prefix to edit OBU buttons
     */
    private static final String EDIT_OBU_BTN_XPATH_PREFIX = "//a[contains(@id, 'obu-Edit')]//span[contains(@id, 'button')][text()='";

    ///////////
    //Getters
    ///////////

    /**
     * The xpath to get from the form content window to the help icon
     *
     * @return the help icon
     */
    private El getEditOBUHelpIcon() {
        return getHelpIcon(EDIT_OBU_HEADER_TEXT);
    }

    /**
     * Gets the given button in the edit OBU modal
     *
     * @param btn - the button to get
     * @return the button element
     */
    private El getEditOBUBtn(BtnNames btn) {
        return el(By.xpath(EDIT_OBU_BTN_XPATH_PREFIX + btn.getLabel() + "']"));
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the edit OBU header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditOBUHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_OBU_HEADER_TEXT);
    }

    /**
     * Checks to see if the edit OBU help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditOBUHelpHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_OBU_HELP_HEADER_TEXT);

    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isEditOBUHelpIconDisplayed() {
        return isHelpIconDisplayed(EDIT_OBU_HEADER_TEXT);
    }

    /**
     * Checks to see if the help content is displayed
     *
     * @return true if the help content is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isEditOBUHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditOBUHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(EDIT_OBU_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the button is displayed
     *
     * @param btnName -the name of the button expected
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditOBUBtnDisplayed(BtnNames btnName) {
        return isElementDisplayed(getEditOBUBtn(btnName));
    }

    ////////////////
    // Interaction
    ///////////////

    /**
     * Click the edit OBU help icon
     */
    public void clickEditOBUHelp() {
        getEditOBUHelpIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickEditOBUHelpOKBtn() {
        clickHelpOKBtn(EDIT_OBU_HELP_HEADER_TEXT);
    }

    /**
     * Click the update button
     *
     * @param success -true if success is expected, false if error expected
     * @return the newly loaded Configure OBU Device Profile Partial Page (if
     *         success), null (if error expected)
     */
    public ConfigureOBUDeviceProfilesPartialPage clickUpdate(boolean success) {
        getEditOBUBtn(BtnNames.UPDATE).click();
        if (success) {
            waitForElementToBeInvisible(By.xpath(EDIT_OBU_HEADER_XPATH));
            return getPage(ConfigureOBUDeviceProfilesPartialPage.class);
        }
        else{
            return null;
        }
    }

    /**
     * Click the reset button
     */
    @Override
    public void clickReset() {
        getEditOBUBtn(BtnNames.RESET).click();
    }

    /**
     * Click the cancel button
     *
     * @return the newly loaded Configure OBU Device Profiles Partial Page
     */
    @Override
    public ConfigureOBUDeviceProfilesPartialPage clickCancel() {
        getEditOBUBtn(BtnNames.CANCEL).click();
        waitForElementToBeInvisible(By.xpath(EDIT_OBU_HEADER_XPATH));
        return getPage(ConfigureOBUDeviceProfilesPartialPage.class);
    }

    /**
     * Clicks the Close icon associated with the Edit OBU modal
     */
    @Override
    public void clickCloseIcon() {
        clickCloseIcon(EDIT_OBU_HEADER_TEXT);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkEditOBUHeaderIcons() {
        checkHeaderIcons(EDIT_OBU_HEADER_TEXT);
    }

    /**
     * Checks that help modal is displayed -- checks both the header and content
     * text
     */
    public void checkEditOBUHelpModal() {
        Assert.assertTrue("Edit OBU Device Profile Help header not displayed as expected.", isEditOBUHelpHeaderDisplayed());
        Assert.assertTrue("Edit OBU Device Profile Help content not displayed as expected.", isEditOBUHelpContentDisplayed());
    }

    /**
     * Checks for the presence of the Update, Reset, and Cancel buttons
     */
    public void checkEditOBUBtns() {
        Assert.assertTrue("The Update button is not displayed.", isEditOBUBtnDisplayed(BtnNames.UPDATE));
        Assert.assertTrue("The Reset button is not displayed.", isEditOBUBtnDisplayed(BtnNames.RESET));
        Assert.assertTrue("The Cancel button is not displayed.", isEditOBUBtnDisplayed(BtnNames.CANCEL));
    }

    //////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(EDIT_OBU_HEADER_XPATH), 20);
    }

}
