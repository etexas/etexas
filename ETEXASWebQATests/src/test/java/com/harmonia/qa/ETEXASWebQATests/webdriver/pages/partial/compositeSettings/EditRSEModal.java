package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the Edit RSE Modal
 *
 * @author llaroussini
 */
public class EditRSEModal extends CreateRSEDeviceModal {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public EditRSEModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the edit RSE header
     */
    private static final String EDIT_RSE_HEADER_TEXT = "Edit RSE Device";

    /**
     * Xpath to the edit RSE header
     */
    private static final String EDIT_RSE_HEADER_XPATH = TITLE_XPATH_PREFIX + EDIT_RSE_HEADER_TEXT + "']";

    /**
     * Text displayed in the edit RSE help header
     */
    private static final String EDIT_RSE_HELP_HEADER_TEXT = "Edit RSE Device Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Edit the name and coordinates for the selected Road Side Equipment (RSE) device. The device name must be unique in the composite. The x (cm), y (cm), and z (cm) coordinates are specified relative to the center point of the simulation.";

    /**
     * Xpath prefix to edit RSE buttons
     */
    private static final String EDIT_RSE_BTN_XPATH_PREFIX = "//a[contains(@id, 'rse')][contains(@id, 'Edit')]//span[contains(@id, 'button')][text()='";

    ///////////
    //Getters
    ///////////

    /**
     * The xpath to get from the form content window to the help icon
     *
     * @return the help icon
     */
    private El getEditRSEHelpIcon() {
        return getHelpIcon(EDIT_RSE_HEADER_TEXT);
    }

    /**
     * Gets the given button in the edit RSE window
     *
     * @param btn - the button to get
     * @return the button element
     */
    private El getBtn(BtnNames btn) {
        return el(By.xpath(EDIT_RSE_BTN_XPATH_PREFIX + btn.getLabel() + "']"));
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the edit RSE header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isEditRSEHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_RSE_HEADER_TEXT);
    }

    /**
     * Checks to see if the edit RSE help header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isEditRSEHelpHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_RSE_HELP_HEADER_TEXT);

    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false otherwise
     */
    public boolean isEditRSEHelpIconDisplayed() {
        return isHelpIconDisplayed(EDIT_RSE_HEADER_TEXT);
    }

    /**
     * Checks to see if the help content is displayed
     *
     * @return true if the help content is displayed, false otherwise
     */
    public boolean isEditRSEHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false otherwise
     */
    public boolean isEditRSEHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(EDIT_RSE_HELP_HEADER_TEXT);
    }

    /**
     * Click the edit RSE help icon
     */
    public void clickEditRSEHelp() {
        getEditRSEHelpIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickEditRSEHelpOKBtn() {
        clickHelpOKBtn(EDIT_RSE_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the given button is displayed
     *
     * @param btnName -the name of the button expected
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isBtnDisplayed(BtnNames btnName) {
        return isElementDisplayed(getBtn(btnName));
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
        if (success) {
            waitForElementToBeInvisible(By.xpath(EDIT_RSE_HEADER_XPATH));
            ETexasCommonUtils.sleep(500); //ZZZ -allows time for RSE list to refresh
        }
    }

    /**
     * Clicks the Reset button
     */
    @Override
    public void clickReset() {
        getBtn(BtnNames.RESET).click();
    }

    /**
     * Clicks the Cancel button
     *
     * @return the newly loaded Configure RSE Devices Partial Page
     */
    @Override
    public ConfigureRSEDevicesPartialPage clickCancel() {
        getBtn(BtnNames.CANCEL).click();
        return getPage(ConfigureRSEDevicesPartialPage.class);
    }

    /**
     * Clicks the Close icon associated with the Edit RSE modal
     */
    @Override
    public void clickCloseIcon() {
        clickCloseIcon(EDIT_RSE_HEADER_TEXT);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkEditRSEHeaderIcons() {
        checkHeaderIcons(EDIT_RSE_HEADER_TEXT);
    }

    /**
     * Checks that help modal is displayed -- checks both the header and content
     * text
     */
    public void checkEditRSEHelpModal() {
        Assert.assertTrue("Edit RSE Help header not displayed as expected.", isEditRSEHelpHeaderDisplayed());
        Assert.assertTrue("Edit RSE Help content not displayed as expected.", isEditRSEHelpContentDisplayed());
    }

    /**
     * Checks for the presence of the Update, Reset, and Cancel buttons
     */
    public void checkEditRSEBtns() {
        Assert.assertTrue("The Update button is not displayed.", isBtnDisplayed(BtnNames.UPDATE));
        Assert.assertTrue("The Reset button is not displayed.", isBtnDisplayed(BtnNames.RESET));
        Assert.assertTrue("The Cancel button is not displayed.", isBtnDisplayed(BtnNames.CANCEL));
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(EDIT_RSE_HEADER_XPATH), 10);
    }
}
