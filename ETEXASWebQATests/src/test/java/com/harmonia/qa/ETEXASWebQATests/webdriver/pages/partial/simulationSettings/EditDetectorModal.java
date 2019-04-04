package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.parital.environment.ConfigureDetectorsPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.parital.environment.CreateDetectorModal;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the Edit Detector form
 *
 * @author rsmith
 */
public class EditDetectorModal extends CreateDetectorModal {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public EditDetectorModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the edit Detector header
     */
    private static final String EDIT_DETECTOR_HEADER_TEXT = "Edit Detector";

    /**
     * Xpath to the edit Detector header
     */
    private static final String EDIT_DETECTOR_HEADER_XPATH = TITLE_XPATH_PREFIX + EDIT_DETECTOR_HEADER_TEXT + "']";

    /**
     * Text displayed in the edit Detector help header
     */
    private static final String EDIT_DETECTOR_HELP_HEADER_TEXT = "Edit Detector Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Edit the lane, width (cm), height (cm), and distance (cm) from the stop line for the selected detector. ";

    /**
     * Xpath prefix to edit Detector buttons
     */
    private static final String EDIT_DETECTOR_BTN_XPATH_PREFIX = "//a[contains(@id, 'detector')][contains(@id, 'Edit')]//span[contains(@id, 'button')][text()='";

    ///////////
    //Getters
    ///////////

    /**
     * The xpath to get from the form content window to the help icon
     *
     * @return the help icon
     */
    private El getEditDetectorHelpIcon() {
        return getHelpIcon(EDIT_DETECTOR_HEADER_TEXT);
    }

    /**
     * Gets the given button in the edit Detector window
     *
     * @param btn - the button to get
     * @return the button element
     */
    private El getBtn(BtnNames btn) {
        return el(By.xpath(EDIT_DETECTOR_BTN_XPATH_PREFIX + btn.getLabel() + "']"));
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the edit Detector header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditDetectorHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_DETECTOR_HEADER_TEXT);
    }

    /**
     * Checks to see if the edit Detector help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditDetectorHelpHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_DETECTOR_HELP_HEADER_TEXT);

    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isEditDetectorHelpIconDisplayed() {
        return isHelpIconDisplayed(EDIT_DETECTOR_HEADER_TEXT);
    }

    /**
     * Checks to see if the help content is displayed
     *
     * @return true if the help content is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isEditDetectorHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditDetectorHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(EDIT_DETECTOR_HELP_HEADER_TEXT);
    }

    /**
     * Click the edit Detector help icon
     */
    public void clickEditDetectorHelp() {
        getEditDetectorHelpIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickEditDetectorHelpOKBtn() {
        clickHelpOKBtn(EDIT_DETECTOR_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the button is displayed
     *
     * @param btnName -the name of the button expected
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isBtnDisplayed(BtnNames btnName) {
        return isElementDisplayed(getBtn(btnName));
    }

    /**
     * Checks to see if the Show Lanes button is displayed
     *
     * @return true if the Show Lanes button is displayed, false if it is not or
     *         cannot be found
     */
    @Override
    public boolean isShowLanesBtnDisplayed() {
        return isElementDisplayed(getBtn(BtnNames.SHOWLANES));
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
            waitForElementToBeInvisible(By.xpath(EDIT_DETECTOR_HEADER_XPATH));
            ETexasCommonUtils.sleep(500); //ZZZ -allows time for Detector list to refresh
        }
    }

    /**
     * Click the reset button
     */
    @Override
    public void clickReset() {
        getBtn(BtnNames.RESET).click();
    }

    /**
     * Click the cancel button
     *
     * @return the newly loaded Configure Detector Form
     */
    @Override
    public ConfigureDetectorsPartialPage clickCancel() {
        getBtn(BtnNames.CANCEL).click();
        waitForElementToBeInvisible(By.xpath(EDIT_DETECTOR_HEADER_XPATH));
        return getPage(ConfigureDetectorsPartialPage.class);
    }

    /**
     * Clicks the Close icon associated with the Edit Detector modal
     */
    @Override
    public void clickCloseIcon() {
        clickCloseIcon(EDIT_DETECTOR_HEADER_TEXT);
    }

    /**
     * Clicks the Edit Detector help icon
     */
    public void clickEditDetectorHelpIcon() {
        getHelpIcon(EDIT_DETECTOR_HEADER_TEXT).click();
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkEditDetectorHeaderIcons() {
        checkHeaderIcons(EDIT_DETECTOR_HEADER_TEXT);
    }

    /**
     * Checks that help modal is displayed -- checks both the header and content
     * text
     */
    @Override
    public void checkHelpModal() {
        Assert.assertTrue("Edit Detector Help header not displayed as expected.", isEditDetectorHelpHeaderDisplayed());
        Assert.assertTrue("Edit Detector Help content not displayed as expected.", isEditDetectorHelpContentDisplayed());
    }

    /**
     * Checks for the presence of the Update, Reset, and Cancel buttons
     */
    public void checkEditDetectorBtns() {
        Assert.assertTrue("The Update button is not displayed.", isBtnDisplayed(BtnNames.UPDATE));
        Assert.assertTrue("The Reset button is not displayed.", isBtnDisplayed(BtnNames.RESET));
        Assert.assertTrue("The Cancel button is not displayed.", isBtnDisplayed(BtnNames.CANCEL));
    }

    //////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(EDIT_DETECTOR_HEADER_XPATH), 20);
    }
}
