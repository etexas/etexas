package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the Edit Cellular Tower form
 *
 * @author llaroussini
 */
public class EditCellTowerForm extends CreateCellTowerModal {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public EditCellTowerForm(WebDriver driver) {
        super(driver);
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the edit cell tower header
     */
    private static final String EDIT_CELL_TOWER_HEADER_TEXT = "Edit Cell Tower";

    /**
     * Xpath to the edit cell tower header
     */
    private static final String EDIT_CELL_TOWER_HEADER_XPATH = TITLE_XPATH_PREFIX + EDIT_CELL_TOWER_HEADER_TEXT + "')]";

    /**
     * Text displayed in the edit cell tower help header
     */
    private static final String EDIT_CELL_TOWER_HELP_HEADER_TEXT = "Edit Cell Tower Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Edit the coordinates and cellular service provider for the selected cell tower. The x (cm), y (cm), and z (cm) coordinates are specified relative to the center point of the simulation, and the cellular service provider is optional.";

    /**
     * Xpath prefix to edit cell tower buttons
     */
    private static final String EDIT_CELL_TOWER_BTN_XPATH_PREFIX = "//a[contains(@id, 'celltower')][contains(@id, 'Edit')]//span[contains(@id, 'button')][text()='";

    ///////////
    //Getters
    ///////////

    /**
     * The xpath to get from the form content window to the help icon
     *
     * @return the help icon
     */
    private El getEditCellTowerHelpIcon() {
        return getHelpIcon(EDIT_CELL_TOWER_HEADER_TEXT);
    }

    /**
     * Gets the given button in the edit cell tower window
     *
     * @param btn - the button to get
     * @return the button element
     */
    private El getBtn(BtnNames btn) {
        return el(By.xpath(EDIT_CELL_TOWER_BTN_XPATH_PREFIX + btn.getLabel() + "']"));
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the edit cell tower header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditCellTowerHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_CELL_TOWER_HEADER_TEXT);
    }

    /**
     * Checks to see if the edit cell tower help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditCellTowerHelpHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_CELL_TOWER_HELP_HEADER_TEXT);

    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isEditCellTowerHelpIconDisplayed() {
        return isHelpIconDisplayed(EDIT_CELL_TOWER_HEADER_TEXT);
    }

    /**
     * Checks to see if the help content is displayed
     *
     * @return true if the help content is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isEditCellTowerHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditCellTowerHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(EDIT_CELL_TOWER_HELP_HEADER_TEXT);
    }

    /**
     * Click the edit cell tower help icon
     */
    public void clickEditCellTowerHelp() {
        getEditCellTowerHelpIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickEditCellTowerHelpOKBtn() {
        clickHelpOKBtn(EDIT_CELL_TOWER_HELP_HEADER_TEXT);
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
            waitForElementToBeInvisible(By.xpath(EDIT_CELL_TOWER_HEADER_XPATH));
            ETexasCommonUtils.sleep(500); //ZZZ -allows time for cell tower list to refresh
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
     * @return the newly loaded Configure Cell Tower Form
     */
    @Override
    public ConfigureCellTowersPartialPage clickCancel() {
        getBtn(BtnNames.CANCEL).click();
        waitForElementToBeInvisible(By.xpath(EDIT_CELL_TOWER_HEADER_XPATH));
        return getPage(ConfigureCellTowersPartialPage.class);
    }

    /**
     * Clicks the Close icon associated with the Edit Cell Tower modal
     */
    public void clickCloseIcon() {
        clickCloseIcon(EDIT_CELL_TOWER_HEADER_TEXT);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkEditCellTowerHeaderIcons() {
        checkHeaderIcons(EDIT_CELL_TOWER_HEADER_TEXT);
    }

    /**
     * Checks that help modal is displayed -- checks both the header and content
     * text
     */
    @Override
    public void checkHelpModal() {
        Assert.assertTrue("Edit Cell Tower Help header not displayed as expected.", isEditCellTowerHelpHeaderDisplayed());
        Assert.assertTrue("Edit Cell Tower Help content not displayed as expected.", isEditCellTowerHelpContentDisplayed());
    }

    /**
     * Checks for the presence of the Update, Reset, and Cancel buttons
     */
    public void checkEditCellTowerBtns() {
        Assert.assertTrue("The Update button is not displayed.", isBtnDisplayed(BtnNames.UPDATE));
        Assert.assertTrue("The Reset button is not displayed.", isBtnDisplayed(BtnNames.RESET));
        Assert.assertTrue("The Cancel button is not displayed.", isBtnDisplayed(BtnNames.CANCEL));
    }
}
