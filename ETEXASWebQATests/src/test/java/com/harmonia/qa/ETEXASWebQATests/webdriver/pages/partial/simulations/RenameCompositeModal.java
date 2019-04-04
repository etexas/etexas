package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulations;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the Rename Composite modal
 *
 * @author llaroussini
 */
public class RenameCompositeModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public RenameCompositeModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the Rename Composite header
     */
    private static final String RENAME_COMPOSITE_HEADER_TEXT = "Rename Composite";

    /**
     * Xpath for Rename Composite header
     */
    private static final String RENAME_COMPOSITE_HEADER_XPATH = "//div[contains(@id, 'Rename')][text()='Rename Composite']";

    /**
     * Text displayed in the Rename Composite Help header
     */
    private static final String RENAME_COMPOSITE_HELP_HEADER_TEXT = "Rename Composite Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Edit the name of the selected composite. The composite name must be unique.";

    /**
     * Text associated with updating composite alert
     */
    private static final String UPDATING_COMPOSITE_ALERT_TEXT = "Updating Composite...";

    /**
     * Xpath of the name text box
     */
    private static final String NAME_TEXT_BOX_XPATH = "//input[contains(@id, 'Rename')][@name='compositeName']";

    /**
     * Composite name field name as displayed in UI
     */
    private static final String NAME_FIELD_DISPLAYED_NAME = "Name";

    /**
     * Error text displayed when an invalid composite name is used
     */
    private static final String INVALID_COMPOSITE_NAME_ERROR_TEXT = "Composite names may contain only letters, digits, hyphens, underscores, and nonconsecutive spaces.";

    /**
     * Text prefix of duplicate composite name error
     */
    private static final String DUPLICATE_COMPOSITE_NAME_ERROR_TEXT_PREFIX = "A composite with the name \"";

    /**
     * Text suffix of duplicate composite name error
     */
    private static final String DUPLICATE_COMPOSITE_NAME_ERROR_TEXT_SUFFIX = "\" already exists.";

    ///////////
    //Getters
    ///////////

    /**
     * The xpath to get from the modal header to the help icon
     *
     * @return the help icon
     */
    private El getHelpIcon() {
        return getHelpIcon(RENAME_COMPOSITE_HEADER_TEXT);
    }

    /**
     * Gets the name text box
     *
     * @return the name text box
     */
    private El getNameTextBox() {
        return el(By.xpath(NAME_TEXT_BOX_XPATH));
    }

    /**
     * Gets the update button
     *
     * @return the update button
     */
    private El getUpdateBtn() {
        return getFormBtn(RENAME_COMPOSITE_HEADER_TEXT, BtnNames.UPDATE.getLabel());
    }

    /**
     * Gets the reset button
     *
     * @return the reset button
     */
    private El getResetBtn() {
        return getFormBtn(RENAME_COMPOSITE_HEADER_TEXT, BtnNames.RESET.getLabel());
    }

    /**
     * Gets the cancel button
     *
     * @return the cancel button
     */
    private El getCancelBtn() {
        return getFormBtn(RENAME_COMPOSITE_HEADER_TEXT, BtnNames.CANCEL.getLabel());
    }

    /**
     * Gets By associated with 'Updating Composite' alert
     *
     * @return by for alert window
     */
    private By renamingCompositeAlert() {
        return By.xpath(CONTENT_AREA_XPATH_PREFIX + UPDATING_COMPOSITE_ALERT_TEXT + "')]");
    }

    /**
     * Gets the value in the name text box
     *
     * @return the displayed Name value
     */
    public String getName() {
        return getNameTextBox().getAttribute("value");
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the rename composite header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isRenameCompositeHeaderDisplayed() {
        return isHeaderDisplayed(RENAME_COMPOSITE_HEADER_TEXT);
    }

    /**
     * Checks to see if the rename composite help header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isRenameCompositeHelpHeaderDisplayed() {
        return isHeaderDisplayed(RENAME_COMPOSITE_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the rename composite help content is displayed
     *
     * @return true if the help content is displayed, false otherwise
     */
    public boolean isRenameCompositeHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false otherwise
     */
    public boolean isRenameCompositeHelpIconDisplayed() {
        return isHelpIconDisplayed(RENAME_COMPOSITE_HEADER_TEXT);
    }

    /**
     * Checks to see if the name text box is displayed
     *
     * @return true if the name text box is displayed, false otherwise
     */
    public boolean isNameTextBoxDisplayed() {
        return isElementDisplayed(getNameTextBox());

    }

    /**
     * Checks to see if the update button is displayed
     *
     * @return true if the update button is displayed, false otherwise
     */
    public boolean isUpdateBtnDisplayed() {
        return isElementDisplayed(getUpdateBtn());
    }

    /**
     * Checks to see if the reset button is displayed
     *
     * @return true if the reset button is displayed, false otherwise
     */
    public boolean isResetBtnDisplayed() {
        return isElementDisplayed(getResetBtn());

    }

    /**
     * Checks to see if the cancel button is displayed
     *
     * @return true if the cancel button is displayed, false otherwise
     */
    public boolean isCancelBtnDisplayed() {
        return isElementDisplayed(getCancelBtn());

    }

    /**
     * Checks to see if the help content is displayed
     *
     * @return true if the help content is displayed, false otherwise
     */
    public boolean isHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false otherwise
     */
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(RENAME_COMPOSITE_HELP_HEADER_TEXT);
    }

    /**
     * Checks if Name Required error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isNameRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(NAME_FIELD_DISPLAYED_NAME);
    }

    /**
     * Checks if Invalid Composite Name error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidCompositeNameErrorDisplayed() {
        return isErrorToolTipDisplayed(INVALID_COMPOSITE_NAME_ERROR_TEXT);
    }

    /**
     * Checks if Leading/Trailing Whitespace error is displayed with Composite
     * Name field
     *
     * @return true if displayed, false otherwise
     */
    public boolean isLeadingTrailingWhitespaceCompositeNameErrorDisplayed() {
        return isWhitespaceErrorDisplayed(NAME_FIELD_DISPLAYED_NAME);
    }

    /**
     * Checks if Duplicate Composite Name error is displayed
     *
     * @param compositeName -duplicated composite name
     * @return true if displayed, false otherwise
     */
    public boolean isDuplicateCompositeNameErrorDisplayed(String compositeName) {
        return isErrorToolTipDisplayed(DUPLICATE_COMPOSITE_NAME_ERROR_TEXT_PREFIX + compositeName + DUPLICATE_COMPOSITE_NAME_ERROR_TEXT_SUFFIX);
    }

    /**
     * Checks if Duplicate Composite Name error is displayed
     *
     * @param composite -duplicated composite
     * @return true if displayed, false otherwise
     */
    public boolean isDuplicateCompositeNameErrorDisplayed(CompositeSimulation composite) {
        String compositeName = composite.getName();
        return isDuplicateCompositeNameErrorDisplayed(compositeName);
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Sets the name of the composite and checks field to ensure text is
     * successfully set
     *
     * @param name -the name of the composite to set
     */
    public void setName(String name) {
        getNameTextBox().setText(name);
        checkNameField(name);
    }

    /**
     * Sets the name associated with the given composite
     *
     * @param composite -the composite whose name should be set
     */
    public void setName(CompositeSimulation composite) {
        String name = composite.getName();
        getNameTextBox().setText(name);
    }

    /**
     * Clicks the Update button and waits for updating composite alert to
     * disappear
     *
     * @param success -true if success is expected, false if error is expected
     * @return the newly loaded Simulations Page (if success)
     */
    public SimulationsPage clickUpdate(boolean success) {
        getUpdateBtn().click();
        if (success) {
            waitUntilAlertIsNotPresent(renamingCompositeAlert(), PAGE_LOAD_TIMEOUT);
            return getPage(SimulationsPage.class);
        }
        else {
            return null;
        }
    }

    /**
     * Click the Reset button
     */
    public void clickReset() {
        getResetBtn().click();
    }

    /**
     * Click the Cancel button
     *
     * @return the newly loaded simulations page
     */
    public SimulationsPage clickCancel() {
        getCancelBtn().click();
        return getPage(SimulationsPage.class);
    }

    /**
     * Clicks the Help icon
     */
    public void clickHelpIcon() {
        getHelpIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(RENAME_COMPOSITE_HELP_HEADER_TEXT);
        waitForElementToBeInvisible(By.xpath(TITLE_XPATH_PREFIX + RENAME_COMPOSITE_HELP_HEADER_TEXT + "')]"));
    }

    /**
     * Clicks the close icon
     */
    public void clickCloseIcon() {
        clickCloseIcon(RENAME_COMPOSITE_HEADER_TEXT);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkHeaderIcons() {
        checkHeaderIcons(RENAME_COMPOSITE_HEADER_TEXT);
    }

    /**
     * Checks that help modal is displayed -- checks both the header and content
     * text
     */
    public void checkHelpModal() {
        Assert.assertTrue("Rename Compsoite Help header not displayed as expected.", isRenameCompositeHelpHeaderDisplayed());
        Assert.assertTrue("Rename Compsoite Help content not displayed as expected.", isRenameCompositeHelpContentDisplayed());
    }

    /**
     * Checks for the presence of the Name text box
     */
    public void checkFields() {
        Assert.assertTrue("The Name text box is not displayed.", isNameTextBoxDisplayed());
    }

    /**
     * Checks for the presence of the Update, Reset, and Cancel buttons
     */
    public void checkBtns() {
        Assert.assertTrue("The Update button is not displayed.", isUpdateBtnDisplayed());
        Assert.assertTrue("The Reset button is not displayed.", isResetBtnDisplayed());
        Assert.assertTrue("The Cancel button is not displayed.", isCancelBtnDisplayed());
    }

    /**
     * Checks that displayed name matches expected name
     *
     * @param name -the name expected
     */
    public void checkNameField(String name) {
        Assert.assertEquals("Displayed name does not match expected name.", name, getName());
    }

    /**
     * Checks that displayed name matches expected name
     *
     * @param composite -the composite whose name is expected
     */
    public void checkNameField(CompositeSimulation composite) {
        String name = composite.getName();
        checkNameField(name);
    }

    //////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(RENAME_COMPOSITE_HEADER_XPATH), 20);
    }

}