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
 * Page class representing the Copy Composite modal
 *
 * @author llaroussini
 */
public class CopyCompositeModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public CopyCompositeModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the copy composite header
     */
    private static final String COPY_COMPOSITE_HEADER_TEXT = "Copy Composite";

    /**
     * Text displayed in the copy composite help header
     */
    private static final String COPY_COMPOSITE_HELP_HEADER_TEXT = "Copy Composite Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Copy the selected composite. The new composite name must be unique.";

    /**
     * Text associated with copying composite alert
     */
    private static final String COPYING_SIM_ALERT_TEXT = "Copying composite...";

    /**
     * The xpath of the Name text box
     */
    private static final String NAME_TEXT_BOX_XPATH = "//input[@name='compositeName']";

    /**
     * Xpath prefix to all buttons
     */
    private static final String COPY_COMPOSITE_FORM_BTN_XPATH_PREFIX = "//div[contains(@id, 'composite')][contains(@id, 'Copy')]//span[text()='";

    /**
     * Xpath suffix to all buttons
     */
    private static final String BTN_XPATH_SUFFIX = "']/ancestor::a";

    /**
     * Xpath to the Create button
     */
    private static final String CREATE_BTN_XPATH = COPY_COMPOSITE_FORM_BTN_XPATH_PREFIX + BtnNames.CREATE.getLabel() + BTN_XPATH_SUFFIX;

    /**
     * Xpath to the Reset button
     */
    private static final String RESET_BTN_XPATH = COPY_COMPOSITE_FORM_BTN_XPATH_PREFIX + BtnNames.RESET.getLabel() + BTN_XPATH_SUFFIX;

    /**
     * Xpath to the Cancel button
     */
    private static final String CANCEL_BTN_XPATH = COPY_COMPOSITE_FORM_BTN_XPATH_PREFIX + BtnNames.CANCEL.getLabel() + BTN_XPATH_SUFFIX;

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
     * The xpath to get from the copy composite header to the help icon
     *
     * @return the help icon
     */
    private El getCopyCompositeHelpIcon() {
        return getHelpIcon(COPY_COMPOSITE_HEADER_TEXT);
    }

    /**
     * Gets the Name text box
     *
     * @return the text box element
     */
    private El getNameTextBox() {
        return el(By.xpath(NAME_TEXT_BOX_XPATH));
    }

    /**
     * Gets the create button
     *
     * @return the create button
     */
    private El getCreateBtn() {
        return el(By.xpath(CREATE_BTN_XPATH));
    }

    /**
     * Gets the reset button
     *
     * @return the reset button
     */
    private El getResetBtn() {
        return el(By.xpath(RESET_BTN_XPATH));
    }

    /**
     * Gets the cancel button
     *
     * @return the cancel button
     */
    private El getCancelBtn() {
        return el(By.xpath(CANCEL_BTN_XPATH));
    }

    /**
     * Gets By associated with 'Copying simulation' alert
     *
     * @return by for alert window
     */
    private By settingUpAlert() {
        return By.xpath(CONTENT_AREA_XPATH_PREFIX + COPYING_SIM_ALERT_TEXT + "')]");
    }

    /**
     * Gets the value in the Name text box
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
     * Checks to see if the copy composite header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isCopyCompositeHeaderDisplayed() {
        return isHeaderDisplayed(COPY_COMPOSITE_HEADER_TEXT);
    }

    /**
     * Checks to see if the copy composite help header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isCopyCompositeHelpHeaderDisplayed() {
        return isHeaderDisplayed(COPY_COMPOSITE_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the Name text box is displayed
     *
     * @return true if the text box is displayed, false otherwise
     */
    public boolean isNameTextBoxDisplayed() {
        return isElementDisplayed(getNameTextBox());
    }

    /**
     * Checks to see if the create button is displayed
     *
     * @return true if the create button is displayed, false otherwise
     */
    public boolean isCreateBtnDisplayed() {
        return isElementDisplayed(getCreateBtn());
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
     * Checks to see if the help header is displayed
     *
     * @return true if the help header is displayed, false otherwise
     */
    public boolean isHelpHeaderDisplayed() {
        return isHeaderDisplayed(COPY_COMPOSITE_HELP_HEADER_TEXT);
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
        return isHelpOKBtnDisplayed(COPY_COMPOSITE_HELP_HEADER_TEXT);
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
     * Sets the name of the composite
     *
     * @param name -the name of the composite to set
     */
    public void setName(String name) {
        getNameTextBox().setText(name);
    }

    /**
     * Sets the name associated with the given composite
     *
     * @param composite -the composite being created
     */
    public void setName(CompositeSimulation composite) {
        String name = composite.getName();
        getNameTextBox().setText(name);
    }

    /**
     * Click the Create button
     *
     * @param success -true if success is expected, false otherwise
     * @return the newly loaded Simulations Page if success, otherwise no return
     */
    public SimulationsPage clickCreate(boolean success) {
        getCreateBtn().click();
        if (success) {
            waitUntilAlertIsNotPresent(settingUpAlert(), 60);
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
     * @return the newly loaded Simulations Page
     */
    public SimulationsPage clickCancel() {
        getCancelBtn().click();
        return getPage(SimulationsPage.class);
    }

    /**
     * Click the Copy Composite Help icon
     */
    public void clickCopyCompositeHelp() {
        getCopyCompositeHelpIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(COPY_COMPOSITE_HELP_HEADER_TEXT);
        waitForElementToBeInvisible(By.xpath(TITLE_XPATH_PREFIX + COPY_COMPOSITE_HELP_HEADER_TEXT + "']"));
    }

    /**
     * Clicks the Close icon
     *
     * @return the newly loaded Simulations Page
     */
    public SimulationsPage clickCloseIcon() {
        clickCloseIcon(COPY_COMPOSITE_HEADER_TEXT);
        return getPage(SimulationsPage.class);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkCopyCompositeHeaderIcons() {
        checkHeaderIcons(COPY_COMPOSITE_HEADER_TEXT);
    }

    /**
     * Checks for presence of expected help header and help content
     */
    public void checkHelpModal() {
        Assert.assertTrue("The Copy Simulation Help header is not displayed as expected.", isHelpHeaderDisplayed());
        Assert.assertTrue("The Copy Simulation help content is not displayed as expected", isHelpContentDisplayed());
    }

    /**
     * Checks for the presence of the Name text box
     */
    public void checkFields() {
        Assert.assertTrue("The Name text box is not displayed.", isNameTextBoxDisplayed());
    }

    /**
     * Checks for the presence of the Create, Reset, and Cancel buttons
     */
    public void checkBtns() {
        Assert.assertTrue("The Create button is not displayed.", isCreateBtnDisplayed());
        Assert.assertTrue("The Reset button is not displayed.", isResetBtnDisplayed());
        Assert.assertTrue("The Cancel button is not displayed.", isCancelBtnDisplayed());
    }

    /**
     * Checks that displayed name matches the expected name
     *
     * @param name -the name expected
     */
    public void checkNameField(String name) {
        Assert.assertEquals("Displayed name does not match expected name.", name, getName());
    }

    /**
     * Checks that displayed composite name matches the name of the composite
     * given
     *
     * @param composite -the composite expected
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
        waitForElementToBeVisible(By.xpath(CREATE_BTN_XPATH), 20);
    }
}
