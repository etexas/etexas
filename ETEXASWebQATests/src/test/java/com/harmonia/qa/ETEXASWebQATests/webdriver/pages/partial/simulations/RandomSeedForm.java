package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulations;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the random seed form displayed when enter random seed
 * is selected from Execute button
 *
 * @author llaroussini
 */
public class RandomSeedForm extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public RandomSeedForm(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the random seed header
     */
    private static final String RANDOM_SEED_HEADER_TEXT = "Enter a number to use as a random seed";

    /**
     * Text displayed in the random seed help header
     */
    private static final String RANDOM_SEED_HELP_HEADER_TEXT = "Random Seed Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "This dialog allows you to choose the random seed number for the simulation. Enter the random seed number.";

    /**
     * Text associated with setting up execution alert
     */
    private static final String SETTING_UP_EXECUTION_ALERT_TEXT = "Setting Up Execution";

    /**
     * The xpath of the random seed text box
     */
    private static final String RANDOM_SEED_TEXT_BOX_XPATH = "//input[@name='rSeed']";

    /**
     * Name of the Create button
     */
    private static final String CREATE_BTN_NAME = "Create";

    /**
     * Name of the Reset button
     */
    private static final String RESET_BTN_NAME = "Reset";

    /**
     * Name of the Cancel button
     */
    private static final String CANCEL_BTN_NAME = "Cancel";

    /**
     * Random Seed field name as displayed in UI
     */
    private static final String RANDOM_SEED_NAME_FIELD_DISPLAYED_NAME = "Random Seed";

    /**
     * Error displayed when invalid random seed is used
     */
    private static final String INVALID_RANDOM_SEED_ERROR_XPATH = "//div[contains(@data-errorqtip, 'the random seed must be a positive, numerical value')]/ancestor::div//span[contains(text(),'Random Seed')]/ancestor::div[contains(@class, 'x-field')]//input";

    ///////////
    //Getters
    ///////////

    /**
     * The xpath to get from the template form content window to the help icon
     *
     * @return the help icon
     */
    private El getRandomSeedHelpIcon() {
        return getHelpIcon(RANDOM_SEED_HEADER_TEXT);
    }

    /**
     * Gets the random text box
     *
     * @return the random text box
     */
    private El getRandomSeedTextBox() {
        return el(By.xpath(RANDOM_SEED_TEXT_BOX_XPATH));
    }

    /**
     * Gets the create button
     *
     * @return the create button
     */
    private El getCreateBtn() {
        return getFormBtn(RANDOM_SEED_HEADER_TEXT, CREATE_BTN_NAME);
    }

    /**
     * Gets the reset button
     *
     * @return the reset button
     */
    private El getResetBtn() {
        return getFormBtn(RANDOM_SEED_HEADER_TEXT, RESET_BTN_NAME);
    }

    /**
     * Gets the cancel button
     *
     * @return the cancel button
     */
    private El getCancelBtn() {
        return getFormBtn(RANDOM_SEED_HEADER_TEXT, CANCEL_BTN_NAME);
    }

    /**
     * Gets By associated with 'Setting Up Simulation' alert
     *
     * @return by for alert window
     */
    private By settingUpAlert() {
        return By.xpath(CONTENT_AREA_XPATH_PREFIX + SETTING_UP_EXECUTION_ALERT_TEXT + "')]");
    }

    /**
     * Gets the invalid random seed error
     *
     * @return the invalid random seed error
     */
    private El getInvalidRandomSeedError() {
        return el(By.xpath(INVALID_RANDOM_SEED_ERROR_XPATH));
    }

    /**
     * Gets the value in the random seed text box
     *
     * @return the displayed random seed value
     */
    public String getRandomSeed() {
        return getRandomSeedTextBox().getAttribute("value");
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the random seed header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isRandomSeedHeaderDisplayed() {
        return isHeaderDisplayed(RANDOM_SEED_HEADER_TEXT);
    }

    /**
     * Checks to see if the random seed help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isRandomSeedHelpHeaderDisplayed() {
        return isHeaderDisplayed(RANDOM_SEED_HELP_HEADER_TEXT);

    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isRandomSeedHelpIconDisplayed() {
        return isHelpIconDisplayed(RANDOM_SEED_HEADER_TEXT);
    }

    /**
     * Checks to see if the random seed text box is displayed
     *
     * @return true if the random seed text box is displayed, false if it is not
     *         or cannot be found
     */
    public boolean isRandomSeedTextBoxDisplayed() {
        try {
            return getRandomSeedTextBox().isDisplayed();
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks to see if the create button is displayed
     *
     * @return true if the create button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCreateBtnDisplayed() {
        return isBtnDisplayed(RANDOM_SEED_HEADER_TEXT, CREATE_BTN_NAME);
    }

    /**
     * Checks to see if the reset button is displayed
     *
     * @return true if the reset button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isResetBtnDisplayed() {
        return isBtnDisplayed(RANDOM_SEED_HEADER_TEXT, RESET_BTN_NAME);

    }

    /**
     * Checks to see if the cancel button is displayed
     *
     * @return true if the cancel button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCancelBtnDisplayed() {
        return isBtnDisplayed(RANDOM_SEED_HEADER_TEXT, CANCEL_BTN_NAME);

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
        return isHelpOKBtnDisplayed(RANDOM_SEED_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if Random Seed field required error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isRandomSeedRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(RANDOM_SEED_NAME_FIELD_DISPLAYED_NAME);
    }

    /**
     * Checks to see if Invalid Number Random Seed error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidNumberRandomSeedErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(RANDOM_SEED_NAME_FIELD_DISPLAYED_NAME);
    }

    /**
     * Checks to see if Invalid Random Seed error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidRandomSeedErrorDisplayed() {
        return isElementDisplayed(getInvalidRandomSeedError());
    }

    /**
     * Checks to see if Maximum Random Seed error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isMaximumRandomSeedErrorDisplayed() {
        return isMaxmimumNumberErrorDisplayed(RANDOM_SEED_NAME_FIELD_DISPLAYED_NAME);
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Sets the random seed value
     *
     * @param seed -the value to set for the random seed
     */
    public void setRandomSeed(String seed) {
        getRandomSeedTextBox().setText(seed);
    }

    /**
     * Click the Create button
     */
    public void clickCreate() {
        getCreateBtn().click();
    }

    /**
     * Click the Create button, waits for execution setup alert to disappear
     */
    public void clickCreateAndWait() {
        clickCreate();
        waitUntilAlertIsPresent(settingUpAlert(), PAGE_LOAD_TIMEOUT);
        waitUntilAlertIsNotPresent(settingUpAlert(), PAGE_LOAD_TIMEOUT);
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
     * Click the Random Seed Help icon
     */
    public void clickRandomSeedHelp() {
        getRandomSeedHelpIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(RANDOM_SEED_HELP_HEADER_TEXT);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkRandomSeedHeaderIcons() {
        checkHeaderIcons(RANDOM_SEED_HEADER_TEXT);
    }

    /**
     * Checks for the presence of the Random Seed text box
     */
    public void checkFields() {
        Assert.assertTrue("The Random Seed text box is not displayed.", isRandomSeedTextBoxDisplayed());
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
     * Checks that displayed random seed value matches the expected random seed
     * value
     *
     * @param seed -the random seed value expected
     */
    public void checkRandomSeedField(String seed) {
        Assert.assertEquals("Displayed random seed does not match expected random seed.", seed, getRandomSeed());
    }

    /**
     * Verifies Field Required error icon/tooltip displayed with Random Seed
     * text box
     */
    public void checkRandomSeedFieldRequiredErrorDisplayed() {
        Assert.assertTrue("Field required error not displayed as expected with Random Seed text box.", isRandomSeedRequiredErrorDisplayed());
    }

    //////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(TITLE_XPATH_PREFIX + RANDOM_SEED_HEADER_TEXT + "')]"));
    }

}
