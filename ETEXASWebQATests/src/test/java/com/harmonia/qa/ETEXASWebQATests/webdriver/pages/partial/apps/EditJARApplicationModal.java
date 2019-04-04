package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.UserJarApp;
import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing Edit A Native Application Modal
 *
 * @author llaroussini
 */
public class EditJARApplicationModal extends CreateJARApplicationModal {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public EditJARApplicationModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the Edit JAR Application header
     */
    private static final String EDIT_JAR_APP_HEADER_TEXT = "Edit JAR Application";

    /**
     * Xpath for Edit JAR Application header
     */
    private static final String EDIT_JAR_APP_HEADER_XPATH = "//div[contains(@id, 'title')][text()='" + EDIT_JAR_APP_HEADER_TEXT + "']";

    /**
     * Text displayed in the Edit JAR Application Help header
     */
    private static final String EDIT_JAR_APP_HELP_HEADER_TEXT = "Edit JAR Application Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Edit the name and file name for the selected JAR application. The application name and file name must be unique.";

    /**
     * Xpath to Application Name text box
     */
    private static final String APP_NAME_TEXT_BOX_XPATH = "//input[@name='applicationProfileName']";

    /////////////
    // Getters
    /////////////

    /**
     * Gets the update button
     *
     * @return the update button
     */
    private El getUpdateBtn() {
        return getFormBtn(EDIT_JAR_APP_HEADER_TEXT, BtnNames.UPDATE.getLabel());
    }

    /**
     * Gets the reset button
     *
     * @return the reset button
     */
    private El getResetBtn() {
        return getFormBtn(EDIT_JAR_APP_HEADER_TEXT, BtnNames.RESET.getLabel());
    }

    /**
     * Gets the cancel button
     *
     * @return the cancel button
     */
    private El getCancelBtn() {
        return getFormBtn(EDIT_JAR_APP_HEADER_TEXT, BtnNames.CANCEL.getLabel());
    }

    /**
     * Gets the edit JAR application help icon
     *
     * @return the help icon
     */
    private El getEditJARAppHelpIcon() {
        return getHelpIcon(EDIT_JAR_APP_HEADER_TEXT);
    }

    /**
     * Gets the (Application) Name text box
     *
     * @return the Application Name text box
     */
    private El getAppNameTextBox() {
        return el(By.xpath(APP_NAME_TEXT_BOX_XPATH));
    }

    /**
     * Gets the value in the (Application) Name text box
     *
     * @return the displayed (Application) Name value
     */
    public String getDisplayedAppName() {
        return getAppNameTextBox().getAttribute(VALUE_ATTRIBUTE);
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the edit JAR app form header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditJARAppHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_JAR_APP_HEADER_TEXT);
    }

    /**
     * Checks to see if the edit JAR app help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditJARAppHelpHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_JAR_APP_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isEditNativeAppHelpIconDisplayed() {
        return isHelpIconDisplayed(EDIT_JAR_APP_HEADER_TEXT);
    }

    /**
     * Checks to see if the close icon is displayed
     *
     * @return true if the close icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isEditNativeAppCloseIconDisplayed() {
        return isCloseIconDisplayed(EDIT_JAR_APP_HEADER_TEXT);
    }

    /**
     * Checks to see if the Application Name text box is displayed
     *
     * @return true if the text box is displayed, false otherwise
     */
    public boolean isApplicationNameTextboxDisplayed() {
        return isElementDisplayed(getAppNameTextBox());
    }

    /**
     * Checks to see if the update button is displayed
     *
     * @return true if the update button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isUpdateBtnDisplayed() {
        return isElementDisplayed(getUpdateBtn());
    }

    /**
     * Checks to see if the reset button is displayed
     *
     * @return true if the reset button is displayed, false if it is not or
     *         cannot be found
     */
    @Override
    public boolean isResetBtnDisplayed() {
        return isElementDisplayed(getResetBtn());
    }

    /**
     * Checks to see if the cancel button is displayed
     *
     * @return true if the cancel button is displayed, false if it is not or
     *         cannot be found
     */
    @Override
    public boolean isCancelBtnDisplayed() {
        return isElementDisplayed(getCancelBtn());
    }

    /**
     * Checks to see if the help content is displayed
     *
     * @return true if the help content is displayed, false if it is not or
     *         cannot be found
     */
    @Override
    public boolean isHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    @Override
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(EDIT_JAR_APP_HELP_HEADER_TEXT);
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Sets the given text in the (Application) Name text box
     *
     * @param text -the text to set
     */
    public void setAppName(String text) {
        getAppNameTextBox().setText(text);
    }

    /**
     * Click the Update button
     *
     * @return the newly loaded/updated JAR Applications partial page
     */
    public JARApplicationsPartialPage clickUpdate() {
        getUpdateBtn().click();
        waitForElementToBeInvisible(By.xpath(EDIT_JAR_APP_HEADER_XPATH));
        return getPage(JARApplicationsPartialPage.class);
    }

    /**
     * Click the Reset button
     */
    @Override
    public void clickResetBtn() {
        getResetBtn().click();
    }

    /**
     * Click the Cancel button
     *
     * @return the newly loaded/updated JAR Applications partial page
     */
    @Override
    public JARApplicationsPartialPage clickCancelBtn() {
        getCancelBtn().click();
        return getPage(JARApplicationsPartialPage.class);
    }

    /**
     * Click the Edit JAR Application Help icon
     */
    public void clickEditJARAppHelpIcon() {
        getEditJARAppHelpIcon().click();
    }

    /**
     * Clicks the Close icon
     *
     * @return the newly loaded/updated JAR Applications partial page
     */
    @Override
    public JARApplicationsPartialPage clickCloseIcon() {
        clickCloseIcon(EDIT_JAR_APP_HEADER_TEXT);
        return getPage(JARApplicationsPartialPage.class);

    }

    /**
     * Clicks the OK button in the help window
     */
    @Override
    public void clickHelpOKBtn() {
        clickHelpOKBtn(EDIT_JAR_APP_HELP_HEADER_TEXT);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkEditJARAppHeaderIcons() {
        checkHeaderIcons(EDIT_JAR_APP_HEADER_TEXT);
    }

    /**
     * Checks for presence of both the Help header and content
     */
    @Override
    public void checkHelpModal() {
        Assert.assertTrue("Edit Native Application Help header is not displayed as expected.", isEditJARAppHelpHeaderDisplayed());
        Assert.assertTrue("Edit Native Application Help content is not displayed as expected.", isHelpContentDisplayed());
    }

    /**
     * Checks for the presence of the Update, Reset, and Cancel buttons in Edit
     * Native App window
     */
    public void checkBtns() {
        Assert.assertTrue("The Update button is not displayed.", isUpdateBtnDisplayed());
        Assert.assertTrue("The Reset button is not displayed.", isResetBtnDisplayed());
        Assert.assertTrue("The Cancel button is not displayed.", isCancelBtnDisplayed());
    }

    /**
     * Verifies the following fields display: JAR Name & (Application) Name text
     * box
     */
    @Override
    public void checkFieldsDisplayed() {
        Assert.assertTrue("The JAR Name text box is not displayed in the Edit JAR Application modal.", isJARNameTextboxDisplayed());
        Assert.assertTrue("The Application Name text box is not displayed in the Edit JAR Application modal.", isApplicationNameTextboxDisplayed());
    }

    /**
     * Checks the displayed values are reset
     *
     * @param app -the user JAR app expected when values are reset
     * @param displayedAppName -the displayed app name expected when values are
     *        reset
     */
    public void checkFieldValues(UserJarApp app, String displayedAppName) {
        Assert.assertEquals("JAR Name text box was not cleared as expected.", app.getName(), getDisplayedJARName());
        Assert.assertEquals("Application Name text box was not cleared as expected.", displayedAppName, getDisplayedAppName());
    }

    //////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(EDIT_JAR_APP_HEADER_XPATH), 10);
    }

}