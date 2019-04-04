package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.UserRemoteApp;
import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.AppsPage;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing Edit A Remote Application Modal
 *
 * @author rsmith
 */
public class EditRemoteApplicationModal extends CreateRemoteApplicationModal {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public EditRemoteApplicationModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    public enum EditRemoteAppField {
        /**
         * Remote App Name field name
         */
        NAME("applicationName", "Name:");

        /**
         * The name attribute of the fields in the Create remote App window
         */
        private String name;

        /**
         * The label of the fields in the Create remote App window
         */
        private String label;

        /**
         * Default constructor
         *
         * @param name The string to set as the name attribute
         * @param label The string to set as the field label
         */
        EditRemoteAppField(String name, String label) {
            this.name = name;
            this.label = label;
        }

        /**
         * Gets the name attribute of the field in the Create remote App window
         *
         * @return The name attribute of the field
         */
        public String getName() {
            return this.name;
        }

        /**
         * Gets the label of the field in the Create App window
         *
         * @return The label of the field
         */
        public String getLabel() {
            return this.label;
        }
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the Edit Remote Application header
     */
    private static final String EDIT_REMOTE_APP_HEADER_TEXT = "Edit Remote Application";

    /**
     * Text displayed in the Edit Remote Application Help header
     */
    private static final String EDIT_REMOTE_APP_HELP_HEADER_TEXT = "Edit Remote Application Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Edit the name for the selected remote application. The application name must be unique across all applications. Note: changing the name of the application will not change the name of any hosted instances of the application.";

    /**
     * Xpath prefix to fields in the Edit Remote Application modal
     */
    private static final String EDIT_REMOTE_APP_FIELD_XPATH_PREFIX = "//div[contains(@id, 'edit-remote-application-profile-form')]//input[@name='";

    /**
     * The xpath for the Name text box
     */
    protected static final String NAME_TEXT_BOX_XPATH = EDIT_REMOTE_APP_FIELD_XPATH_PREFIX + "applicationProfileName']";

    /////////////
    // Getters
    /////////////

    /**
     * Gets the edit remote application help icon
     *
     * @return the help icon
     */
    private El getEditRemoteAppHelpIcon() {
        return getHelpIcon(EDIT_REMOTE_APP_HEADER_TEXT);
    }

    /**
     * Gets the Name text box
     *
     * @return the Name text box
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
        return getFormBtn(EDIT_REMOTE_APP_HEADER_TEXT, BtnNames.UPDATE.getLabel());
    }

    /**
     * Gets the reset button
     *
     * @return the reset button
     */
    private El getResetBtn() {
        return getFormBtn(EDIT_REMOTE_APP_HEADER_TEXT, BtnNames.RESET.getLabel());
    }

    /**
     * Gets the cancel button
     *
     * @return the cancel button
     */
    private El getCancelBtn() {
        return getFormBtn(EDIT_REMOTE_APP_HEADER_TEXT, BtnNames.CANCEL.getLabel());
    }

    /**
     * Gets the value in the Remote App Name text box
     *
     * @return the displayed Remote App Name value
     */
    @Override
    public String getDisplayedAppName() {
        return getNameTextBox().getAttribute("value");
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the edit remote app form header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditRemoteAppHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_REMOTE_APP_HEADER_TEXT);
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
     * Sets the text in the Name text box
     *
     * @param text the text to set in the text box
     */
    @Override
    public void setNameText(String text) {
        getNameTextBox().setText(text);
    }

    /**
     * Checks to see if the Reset button is displayed on the form
     *
     * @return true if the Reset button is displayed, false otherwise
     */
    public boolean isUpdateBtnDisplayed() {
        return isElementDisplayed(getUpdateBtn());
    }

    /**
     * Checks to see if the Reset button is displayed on the form
     *
     * @return true if the Reset button is displayed, false otherwise
     */

    @Override
    public boolean isResetBtnDisplayed() {
        return isElementDisplayed(getResetBtn());
    }

    /**
     * Checks to see if the edit remote app help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditRemoteAppHelpHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_REMOTE_APP_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false otherwise
     */
    @Override
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(EDIT_REMOTE_APP_HELP_HEADER_TEXT);
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
     * Checks to see if the Name text box is displayed
     *
     * @return true if the text box is displayed, false otherwise
     */
    @Override
    public boolean isNameTextboxDisplayed() {
        return isElementDisplayed(getNameTextBox());
    }

    /**
     * Checks to see if the Name text box is displayed
     */
    @Override
    public void checkFieldsDisplayed() {
        Assert.assertTrue("The Name text box is not displayed in the Create Remote Application modal.", isNameTextboxDisplayed());
    }

    /**
     * Verifies 'required field' error displays for all fields (checks Name and
     * Device Type fields)
     */
    @Override
    public void checkRequiredFieldErrorAllFields() {
        Assert.assertTrue("Required field error is not displayed for the Name field.", isFieldRequiredErrorDisplayed(EditRemoteAppField.NAME.getLabel()));
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkEditRemoteAppHeaderIcons() {
        checkHeaderIcons(EDIT_REMOTE_APP_HEADER_TEXT);
    }

    /**
     * Click the Edit Remote Application Help icon
     */
    public void clickEditRemoteAppHelpIcon() {
        getEditRemoteAppHelpIcon().click();
    }

    /**
     * Clicks the Reset button
     */
    @Override
    public void clickResetBtn() {
        getResetBtn().click();
    }

    /**
     * Click the Create Remote Application Close icon
     *
     * @return the newly loaded Apps Page
     */
    @Override
    public AppsPage clickCloseIcon() {
        getCloseIcon(EDIT_REMOTE_APP_HEADER_TEXT).click();
        return getPage(AppsPage.class);
    }

    /**
     * Clicks the Cancel button
     */
    @Override
    public void clickCancelBtn() {
        getCancelBtn().click();
    }

    /**
     * Click the Update button
     *
     * @param success - true if success expected, false otherwise
     * @return the newly loaded/updated Remote Applications page if success is
     *         true, otherwise return null
     */
    public RemoteApplicationsPartialPage clickUpdate(boolean success) {
        getUpdateBtn().click();
        if (success) {
            waitForElementToBeInvisible(By.xpath(NAME_TEXT_BOX_XPATH));
            return getPage(RemoteApplicationsPartialPage.class);
        }
        else {
            return null;
        }
    }

    /**
     * Clicks the OK button in the help window
     */
    @Override
    public void clickHelpOKBtn() {
        clickHelpOKBtn(EDIT_REMOTE_APP_HELP_HEADER_TEXT);
    }

    /**
     * Checks for presence of both the Help header and content
     */
    @Override
    public void checkHelpModal() {
        Assert.assertTrue("Edit Remote Application Help header is not displayed as expected.", isEditRemoteAppHelpHeaderDisplayed());
        //TODO Re-enable this test once unique IDs have been setup for Help Modals or a work-around is found.
        //Assert.assertTrue("Edit Remote Application Help content is not displayed as expected.", isHelpContentDisplayed());
    }

    /**
     * Checks that displayed values match values associated with the given app
     *
     * @param app - User Remote App expected
     */
    public void checkNameField(UserRemoteApp app) {
        String name = app.getName();
        Assert.assertEquals("Remote App Name value not displayed as expected.", name, getDisplayedAppName());
    }

    /**
     * Verifies Application Name Field error displays for the Application name
     * field. Indicating all of the acceptable values.
     */
    public void checkInvalidNameError() {
        Assert.assertTrue("Error message indicating acceptable values is not displayed for the Application Name field.", isInvalidNameErrorDisplayed());
    }

    /**
     * Checks for the presence of whitespace in the Application Name field.
     */
    public void checkWhiteSpaceNameError() {
        Assert.assertTrue("Error message informing the usage of white space is not displayed for the Application Name field.", isInvalidAppNameErrorDisplayed());
    }

    /**
     * Checks for the presence of the Create, Reset, and Cancel buttons in
     * Create Remote App window
     */
    public void checkBtns() {
        Assert.assertTrue("The Update button is not displayed.", isUpdateBtnDisplayed());
        Assert.assertTrue("The Reset button is not displayed.", isResetBtnDisplayed());
        Assert.assertTrue("The Cancel button is not displayed.", isCancelBtnDisplayed());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(NAME_TEXT_BOX_XPATH), 10);
    }
}