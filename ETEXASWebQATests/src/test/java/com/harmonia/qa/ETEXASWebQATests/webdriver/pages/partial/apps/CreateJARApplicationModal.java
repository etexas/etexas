package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps;

import java.io.File;
import java.io.IOException;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.LocalFileDetector;
import org.openqa.selenium.remote.RemoteWebElement;

import com.harmonia.qa.ETEXASWebQATests.entities.UserJarApp;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.AppsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the Create JAR Application modal
 *
 * @author llaroussini
 */
public class CreateJARApplicationModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public CreateJARApplicationModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    /////////////////
    // Enumerations
    ////////////////

    /**
     * JAR App Field names
     *
     * @author llaroussini
     */
    public enum CreateJARAppField {
        /**
         * Name field name
         */
        NAME("File Name:"),
        /**
         * Device Type field name
         */
        UPLOAD("Upload:");

        /**
         * The label of the fields in the Create JAR Application modal
         */
        private String label;

        /**
         * Default constructor
         *
         * @param label The string to set as the field label
         */
        CreateJARAppField(String label) {
            this.label = label;
        }

        /**
         * Gets the label of the field in the Create JAR Application modal
         *
         * @return The label of the field
         */
        public String getLabel() {
            return this.label;
        }
    }

    /////////////////
    // Identifiers
    ////////////////

    /**
     * Text displayed in the Create Applications from JAR header
     */
    private static final String CREATE_JAR_APP_HEADER_TEXT = "Upload JAR Applications";

    /**
     * Text displayed in the Create Applications from JAR Help header
     */
    private static final String CREATE_JAR_APP_HELP_HEADER_TEXT = "Upload JAR Applications Help";

    /**
     * Text displayed as the content of Create Applications from JAR Help
     */
    private static final String CREATE_JAR_APP_HELP_CONTENT_TEXT = "Upload a group of applications from the specified JAR file. The file name must be unique, and will be used to group applications uploaded from the same file.";

    /**
     * Xpath prefix for all buttons in Create Applications from JAR modal
     */
    private static final String BTN_XPATH_PREFIX = "//div[contains(@id, 'upload-jar-application-profile-form')]/div[contains(@id, 'toolbar')]//span[text()='";

    /**
     * Xpath suffix for all buttons in Create Applications from JAR modal
     */
    private static final String BTN_XPATH_SUFFIX = "']/ancestor::a";

    /**
     * Xpath to Create button
     */
    private static final String UPLOAD_BTN_XPATH = BTN_XPATH_PREFIX + "Upload" + BTN_XPATH_SUFFIX;

    /**
     * Xpath to Edit button
     */
    private static final String RESET_BTN_XPATH = BTN_XPATH_PREFIX + "Reset" + BTN_XPATH_SUFFIX;

    /**
     * Xpath to Delete button
     */
    private static final String CANCEL_BTN_XPATH = BTN_XPATH_PREFIX + "Cancel" + BTN_XPATH_SUFFIX;

    /**
     * Xpath to JAR Name text box
     */
    private static final String JAR_NAME_TEXT_BOX_XPATH = "//input[contains(@id, 'file-name-field')]";

    /**
     * Xpath to Upload field
     */
    private static final String UPLOAD_FIELD_XPATH = "//input[contains(@id, 'file-field')]";

    /**
     * Xpath to Browse button
     */
    private static final String BROWSE_BTN_XPATH = "//span[text()='Browse...']";

    /**
     * Xpath to Browse button
     */
    private static final String BROWSE_SELECTOR_XPATH = BROWSE_BTN_XPATH + "/ancestor::div[contains(@class, 'x-form-file-btn')]//input";

    /**
     * By locator suitable for locating the Alert displayed when uploading a JAR
     * app
     */
    private static final By UPLOADING_APP_ALERT_LOCATOR = By.xpath(".//div[contains(@id,'messagebox')][contains(text(),'Uploading Remote App')]");

    /**
     * Error text prefix displayed when duplicate app name is used
     */
    private static final String DUPLICATE_APP_NAME_ERROR_TEXT_PREFIX = "A JAR applications file with the name \"";

    /**
     * Error text suffix for a duplicate App name
     */
    private static final String DUPLICATE_APP_NAME_ERROR_TEXT_SUFFIX = "\" already exists.";

    /**
     * Error text for an invalid file type
     */
    private static final String INVALID_FILE_TYPE_TEXT = "A valid applications (.jar) file is required.";

    ///////////
    // Getters
    //////////

    /**
     * Gets the Create button
     *
     * @return the Create button
     */
    private El getCreateBtn() {
        return el(By.xpath(UPLOAD_BTN_XPATH));
    }

    /**
     * Gets the Reset button from this form
     *
     * @return the Reset button
     */
    private El getResetBtn() {
        return el(By.xpath(RESET_BTN_XPATH));
    }

    /**
     * Gets the Cancel button from this form
     *
     * @return the Cancel button
     */
    private El getCancelBtn() {
        return el(By.xpath(CANCEL_BTN_XPATH));
    }

    /**
     * Gets the JAR Name text box
     *
     * @return the Name text box
     */
    private El getJARNameTextBox() {
        return el(By.xpath(JAR_NAME_TEXT_BOX_XPATH));
    }

    /**
     * Gets the Upload field
     *
     * @return the Upload field
     */
    private El getUploadField() {
        return el(By.xpath(UPLOAD_FIELD_XPATH));
    }

    /**
     * Gets the Browse button
     *
     * @return the Browse button
     */
    private El getBrowseBtn() {
        return el(By.xpath(BROWSE_BTN_XPATH));
    }

    /**
     * Gets the Browse selector
     *
     * @return the Browse selector
     */
    private El getBrowseSelector() {
        return el(By.xpath(BROWSE_SELECTOR_XPATH));
    }

    /**
     * Gets the value in the JAR Name text box
     *
     * @return the displayed JAR Name value
     */
    public String getDisplayedJARName() {
        return getJARNameTextBox().getAttribute(VALUE_ATTRIBUTE);
    }

    ///////////
    // Checkers
    ///////////

    /**
     * Checks to see if the Create button is displayed
     *
     * @return true if the Create button is displayed, false otherwise
     */
    public boolean isCreateBtnDisplayed() {
        return isElementDisplayed(getCreateBtn());
    }

    /**
     * Checks to see if the Reset button is displayed on the form
     *
     * @return true if the Reset button is displayed, false otherwise
     */
    public boolean isResetBtnDisplayed() {
        return isElementDisplayed(getResetBtn());
    }

    /**
     * Checks to see if the Cancel button is displayed on the form
     *
     * @return true if the Cancel button is displayed, false otherwise
     */
    public boolean isCancelBtnDisplayed() {
        return isElementDisplayed(getCancelBtn());
    }

    /**
     * Checks to see if the Create Applications from JAR header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_JAR_APP_HEADER_TEXT);
    }

    /**
     * Checks to see if the Create Applications from JAR Help header is
     * displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isHelpHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_JAR_APP_HELP_HEADER_TEXT);

    }

    /**
     * Checks to see if the Create Applications from JAR Help content is
     * displayed
     *
     * @return true if the content text is displayed, false otherwise
     */
    public boolean isHelpContentDisplayed() {
        return isContentDisplayed(CREATE_JAR_APP_HELP_CONTENT_TEXT);

    }

    /**
     * Checks to see if the Name text box is displayed
     *
     * @return true if the text box is displayed, false otherwise
     */
    public boolean isJARNameTextboxDisplayed() {
        return isElementDisplayed(getJARNameTextBox());
    }

    /**
     * Checks to see if the Upload field is displayed
     *
     * @return true if the field is displayed, false otherwise
     */
    public boolean isUploadFieldDisplayed() {
        return isElementDisplayed(getUploadField());
    }

    /**
     * Checks to see if the Browse button is displayed
     *
     * @return true if the Browse button is displayed, false otherwise
     */
    public boolean isBrosweBtnDisplayed() {
        return isElementDisplayed(getBrowseBtn());
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false otherwise
     */
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(CREATE_JAR_APP_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if Field Required error icon/tooltip is displayed with the
     * Name text box
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isNameFieldRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(CreateJARAppField.NAME.getLabel());
    }

    /**
     * Checks to see if Field Required error icon/tooltip is displayed with the
     * Upload field
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isUploadFieldRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(CreateJARAppField.UPLOAD.getLabel());
    }

    /**
     * Checks to see if Invalid Name error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isInvalidAppNameErrorDisplayed() {
        return isInvalidNameErrorDisplayed(CreateJARAppField.NAME.getLabel());
    }

    /**
     * Checks to see if leading or trailing spaces error is displayed in Name
     * text box
     *
     * @return true if the error is displayed, otherwise false
     */
    public boolean isLeadingTrailingAppNameErrorDisplayed() {
        return isWhitespaceErrorDisplayed(CreateJARAppField.NAME.getLabel());
    }

    /**
     * Checks to see if Duplicate App Name error icon/tooltip is displayed
     *
     * @param jarAppName - the name of the duplicate Jar app
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isDuplicateAppNameErrorDisplayed(String jarAppName) {
        return isElementDisplayed(getSpecificErrorToolTip(DUPLICATE_APP_NAME_ERROR_TEXT_PREFIX + jarAppName + DUPLICATE_APP_NAME_ERROR_TEXT_SUFFIX));
    }

    /**
     * Checks to see if an error appears when using an non-jar file to create an
     * Application
     *
     * @return true if the error is displayed, otherwise false
     */
    public boolean isInvalidFileErrorDisplayed() {
        return isElementDisplayed(getSpecificErrorToolTip(INVALID_FILE_TYPE_TEXT));
    }

    ///////////
    // Interactions
    ///////////

    /**
     * Clicks the Create button and waits for window to close
     */
    public void clickCreateBtnAndWait() {
        getCreateBtn().click();
        waitForElementToBeInvisible(By.xpath(JAR_NAME_TEXT_BOX_XPATH));
    }

    /**
     * Clicks the Create button when error expected (does not wait for window to
     * close)
     */
    public void clickRegisterBtn() {
        getCreateBtn().click();
    }

    /**
     * Click the Create Applications from JAR Help icon
     */
    public void clickHelpIcon() {
        getHelpIcon(CREATE_JAR_APP_HEADER_TEXT).click();
    }

    /**
     * Click the Create Applications from JAR Close icon
     *
     * @return the newly loaded Apps Page
     */
    public AppsPage clickCloseIcon() {
        getCloseIcon(CREATE_JAR_APP_HEADER_TEXT).click();
        return getPage(AppsPage.class);
    }

    /**
     * Clicks the Reset button
     */
    public void clickResetBtn() {
        getResetBtn().click();
    }

    /**
     * Clicks the Cancel button
     *
     * @return the newly loaded Apps Page
     */
    public AppsPage clickCancelBtn() {
        getCancelBtn().click();
        return getPage(AppsPage.class);
    }

    /**
     * Sets the text in the Name text box
     *
     * @param text the text to set in the text box
     */
    public void setJARName(String text) {
        getJARNameTextBox().setText(text);
    }

    /**
     * Sets the text in the Name text box
     *
     * @param app the app whose name should be set
     */
    public void setJARName(UserJarApp app) {
        String appName = app.getName();
        setJARName(appName);
    }

    /**
     * Attempt at setting the upload file, based off NavyPal's implementation
     *
     * @param file the file to be set
     * @throws IOException if an error occurs getting the file path, likely the
     *         result of a permissions error
     */
    public void setUploadFile(File file) throws IOException {
        String path = file.getCanonicalPath();
        El browseSelector = getBrowseSelector();
        if (IS_REMOTE) {
            RemoteWebElement uploader = ((RemoteWebElement)browseSelector.webElement()); //TODO implement in automation base - El
            uploader.setFileDetector(new LocalFileDetector());
            uploader.sendKeys(path);
        }
        else {
            browseSelector.sendKeys(path);
        }
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(CREATE_JAR_APP_HELP_HEADER_TEXT);
    }

    ///////////
    // Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkHeaderIcons() {
        checkHeaderIcons(CREATE_JAR_APP_HEADER_TEXT);
    }

    /**
     * Verifies expected header and content text is displayed in the help modal
     */
    public void checkHelpModal() {
        Assert.assertTrue("Create Applications from JAR Help header not displayed after clicking help icon.", isHelpHeaderDisplayed());
        Assert.assertTrue("Create Applications from JAR help text is not displayed. Expected: '" + CREATE_JAR_APP_HELP_CONTENT_TEXT + "'", isHelpContentDisplayed());
    }

    /**
     * Verifies the following fields display: Name text box, Upload field, and
     * Browse button
     */
    public void checkFieldsDisplayed() {
        Assert.assertTrue("The JAR Name text box is not displayed in the Create Applications from JAR modal.", isJARNameTextboxDisplayed());
        Assert.assertTrue("The Upload field is not displayed in the Create Applications from JAR modal.", isUploadFieldDisplayed());
        Assert.assertTrue("The Browse button (associated with the Upload field) is not displayed in the Create Applications from JAR modal.", isBrosweBtnDisplayed());
    }

    /**
     * Verifies the following buttons display at the bottom of the modal:
     * create, reset, cancel
     */
    public void checkFormBtns() {
        Assert.assertTrue("The Create button is not displayed in the Create Applications from JAR modal.", isCreateBtnDisplayed());
        Assert.assertTrue("The Cancel button is not displayed in the Create Applications from JAR modal.", isCancelBtnDisplayed());
        Assert.assertTrue("The Reset button is not displayed in the Create Applications from JAR modal.", isResetBtnDisplayed());
    }

    /**
     * Checks the displayed values are reset. Currently only checks JAR Name
     * field -- Upload field value is unreachable
     */
    public void checkResetValues() {
        Assert.assertEquals("JAR Name text box was not cleared as expected.", "", getDisplayedJARName());
    }

    /**
     * Verifies 'required field' error displays for all fields (checks Name and
     * Upload fields)
     */
    public void checkRequiredFieldErrorAllFields() {
        Assert.assertTrue("Required field error is not displayed for the Name field.", isFieldRequiredErrorDisplayed(CreateJARAppField.NAME.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Upload field.", isFieldRequiredErrorDisplayed(CreateJARAppField.UPLOAD.getLabel()));
    }

    //////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(UPLOAD_FIELD_XPATH));
    }

    /**
     * Waits for the Uploading JAR App alert to disappear
     */
    public void waitForAlert() {
        waitForElementToBeInvisible(UPLOADING_APP_ALERT_LOCATOR);
    }

}