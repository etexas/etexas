package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial;

import junit.framework.Assert;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the new user form displayed when the new user link is
 * clicked
 *
 * @author cbulloss
 */
public class NewUserForm extends BaseForm {

    ///////////
    //ID's & Locators
    ///////////

    /**
     * CSS Class assigned to required fields
     */
    private static final String REQUIRED_CSS_CLASS = "x-form-invalid-field";

    /**
     * The ID assigned to the Username Error Message
     */
    private static final String INVALID_USERNAME_ERROR_MESSAGE_NAME_XPATH = "//div[contains(@class, 'x-form-invalid-icon')][contains(@data-errorqtip, 'Usernames must start with a valid letter and may only contain letters and digits.')]";

    /**
     * The xpath assigned to the New User form header
     */
    private static final String NEW_USER_HEADER_XPATH = "//div[text()='Create a User Account for eTEXAS']";

    /**
     * Xpath prefix to fields in Registration modal
     */
    private static final String REGISTRATION_FIELD_XPATH_PREFIX = NEW_USER_HEADER_XPATH + "/ancestor::div[contains(@class, 'x-panel-default')]//input[contains(@name, '";

    /**
     * THe ID assigned to the username textbox
     */
    private static final String USERNAME_TEXTBOX_NAME = "username";

    /**
     * The ID assigned to the Password textbox
     */
    private static final String PASSWORD_TEXTBOX_NAME = "password";

    /**
     * The ID assigned to the Confirm Password Textbox
     */
    private static final String CONFIRM_PASSWORD_TEXTBOX_NAME = "confirm-password";

    /**
     * The ID assigned to the Email address textbox
     */
    private static final String EMAIL_TEXTBOX_NAME = "email";

    /**
     * The ID assigned to the First Name textbox
     */
    private static final String FIRST_NAME_TEXTBOX_NAME = "firstName";

    /**
     * The ID assigned to the Last Name textbox
     */
    private static final String LAST_NAME_TEXTBOX_NAME = "lastName";

    /**
     * The ID assigned to the Organization textbox
     */
    private static final String ORG_TEXTBOX_NAME = "organization";

    /**
     * XPath assigned to the Register button
     */
    private static final String REGISTER_BTN_XPATH = "//span[text()='Register']";

    /**
     * The xpath assigned to the Cancel button
     */
    private static final String CANCEL_BTN_XPATH = "//span[text()='Cancel']";

    /**
     * The xpath to the Registration Failed error window
     */
    private static final String REGISTRATION_FAILED_ERROR_WINDOW_XPATH = "//div[text()='Add User Failure']/ancestor::div[contains(@class, 'x-message-box')]";

    /**
     * The xpath to the username in use error message in the registration failed
     * error window
     */
    private static final String USERNAME_IN_USE_ERROR_MESSAGE_XPATH = REGISTRATION_FAILED_ERROR_WINDOW_XPATH + "//div[contains(@class, 'x-window-text')]";

    /**
     * Text prefix for username in use error message
     */
    private static final String USERNAME_IN_USE_ERROR_TEXT_PREFIX = "A user with the username \"";

    /**
     * Text suffix for username in use error message
     */
    private static final String USERNAME_IN_USE_ERROR_TEXT_SUFFIX = "\" already exists.";

    /**
     * Error text displayed when password and confirm password do not match
     */
    private static final String NON_MATCHING_PASSWORD_ERROR_TEXT = "Passwords do not match";

    /**
     * Error text displayed when invalud email is used
     */
    private static final String INVALID_EMAIL_ERROR_TEXT = "This field should be an e-mail address in the format \"user@example.com\"";

    /**
     * The displayed name of the username textbox
     */
    private static final String USERNAME_TEXTBOX_DISPLAYED_NAME = "Username";

    /**
     * The displayed name of the Password textbox
     */
    private static final String PASSWORD_TEXTBOX_DISPLAYED_NAME = "Password";

    /**
     * The displayed name of Confirm Password Textbox
     */
    private static final String CONFIRM_PASSWORD_TEXTBOX_DISPLAYED_NAME = "Confirm Password";

    /**
     * The displayed name of the Email address textbox
     */
    private static final String EMAIL_TEXTBOX_DISPLAYED_NAME = "Email Address";

    /**
     * The displayed name of the First Name textbox
     */
    private static final String FIRST_NAME_TEXTBOX_DISPLAYED_NAME = "First Name";

    /**
     * The displayed name of the Last Name textbox
     */
    private static final String LAST_NAME_TEXTBOX_DISPLAYED_NAME = "Last Name";

    /**
     * The displayed header for the Create a User Account for eTEXAS modal
     */
    private static final String CREATE_USER_ACCOUNT_HEADER = "Create a User Account for eTEXAS";

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public NewUserForm(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //Getters
    ///////////

    /**
     * Gets the new user header element
     *
     * @return the new user header element for the form
     */
    private El getHeader() {
        return el(By.xpath(NEW_USER_HEADER_XPATH));
    }

    /**
     * Gets the username textbox element
     *
     * @return the username textbox
     */
    private El getUsernameTextbox() {
        return el(By.xpath(REGISTRATION_FIELD_XPATH_PREFIX + USERNAME_TEXTBOX_NAME + "')]"));
    }

    /**
     * Gets the password textbox element
     *
     * @return the password textbox
     */
    private El getPasswordTextbox() {
        return el(By.xpath(REGISTRATION_FIELD_XPATH_PREFIX + PASSWORD_TEXTBOX_NAME + "')]"));
    }

    /**
     * Gets the confirm password textbox element
     *
     * @return the confirm password textbox
     */
    private El getConfirmPasswordTextbox() {
        return el(By.xpath(REGISTRATION_FIELD_XPATH_PREFIX + CONFIRM_PASSWORD_TEXTBOX_NAME + "')]"));
    }

    /**
     * Gets the email address textbox element
     *
     * @return the email address textbox
     */
    private El getEmailAddressTextbox() {
        return el(By.xpath(REGISTRATION_FIELD_XPATH_PREFIX + EMAIL_TEXTBOX_NAME + "')]"));
    }

    /**
     * Gets the first name textbox element
     *
     * @return the first name textbox
     */
    private El getFirstNameTextbox() {
        return el(By.xpath(REGISTRATION_FIELD_XPATH_PREFIX + FIRST_NAME_TEXTBOX_NAME + "')]"));
    }

    /**
     * Gets the last name textbox element
     *
     * @return the last name textbox
     */
    private El getLastNameTextbox() {
        return el(By.xpath(REGISTRATION_FIELD_XPATH_PREFIX + LAST_NAME_TEXTBOX_NAME + "')]"));
    }

    /**
     * Gets the invalid username Error message
     *
     * @return the invalid username error message
     */
    private El getUsernameError() {
        return el(By.xpath(INVALID_USERNAME_ERROR_MESSAGE_NAME_XPATH));
    }

    /**
     * Get's the Passwords Match Error message
     *
     * @return the error message stating that the password's do not match
     */
    private El getPasswordMatchError() {
        return el(By.xpath(NON_MATCHING_PASSWORD_ERROR_TEXT));
    }

    /**
     * Gets the organization textbox element
     *
     * @return the organization textbox
     */
    private El getOrgTextbox() {
        return el(By.xpath(REGISTRATION_FIELD_XPATH_PREFIX + ORG_TEXTBOX_NAME + "')]"));
    }

    /**
     * Gets the register button
     *
     * @return the register button
     */
    private El getRegisterBtn() {
        return el(By.xpath(REGISTER_BTN_XPATH));
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
     * Gets the Registration Failed error window
     *
     * @return the Registration Failed error window element
     */
    private El getRegistrationFailedErrorWindow() {
        return el(By.xpath(REGISTRATION_FAILED_ERROR_WINDOW_XPATH));
    }

    /**
     * Gets the Username in Use error message in Registration Failed error
     * window
     *
     * @return the Username in Use erorr message element
     */
    private El getUsernameInUseErrorMessage() {
        return el(By.xpath(USERNAME_IN_USE_ERROR_MESSAGE_XPATH));
    }

    /**
     * Gets the OK button in the Registration Error window
     *
     * @return the OK button element
     */
    private El getRegistrationErrorOKBtn() {
        return el(By.xpath(OK_BTN_XPATH));
    }

    /**
     * Gets the value in the username text box
     *
     * @return the displayed Username value
     */
    public String getDisplayedUsername() {
        return getUsernameTextbox().getAttribute("value");
    }

    /**
     * Gets the value in the password text box
     *
     * @return the displayed password value
     */
    public String getDisplayedPassword() {
        return getPasswordTextbox().getAttribute("value");
    }

    /**
     * Gets the value in the confirm password text box
     *
     * @return the displayed confirm password value
     */
    public String getDisplayedConfirmPassword() {
        return getConfirmPasswordTextbox().getAttribute("value");
    }

    /**
     * Gets the value in the email text box
     *
     * @return the displayed email address value
     */
    public String getDisplayedEmail() {
        return getEmailAddressTextbox().getAttribute("value");
    }

    /**
     * Gets the value in the first name text box
     *
     * @return the displayed name value
     */
    public String getDisplayedFirstName() {
        return getFirstNameTextbox().getAttribute("value");
    }

    /**
     * Gets the value in the last name text box
     *
     * @return the displayed name value
     */
    public String getDisplayedLastName() {
        return getLastNameTextbox().getAttribute("value");
    }

    /**
     * Gets the value in the organization text box
     *
     * @return the displayed organization value
     */
    public String getDisplayedOrganization() {
        return getOrgTextbox().getAttribute("value");
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isHeaderDisplayed() {
        return isElementDisplayed(getHeader());
    }

    /**
     * Checks to see if the username textbox is displayed
     *
     * @return true if the username textbox is found and displayed, false if it
     *         cannot be found or is not displayed
     */
    public boolean isUsernameTextboxDisplayed() {
        return isElementDisplayed(getUsernameTextbox());
    }

    /**
     * Checks to see if the password textbox is displayed
     *
     * @return true if the password textbox is located and displayed, false if
     *         it cannot be found or if it is not displayed
     */
    public boolean isPasswordTextboxDisplayed() {
        return isElementDisplayed(getPasswordTextbox());
    }

    /**
     * Checks to see if the confirm password textbox is displayed
     *
     * @return true if the confirm password textbox is located and displayed,
     *         false if it cannot be located or is not displayed
     */
    public boolean isConfirmPasswordTextboxDisplayed() {
        return isElementDisplayed(getConfirmPasswordTextbox());
    }

    /**
     * CHecks to see if the email textbox is displayed
     *
     * @return true if the email textbox is located and displayed, false if it
     *         cannot be located or is not displayed
     */
    public boolean isEmailTextboxDisplayed() {
        return isElementDisplayed(getEmailAddressTextbox());
    }

    /**
     * Checks to see if the first name textbox is displayed
     *
     * @return true if the first name textbox is located and displayed, false if
     *         it is found or visible
     */
    public boolean isFirstNameTextboxDisplayed() {
        return isElementDisplayed(getFirstNameTextbox());
    }

    /**
     * Checks to see if the last name textbox is displayed
     *
     * @return true if the last name text box is located and displayed, false if
     *         it cannot be located or is not displayed
     */
    public boolean isLastNameTextboxDisplayed() {
        return isElementDisplayed(getLastNameTextbox());
    }

    /**
     * Checks to see if the organization textbox is displayed
     *
     * @return true if the organization textbox is located and displayed, false
     *         if it cannot be located or is not displayed
     */
    public boolean isOrganizationTextboxDisplayed() {
        return isElementDisplayed(getOrgTextbox());
    }

    /**
     * Checks to see if the register button is displayed
     *
     * @return true if the register button is displayed, false if it is not or
     *         cannot be located
     */
    public boolean isRegisterBtnDisplayed() {
        return getRegisterBtn() != null;
    }

    /**
     * Checks to see if the cancel button is displayed
     *
     * @return true if the cancel button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCancelBtnDisplayed() {
        return isElementDisplayed(getCancelBtn());
    }

    /**
     * Checks to see if the close icon is displayed
     *
     * @return true if the close icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isCloseIconDisplayed() {
        return isCloseIconDisplayed(CREATE_USER_ACCOUNT_HEADER);
    }

    /**
     * Checks to see if Registration Failed window is displayed
     *
     * @return true if the window is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isRegistrationFailedWindowDisplayed() {
        return isElementDisplayed(getRegistrationFailedErrorWindow());
    }

    /**
     * Checks to see if Username In Use Error Message is displayed
     *
     * @return true if the error message is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isUsernameInUseErrorMsgDisplayed() {
        return isElementDisplayed(getUsernameInUseErrorMessage());
    }

    /**
     * Checks to see if Registration Failed window OK button is displayed
     *
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isErrorOKBtnDisplayed() {
        return isElementDisplayed(getRegistrationErrorOKBtn());
    }

    /**
     * Checks to see if Password Match error icon/tooltip is displayed
     *
     * @param the error message displayed for password error
     * @return returns true if the error message is displayed
     */
    public boolean isPasswordMatchErrorDisplayed() {
        return getPasswordMatchError() != null; //TODO check to see if there is a better way to implement this
    }

    /**
     * Checks to see if Invalid Email error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isInvalidEmailErrorDisplayed() {
        return isElementDisplayed(getSpecificErrorToolTip(INVALID_EMAIL_ERROR_TEXT));
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Sets the text in the username textbox
     *
     * @param text the text to set in the username textbox
     */
    public void setUsernameText(String text) {
        getUsernameTextbox().setText(text);
    }

    /**
     * Sets the text in the password textbox
     *
     * @param text the text to set in the password textbox
     */
    public void setPasswordText(String text) {
        getPasswordTextbox().setText(text);
    }

    /**
     * Sets the text in the confirm password textbox
     *
     * @param text the text to set in the confirm password textbox
     */
    public void setConfirmPasswordText(String text) {
        getConfirmPasswordTextbox().setText(text);
    }

    /**
     * Sets the text in the email address textbox
     *
     * @param text the text to set in the email address textbox
     */
    public void setEmailText(String text) {
        getEmailAddressTextbox().setText(text);
    }

    /**
     * Sets the text in the first name textbox
     *
     * @param text the text to set in the first name textbox
     */
    public void setFirstNameText(String text) {
        getFirstNameTextbox().setText(text);
    }

    /**
     * Sets the text in the last name textbox
     *
     * @param text the text to set in the last name textbox
     */
    public void setLastNameText(String text) {
        getLastNameTextbox().setText(text);
    }

    /**
     * Sets the text in the organization textbox
     *
     * @param text the text to set in the organization textbox
     */
    public void setOrganizationText(String text) {
        getOrgTextbox().setText(text);
    }

    /**
     * Clicks the register button
     */
    public void clickRegisterBtn() {
        getRegisterBtn().click();
    }

    /**
     * Clicks the register button assuming valid values entered on form and
     * returns success window
     *
     * @return the newly loaded register success window
     */
    public RegisterSuccessWindow clickRegisterBtnRtrnSuccess() {
        getRegisterBtn().click();
        return getPage(RegisterSuccessWindow.class);
    }

    /**
     * Clicks the cancel button
     */
    public void clickCancelBtn() {
        getCancelBtn().click();
    }

    /**
     * Clicks the exit icon
     */
    public void clickCloseIcon() {
        getCloseIcon(CREATE_USER_ACCOUNT_HEADER).click();
    }

    /**
     * Clicks the OK button in the Registration Error window
     */
    public void clickRegistrationErrorOKBtn() {
        getRegistrationErrorOKBtn().click();
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Sets the text of both the password and confirmation textboxes
     *
     * @param text the text to set in both textboxes
     */
    public void setPasswordAndConfirm(String text) {
        setPasswordText(text);
        setConfirmPasswordText(text);
    }

    /**
     * Populates the user account form with the values according to the
     * specified user
     *
     * @param user the user whose values will be populated in the form
     */
    public void populateForm(ETexasUser user) {
        setUsernameText(user.getUsername());
        setPasswordAndConfirm(user.getPassword());
        setEmailText(user.getEmailAddress());
        setFirstNameText(user.getFirstName());
        setLastNameText(user.getLastName());
        setOrganizationText(user.getOrganization());
    }

    /**
     * Performs assertions to check the form is displayed correctly.
     */
    public void checkForm() {
        Assert.assertTrue("Username field is not displayed on the new user form.", isUsernameTextboxDisplayed());
        Assert.assertTrue("Password field is not displayed on the new user form.", isPasswordTextboxDisplayed());
        Assert.assertTrue("Confirm password field is not displayed on new user form.", isConfirmPasswordTextboxDisplayed());
        Assert.assertTrue("Email address field is not displayed on new user form.", isEmailTextboxDisplayed());
        Assert.assertTrue("First name field is not displayed on the new user form.", isFirstNameTextboxDisplayed());
        Assert.assertTrue("Last name field is not displayed on the new user form.", isLastNameTextboxDisplayed());
        Assert.assertTrue("Organization field is not displayed on the new user form.", isOrganizationTextboxDisplayed());
    }

    /**
     * Performs assertions to check that the necessary fields are required
     */
    public void checkRequiredFields() {
        Assert.assertTrue("Username field is not required.", isUsernameRequired());
        Assert.assertTrue("Password field is not required.", isPasswordRequired());
        Assert.assertTrue("Confirm password field is not required.", isConfirmPasswordRequired());
        Assert.assertTrue("Email address field is not required.", isEmailRequired());
        Assert.assertTrue("First name field is not required.", isFirstNameRequired());
        Assert.assertTrue("Last name field is not required.", isLastNameRequired());
    }

    /**
     * Verifies 'required field' error displays for all fields except
     * Organization (checks Username, Password, Confirm Password, Email Address,
     * First Name, and Last Name)
     */
    public void checkRequiredFieldErrorAllFields() {
        Assert.assertTrue("Required field error is not displayed for the Username field.", isFieldRequiredErrorDisplayed(USERNAME_TEXTBOX_DISPLAYED_NAME));
        Assert.assertTrue("Required field error is not displayed for the Password field.", isFieldRequiredErrorDisplayed(PASSWORD_TEXTBOX_DISPLAYED_NAME));
        Assert.assertTrue("Required field error is not displayed for the Confirm Password field.", isFieldRequiredErrorDisplayed(CONFIRM_PASSWORD_TEXTBOX_DISPLAYED_NAME));
        Assert.assertTrue("Required field error is not displayed for the Email Address field.", isFieldRequiredErrorDisplayed(EMAIL_TEXTBOX_DISPLAYED_NAME));
        Assert.assertTrue("Required field error is not displayed for the First Name field.", isFieldRequiredErrorDisplayed(FIRST_NAME_TEXTBOX_DISPLAYED_NAME));
        Assert.assertTrue("Required field error is not displayed for the Last Name field.", isFieldRequiredErrorDisplayed(LAST_NAME_TEXTBOX_DISPLAYED_NAME));
    }

    /**
     * Verifies Username Field error displays for the Username field. Indicating
     * all of the acceptable values.
     */
    public void checkUsernameFieldError() {
        Assert.assertTrue("Error message indicating acceptable values is not displayed for the Username field.", isUsernameFieldErrorDisplayed());
    }

    /**
     * Checks Username in Use error text and verifies it displays as expected
     *
     * @param userName -the username that is already in use
     */
    public void checkUsernameInErrorMessageText(String userName) {
        String displayedText = getUsernameInUseErrorMessage().getText();
        Assert.assertEquals("Username in error text not displayed as expected.", USERNAME_IN_USE_ERROR_TEXT_PREFIX + userName + USERNAME_IN_USE_ERROR_TEXT_SUFFIX, displayedText);
    }

    /**
     * Checks that displayed values match values for given user (Checks
     * username, email, first name, last name and organization and verified
     * password/confirm password are the same size -- these are masked fields so
     * exact values cannot be checked)
     *
     * @param user -the user info expected
     */
    public void checkDisplayedSetFields(ETexasUser user) {
        String username = user.getUsername();
        String email = user.getEmailAddress();
        String firstName = user.getFirstName();
        String lastName = user.getLastName();
        String org = user.getOrganization();
        int passwordSize = user.getPassword().length();
        Assert.assertEquals("Username, " + username + " , not displayed as expected.", username, getDisplayedUsername());
        Assert.assertEquals("Email, " + email + " , not displayed as expected.", email, getDisplayedEmail());
        Assert.assertEquals("First name, " + firstName + " , not displayed as expected.", firstName, getDisplayedFirstName());
        Assert.assertEquals("Last name, " + lastName + " , not displayed as expected.", lastName, getDisplayedLastName());
        Assert.assertEquals("Organization, " + org + " , not displayed as expected.", org, getDisplayedOrganization());
        Assert.assertEquals("Password with length of " + passwordSize + " not displayed as expected.", passwordSize, getDisplayedPassword().length());
        Assert.assertEquals("Confirm Password with length of " + passwordSize + " not displayed as expected.", passwordSize, getDisplayedConfirmPassword().length());
    }

    /**
     * Checks that displayed values are cleared
     */
    public void checkClearedFields() {
        Assert.assertEquals("Username field not cleared as expected.", "", getDisplayedUsername());
        Assert.assertEquals("Email field not cleared as expected.", "", getDisplayedEmail());
        Assert.assertEquals("First name field not cleared as expected.", "", getDisplayedFirstName());
        Assert.assertEquals("Last name field not cleared as expected.", "", getDisplayedLastName());
        Assert.assertEquals("Organization field not cleared as expected.", "", getDisplayedOrganization());
        Assert.assertEquals("Password field not cleared as expected.", "", getDisplayedPassword());
        Assert.assertEquals("Confirm Password field not cleared as expected.", "", getDisplayedConfirmPassword());
    }

    //////////
    //Check required fields
    //////////

    /**
     * Checks to see if the username field is required
     *
     * @return true if the username field is required
     */
    public boolean isUsernameRequired() {
        return getUsernameTextbox().hasClass(REQUIRED_CSS_CLASS);
    }

    /**
     * Checks to see if the password textbox is required
     *
     * @return true if the confirm password textbox is required
     */
    public boolean isPasswordRequired() {
        return getPasswordTextbox().hasClass(REQUIRED_CSS_CLASS);
    }

    /**
     * Checks to see if the password confirmation textbox is required
     *
     * @return true if the confirm password textbox is required
     */
    public boolean isConfirmPasswordRequired() {
        return getConfirmPasswordTextbox().hasClass(REQUIRED_CSS_CLASS);
    }

    /**
     * Checks to see if the email textbox is required
     *
     * @return true if the email textbox is required
     */
    public boolean isEmailRequired() {
        return getEmailAddressTextbox().hasClass(REQUIRED_CSS_CLASS);
    }

    /**
     * Checks to see if the first name textbox is required
     *
     * @return true if the first name textbox is required
     */
    public boolean isFirstNameRequired() {
        return getEmailAddressTextbox().hasClass(REQUIRED_CSS_CLASS);
    }

    /**
     * Checks to see if the last name textbox is required
     *
     * @return true if the last name textbox is required
     */
    public boolean isLastNameRequired() {
        return getLastNameTextbox().hasClass(REQUIRED_CSS_CLASS);
    }

    /**
     * Checks to see if the Username Error message is displayed
     *
     * @param the error message displayed for Username error
     * @return returns true if the error message is displayed
     */
    public boolean isUsernameFieldErrorDisplayed() {
        return getUsernameError() != null; //TODO check to see if there is a better way to implement this.
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(NEW_USER_HEADER_XPATH));
    }

}
