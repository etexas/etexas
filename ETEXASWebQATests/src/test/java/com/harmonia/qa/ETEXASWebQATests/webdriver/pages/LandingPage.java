package com.harmonia.qa.ETEXASWebQATests.webdriver.pages;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.NewUserForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the eTexas Landing Page
 *
 * @author cbulloss
 * @author rsmith
 */
public class LandingPage extends ETexasBasePage {

    /**
     * ID assigned to the header div
     */
    private static final String HEADER_ID = "header";

    /**
     * ID assigned to the Login Form div (contains entire login form including
     * new account link)
     */
    private static final String LOGIN_FORM_ID = "loginForm";

    /**
     * Xpath statement used to locate the username textbox in the login form
     */
    private static final String USERNAME_TEXTBOX_XPATH = ".//input[@name='username']";

    /**
     * Xpath statement used to locate the password textbox in the login form
     */
    private static final String PASSWORD_TEXTBOX_XPATH = ".//input[@name='password']";

    /**
     * By locator suitable for reliably locating the 'Need an account? Sign Up.'
     * link which allows new user registration.
     */
    private static final By NEW_USER_LINK_BY = By.xpath(".//a[text()='Need an account? Sign up.']");

    /**
     * Xpath assigned to the Login button
     */
    private static final String LOGIN_BTN_XPATH = "//span[contains(@id, 'Login')][contains(@class, 'x-btn-inner')]";

    /**
     * Username field name as displayed in UI
     */
    private static final String USERNAME_FIELD_DISPLAYED_NAME = "Username";

    /**
     * Password field name as displayed in UI
     */
    private static final String PASSWORD_FIELD_DISPLAYED_NAME = "Password";

    /**
     * Xpath to the Login Failed error message
     */
    private static final String LOGIN_FAILED_ERROR_MSG_XPATH = "//div[contains(@id, 'ETexas-Login-error-container')]/label[contains(text(), 'Login failed.')]";

    /**
     * Default constructor
     *
     * @param driver the web driver instance being used
     */
    public LandingPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //Element Getters
    ///////////

    /**
     * Gets the page header
     *
     * @return the landing page header
     */
    private El getHeader() {
        return el(By.id(HEADER_ID));
    }

    /**
     * Gets the login form from the page
     *
     * @return the login form
     */
    private El getLoginForm() {
        return el(By.id(LOGIN_FORM_ID));
    }

    /**
     * Gets the username field in the login form
     *
     * @return the username field for logging in
     */
    private El getUsernameField() {
        return el(By.xpath(USERNAME_TEXTBOX_XPATH));
    }

    /**
     * Gets the password field in the login form
     *
     * @return the password field for logging in
     */
    private El getPasswordField() {
        return el(By.xpath(PASSWORD_TEXTBOX_XPATH));
    }

    /**
     * Gets the new user link
     *
     * @return the link which allows a new user to register
     */
    private El getNewUserLink() {
        return el(NEW_USER_LINK_BY);
    }

    /**
     * Gets the login button
     *
     * @return the login button for submitting the login button
     */
    private El getLoginBtn() {
        return el(By.xpath(LOGIN_BTN_XPATH));
    }

    /**
     * Gets the login failed error message
     *
     * @return the error message element
     */
    private El getLoginFailedError() {
        return el(By.xpath(LOGIN_FAILED_ERROR_MSG_XPATH));
    }

    ///////////
    //Element Checkers
    ///////////

    /**
     * Checks to see if the header element is displayed.
     *
     * @return true if the element is found and is displayed, false if it is not
     *         OR cannot be found.
     */
    public boolean isHeaderDisplayed() {
        try {
            return getHeader().isDisplayed();
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks to see if the login form is displayed
     *
     * @return true if the login form is found and visible, false if it is not
     *         or cannot be found
     */
    public boolean isLoginFormDisplayed() {
        try {
            return getLoginForm().isDisplayed();
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks to see if the username field is displayed
     *
     * @return true if the username field is found and displayed, false if it
     *         cannot be found or is not visible.
     */
    public boolean isUsernameFieldDisplayed() {
        try {
            return getUsernameField().isDisplayed();
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks to see if the password field is displayed
     *
     * @return true if the password field is found and displayed, false if it is
     *         not found or not visible
     */
    public boolean isPasswordFieldDisplayed() {
        try {
            return getPasswordField().isDisplayed();
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks to see if the new user link is displayed
     *
     * @return true if the new user link is found and displayed, false it is not
     *         found or not visible
     */
    public boolean isNewUserLinkDisplayed() {
        try {
            return getNewUserLink().isDisplayed();
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks to see if the login button is displayed.
     *
     * @return true if the login button is found and displayed, false if it is
     *         not found or not visible
     */
    public boolean isLoginBtnDisplayed() {
        try {
            return getLoginBtn().isDisplayed();
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks if Login Failed error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isLoginFailedErrorDisplayed() {
        return isElementDisplayed(getLoginFailedError());
    }

    /**
     * Checks if Failure message is displayed //TODO - remove once updates are
     * made to REST API, this failure message is only displayed due to
     * discrepancies between front-end and REST API validation.
     *
     * @return true if displayed, false otherwise
     */
    public boolean isFailureMessageDisplayed() {
        return isElementDisplayed(By.xpath("//div[text()='Failure'][contains(@id, 'messagebox')]"));
    }

    ///////////
    //Element Interaction
    ///////////

    /**
     * Sets the text in the username field for logging in
     *
     * @param text the text to set in the username field
     */
    public void setUsernameText(String text) {
        getUsernameField().setText(text);
    }

    /**
     * Sets the text in the password field for logging in
     *
     * @param text the text to set in the password field
     */
    public void setPasswordText(String text) {
        getPasswordField().setText(text);
    }

    /**
     * Clicks the new user link
     *
     * @return the newly loaded New User Form
     */
    public NewUserForm clickSignUp() {
        getNewUserLink().click();
        return getPage(NewUserForm.class);
    }

    /**
     * Clicks the login button
     *
     * @return the newly loaded Simulations Page (displayed following login)
     */
    public SimulationsPage clickLogin() {
        getLoginBtn().click();
        return getPage(SimulationsPage.class);
    }

    /**
     * Clicks the login button (to be used when login fields are blank) -- no
     * return
     */
    public void clickLoginNoRtn() {
        getLoginBtn().click();
    }

    /**
     * Clicks OK in Failure pop-up //TODO - remove once updates are made to REST
     * API, this failure message is only displayed due to discrepancies between
     * front-end and REST API validation.
     */
    public void clickFailureOK() {
        el(By.xpath("//span[@data-ref='btnInnerEl'][text()='OK']")).click();
    }

    ///////////
    //Misc Utilities
    ///////////

    /**
     * Sets the text of both the username and password textboxes based on the
     * given user
     *
     * @param user -the user who's username/password will be entered
     */
    public void setUsernameAndPassword(ETexasUser user) {
        String username = user.getUsername();
        String password = user.getPassword();
        setUsernameText(username);
        setPasswordText(password);
    }

    /**
     * Sets the username and pasword text boxes with this user's information and
     * logs in to the application
     *
     * @param user the user which will be logged in
     * @return the newly loaded simulations page
     */
    public SimulationsPage loginAs(ETexasUser user) {
        setUsernameAndPassword(user);
        return clickLogin();
    }

    /**
     * Verifies Field Required error icon/tooltip displayed with Username field
     */
    public void checkUsernameFieldRequiredErrorDisplayed() {
        Assert.assertTrue("Field required error is not displayed as expected with Username field.", isFieldRequiredErrorDisplayed(USERNAME_FIELD_DISPLAYED_NAME));
    }

    /**
     * Verifies Field Required error icon/tooltip displayed with Password field
     */
    public void checkPasswordFieldRequiredErrorDisplayed() {
        Assert.assertTrue("Field required error is not displayed as expected with Password field.", isFieldRequiredErrorDisplayed(PASSWORD_FIELD_DISPLAYED_NAME));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(USERNAME_TEXTBOX_XPATH));
    }
}
