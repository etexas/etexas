package com.harmonia.qa.ETEXASWebQATests.webdriver.pages;

import junit.framework.Assert;

import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.ui.ExpectedConditions;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the dashboard page where the user is redirected after
 * login - this page class defines the top portion of the static page displayed
 * while the user is logged in
 *
 * @author cbulloss
 */
public class DashboardPage extends ETexasBasePage {

    ///////////
    //Element ID's & Identifiers
    ///////////

    /**
     * Xpath assigned to the dashboard page header
     */
    private static final String HEADER_XPATH = "//div[contains(@class, 'etexas-title')]";

    /**
     * The header text displayed on the dashboard page
     */
    private static final String DASHBOARD_HEADER_TEXT = "eTEXAS";

    /**
     * Xpath prefix to button elements in UI
     */
    private static final String BUTTONS_XPATH_PREFIX = "//span[@data-ref='btnInnerEl'][text()='";

    /**
     * Xpath suffix to button elements in UI
     */
    private static final String BUTTONS_XPATH_SUFFIX = "']";

    /**
     * Text displayed in the simulations link
     */
    private static final String SIMULATIONS_LINK_TEXT = "Simulations";

    /**
     * Text displayed in the executions link
     */
    private static final String EXECUTIONS_LINK_TEXT = "Executions";

    /**
     * Text displayed in the apps link
     */
    private static final String APPS_LINK_TEXT = "Applications";

    /**
     * Xpath assigned to the logout link
     */
    private static final String LOG_OUT_LINK_XPATH = "//span[@data-ref='textEl'][text()='Log Out']/ancestor::a";

    /**
     * Default constructor
     *
     * @param driver the web driver instance being used
     */
    public DashboardPage(WebDriver driver) {
        super(driver);
    }

    ///////////
    //Element Getters
    ///////////

    /**
     * Gets the header for this page
     *
     * @return the header
     */
    private El getHeader() {
        return el(By.xpath(HEADER_XPATH));
    }

    /**
     * Gets button with given text
     *
     * @param btnName -the text associated with the expected button
     * @return the button element
     */
    private El getBtn(String btnName) {
        return el(By.xpath(BUTTONS_XPATH_PREFIX + btnName + BUTTONS_XPATH_SUFFIX));
    }

    /**
     * Gets the simulations link
     *
     * @return the simulations link
     */
    private El getSimulationsLink() {
        return el(getBtn(SIMULATIONS_LINK_TEXT));
    }

    /**
     * Gets the executions link
     *
     * @return the executions link
     */
    private El getExecutionsLink() {
        return el(getBtn(EXECUTIONS_LINK_TEXT));
    }

    /**
     * Gets the apps link
     *
     * @return the apps link
     */
    private El getAppsLink() {
        return el(getBtn(APPS_LINK_TEXT));
    }

    /**
     * Gets the username link
     *
     * @param user - the logged in user
     * @return the username link
     */
    private El getUsernameLink(ETexasUser user) {
        String username = user.getUsername();
        return el(getBtn(username));
    }

    /**
     * Gets the logout link
     *
     * @return the logout link
     */
    private El getLogoutLink() {
        return el(By.xpath(LOG_OUT_LINK_XPATH));
    }

    ///////////
    //Element Checkers
    ///////////

    /**
     * Checks to see if the header is displayed
     *
     * @return true if the header is displayed, false if it cannot be found or
     *         is not visible
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
     * Checks to see if the simulations link is displayed
     *
     * @return true if the simulations link is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isSimulationsLinkDisplayed() {
        try {
            return getSimulationsLink().isDisplayed();
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks to see if the executions link is displayed
     *
     * @return true if the executions link is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isExecutionsLinkDisplayed() {
        try {
            return getExecutionsLink().isDisplayed();
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks to see if the apps link is displayed
     *
     * @return true if the apps link is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isAppsLinkDisplayed() {
        try {
            return getAppsLink().isDisplayed();
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks to see if the username link is displayed
     *
     * @param user - the logged in user
     * @return true if the username link is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isUsernameDisplayed(ETexasUser user) {
        try {
            return getUsernameLink(user).isDisplayed();
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks to see if the logout link is displayed
     *
     * @return true if the logout link is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isLogoutDisplayed() {
        try {
            return getLogoutLink().isDisplayed();
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    ///////////
    //Interactions
    ///////////

    /**
     * Click the executions link
     *
     * @return the newly loaded Executions page
     */
    public ExecutionsPage clickExecutions() {
        getExecutionsLink().click();
        return getPage(ExecutionsPage.class);
    }

    /**
     * Click the apps link
     *
     * @return the newly loaded Apps page
     */
    public AppsPage clickApps() {
        getAppsLink().click();
        return getPage(AppsPage.class);
    }

    /**
     * Clicks the username link
     *
     * @param user - the logged in user
     */
    public void clickUsername(ETexasUser user) {
        getUsernameLink(user).click();
    }

    /**
     * Gets the text displayed by the username link
     *
     * @param user - the logged in user
     * @return the displayed username
     */
    public String getUsernameText(ETexasUser user) {
        return getUsernameLink(user).getText();
    }

    /**
     * Clicks the logout link
     *
     * @return the newly loaded landing page
     */
    public LandingPage clickLogout() {
        getLogoutLink().click();
        return getPage(LandingPage.class);
    }

    /**
     * Logs the user out, including clicking the username to display the menu.
     *
     * @param user - the logged in user
     * @return the newly loaded LandingPage object.
     */
    public LandingPage logout(ETexasUser user) {
        Assert.assertTrue("Username link could not be found.", isUsernameDisplayed(user));
        clickUsername(user);
        waitForElementToBeVisible((By.xpath(LOG_OUT_LINK_XPATH)), 5);
        return clickLogout();
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Check if header displays the text 'eTEXAS'
     */
    public void checkHeaderText() {
        String displayedText = getHeader().getText();
        Assert.assertEquals("Header is not displaying the expected text, the text '" + displayedText + "' is displayed.", DASHBOARD_HEADER_TEXT, displayedText);
    }

    /**
     * Checks to see if the correct username is displayed on the page (includes
     * assertion)
     *
     * @param user the logged in user
     */
    public void checkUsername(ETexasUser user) {
        String username = user.getUsername();
        Assert.assertEquals("Missing or incorrect username displaed.", username, getUsernameText(user));
    }

    /**
     * Checks to see if the Applications, Executions, and Simulations links are
     * displayed.
     */
    public void checkLinks() {
        Assert.assertTrue("Applications link was not found where expected.", isAppsLinkDisplayed());
        //Assert.assertTrue("Executions link was not found where expected.", isExecutionsLinkDisplayed()); -- TODO update when executions re-implemented in version 3.0
        Assert.assertTrue("Simulations link was not found where expected.", isSimulationsLinkDisplayed());
    }

    /**
     * Checks the dashboard displays all expected elements for the given
     * logged-in user (header text, simulation, apps, and execution links, and
     * username link)
     *
     * @param user -the user currently logged in
     */
    public void checkDashboard(ETexasUser user) {
        checkHeaderText();
        checkLinks();
        checkUsername(user);
    }

    /**
     * Waits for an element to be enabled using the By locator.
     *
     * @param by the By locator for finding the element
     */
    public void waitForElementToBeEnabled(final By by) {
        long before = System.currentTimeMillis();
        newWait(5).until(ExpectedConditions.elementToBeClickable(by));
        long after = System.currentTimeMillis();
        logSleepWait(after - before);
    }
}
