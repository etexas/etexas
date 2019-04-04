package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the Errors Window
 *
 * @author llaroussini
 */
public class ErrorsWindow extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public ErrorsWindow(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////////////
    // ID's & Locators
    ///////////////////

    /**
     * Text displayed in the Errors header
     */
    private static final String ERRORS_HEADER_TEXT = "Errors";

    /**
     * Xpath suffix to the errors content area
     */
    private static final String ERRORS_CONTENT_XPATH_SUFFIX = "./ancestor::div//div[contains(@class, 'x-window-text')]";

    ///////////////////
    // Getters
    ///////////////////

    /**
     * Gets the errors content area
     *
     * @return the errors content area element
     */
    private El getErrorsContent() {
        return getHeader(ERRORS_HEADER_TEXT).el(By.xpath(ERRORS_CONTENT_XPATH_SUFFIX));
    }

    ///////////////////
    // Checkers
    ///////////////////

    /**
     * Checks to see if the Errors header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isErrorsHeaderDisplayed() {
        return isHeaderDisplayed(ERRORS_HEADER_TEXT);

    }

    /**
     * Checks to see if errors content area is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isErrorsContentDisplayed() {
        return isElementDisplayed(getErrorsContent());
    }

    /**
     * Checks to see if the ok button is displayed in window
     *
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isErrorsOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(ERRORS_HEADER_TEXT);
    }

    /////////////////
    // Utilities
    ////////////////

    /**
     * Verifies text is displayed in the errors content area
     */
    public void checkContentText() {
        String messageContent = getErrorsContent().getText();
        Assert.assertNotNull("Not text could be found in the errors content area.", messageContent);
    }

    /**
     * Clicks the OK button in the window
     */
    public void clickErrorsOKBtn() {
        clickHelpOKBtn(ERRORS_HEADER_TEXT);
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(TITLE_XPATH_PREFIX + ERRORS_HEADER_TEXT + "')]"));
    }

}
