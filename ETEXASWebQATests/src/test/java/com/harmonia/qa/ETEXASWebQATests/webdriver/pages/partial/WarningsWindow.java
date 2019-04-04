package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the Warnings Window
 *
 * @author llaroussini
 */
public class WarningsWindow extends BaseForm {

    /**
     * Default constructor
     * 
     * @param driver -the web driver
     */
    public WarningsWindow(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////////////
    // ID's & Locators
    ///////////////////

    /**
     * Text displayed in the Warnings header
     */
    private static final String WARNINGS_HEADER_TEXT = "Warnings";

    /**
     * Xpath suffix to the warnings content area
     */
    private static final String WARNINGS_CONTENT_XPATH_SUFFIX = "./ancestor::div//div[contains(@class, 'x-window-text')]";

    ///////////////////
    // Getters
    ///////////////////

    /**
     * Gets the warnings content area
     *
     * @return the warnings content area element
     */
    private El getWarningsContent() {
        return getHeader(WARNINGS_HEADER_TEXT).el(By.xpath(WARNINGS_CONTENT_XPATH_SUFFIX));
    }

    ///////////////////
    // Checkers
    ///////////////////

    /**
     * Checks to see if the Wanrings header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isWarningsHeaderDisplayed() {
        return isHeaderDisplayed(WARNINGS_HEADER_TEXT);

    }

    /**
     * Checks to see if warnings content area is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isWarningsContentDisplayed() {
        return isElementDisplayed(getWarningsContent());
    }

    /**
     * Checks to see if the ok button is displayed in window
     *
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isWarningsOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(WARNINGS_HEADER_TEXT);
    }

    /////////////////
    // Utilities
    ////////////////

    /**
     * Verifies text is displayed in the warnings content area
     */
    public void checkContentText() {
        String messageContent = getWarningsContent().getText();
        Assert.assertNotNull("Not text could be found in the warnings content area.", messageContent);
    }

    /**
     * Clicks the OK button in the window
     */
    public void clickWarningsOKBtn() {
        clickHelpOKBtn(WARNINGS_HEADER_TEXT);
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(TITLE_XPATH_PREFIX + WARNINGS_HEADER_TEXT + "')]"));
    }

}
