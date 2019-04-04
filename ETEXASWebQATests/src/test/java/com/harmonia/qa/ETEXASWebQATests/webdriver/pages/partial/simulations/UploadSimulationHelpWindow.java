package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulations;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the upload simulation help window
 *
 * @author cbulloss
 */
public class UploadSimulationHelpWindow extends BaseForm {

    /**
     * Default page class constructor
     *
     * @param driver the driver being used
     */
    public UploadSimulationHelpWindow(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////////////
    // ID's & Locators
    ///////////////////

    /**
     * Help header text
     */
    public static final String UPLOAD_HELP_HEADER_TEXT = "Create Simulation from Upload Help";

    /**
     * By locator suitable for locating the Ok button
     */
    private static final By OK_BTN_LOCATOR = By.xpath(".//span[@data-ref='btnInnerEl'][contains(text(),'OK')]");

    /**
     * By locator suitable for locating the content area
     */
    private static final By CONTENT_AREA_LOCATOR = By.xpath(".//*[contains(@class,'x-component-default')][string-length(text()) > 0]");

    /**
     * Expected help text
     */
    public static final String EXPECTED_HELP_TEXT = "To upload a jar of applications enter the name of the file you wish to upload and then select the file fro your system.  This process could take a few seconds.";

    ///////////////////
    // Getters
    ///////////////////

    /**
     * Gets the OK Button
     *
     * @return the ok button element
     */
    private El getOkBtn() {
        return el(OK_BTN_LOCATOR);
    }

    /**
     * Gets the Content area from the window
     *
     * @return the content area where help text is displayed
     */
    private El getContentArea() {
        return el(CONTENT_AREA_LOCATOR);
    }

    ///////////////////
    // Checkers
    ///////////////////

    /**
     * Checks to see if the Ok button is displayed
     *
     * @return true if the Ok button is displayed, false if it is not or cannot
     *         be displayed
     */
    public boolean isOkBtnDisplayed() {
        return isElementDisplayed(OK_BTN_LOCATOR);
    }

    /**
     * Checks to see if the help header is displayed.
     *
     * @return true if the help header is displayed and has text, false
     *         otherwise
     */
    public boolean isHelpHeaderDisplayed() {
        return isHeaderDisplayed(UPLOAD_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the content area is displayed. Also ensures text length
     * is greater than zero based on the xpath locator
     *
     * @return true if the content area is displayed and has text, false if it
     *         is not, is missing text, or cannot be found
     */
    public boolean isContentAreaDisplayed() {
        return isElementDisplayed(CONTENT_AREA_LOCATOR);
    }

    ///////////////////
    // Interaction
    ///////////////////
    /**
     * Clicks the Ok button
     */
    public CreateSimulationFromUploadModal clickOk() {
        getOkBtn().click();
        return getPage(CreateSimulationFromUploadModal.class);
    }

    /**
     * Gets the text displayed in the content area
     *
     * @return the text displayed in the content area of the help window
     */
    public String getContentAreaText() {
        return getContentArea().getText();
    }

    ///////////////////
    // Utilities
    ///////////////////

}
