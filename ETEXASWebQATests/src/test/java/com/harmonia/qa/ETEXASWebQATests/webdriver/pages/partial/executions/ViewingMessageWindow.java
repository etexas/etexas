package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the viewing message window
 *
 * @author llaroussini
 */
public class ViewingMessageWindow extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public ViewingMessageWindow(WebDriver driver) {
        super(driver);
    }

    ///////////////////
    // ID's & Locators
    ///////////////////

    /**
     * Text prefix displayed in the viewing message header
     */
    private static final String VIEWING_MESSAGE_HEADER_TEXT_PREFIX = "Viewing Message: ";

    /**
     * Text suffix displayed in the viewing message header for BSMV messages
     */
    private static final String VIEWING_BSMV_MESSAGE_HEADER_TEXT_SUFFIX = " (BSMV)";

    /**
     * Xpath suffix to the message content area
     */
    private static final String MESSAGE_CONTENT_XPATH_SUFFIX = "./ancestor::div//div[contains(@class, 'x-window-text')]";

    ///////////////////
    // Getters
    ///////////////////

    /**
     * Gets the message content area
     *
     * @param id -the message ID
     * @return the message content area element
     */
    private El getMessageContent(String id) {
        return getHeader(VIEWING_MESSAGE_HEADER_TEXT_PREFIX + id + VIEWING_BSMV_MESSAGE_HEADER_TEXT_SUFFIX).el(By.xpath(MESSAGE_CONTENT_XPATH_SUFFIX));
    }

    ///////////////////
    // Checkers
    ///////////////////

    /**
     * Checks to see if the viewing BSMV message help header is displayed
     *
     * @param id -the message ID
     * @return true if the header is displayed, false otherwise
     */
    public boolean isViewingBSMVMessageHeaderDisplayed(String id) {
        return isHeaderDisplayed(VIEWING_MESSAGE_HEADER_TEXT_PREFIX + id + VIEWING_BSMV_MESSAGE_HEADER_TEXT_SUFFIX);

    }

    /**
     * Checks to see if message content area is displayed
     *
     * @param id -the message ID
     * @return true if displayed, false otherwise
     */
    public boolean isViewingBSMVMessageContentDisplayed(String id) {
        return isElementDisplayed(getMessageContent(id));
    }

    /**
     * Checks to see if the ok button is displayed in window
     *
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isViewingMessageOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(VIEWING_BSMV_MESSAGE_HEADER_TEXT_SUFFIX);
    }

    /////////////////
    // Utilities
    ////////////////

    /**
     * Verifies text is displayed in the message content area
     *
     * @param id -the message ID
     */
    public void checkContentText(String id) {
        String messageContent = getMessageContent(id).getText();
        Assert.assertNotNull("Not text could be found in the message content area.", messageContent);
    }

    /**
     * Clicks the OK button in the window
     */
    public void clickViewingMessageOKBtn() {
        clickHelpOKBtn(VIEWING_BSMV_MESSAGE_HEADER_TEXT_SUFFIX);
    }

}
