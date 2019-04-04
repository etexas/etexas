package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial;

import org.openqa.selenium.WebDriver;

/**
 * Page class representing the messages help window
 *
 * @author llaroussini
 */
public class MessagesHelpWindow extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public MessagesHelpWindow(WebDriver driver) {
        super(driver);
    }

    ///////////////////
    // ID's & Locators
    ///////////////////

    /**
     * Text displayed in the messages help header
     */
    private static final String MESSAGES_HELP_HEADER_TEXT = "Messages Help";

    /**
     * Expected help text
     */
    private static final String EXPECTED_HELP_TEXT = "The Message viewer allows the user to see messages from applications during the current time step for an execution.  To view the details of the message click on the green icon on the right side of the display for the device and application.";

    ///////////////////
    // Checkers
    ///////////////////

    /**
     * Checks to see if the messages help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isMessagesHelpHeaderDisplayed() {
        return isHeaderDisplayed(MESSAGES_HELP_HEADER_TEXT);

    }

    /**
     * Checks to see if the help content is displayed
     *
     * @return true if the help content is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isHelpContentDisplayed() {
        return isContentDisplayed(EXPECTED_HELP_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(MESSAGES_HELP_HEADER_TEXT);
    }

    //////////////
    // Utilities
    //////////////

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(MESSAGES_HELP_HEADER_TEXT);
    }

}
