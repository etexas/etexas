package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions;

import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;

/**
 * Page class representing the command queue options help window
 *
 * @author llaroussini
 */
public class CommandQueueOptionsHelpWindow extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver
     */
    public CommandQueueOptionsHelpWindow(WebDriver driver) {
        super(driver);
    }

    ///////////////////
    // ID's & Locators
    ///////////////////

    /**
     * Text displayed in the command queue help header
     */
    private static final String COMMAND_QUEUE_HELP_HEADER_TEXT = "Command Queue Help";

    /**
     * Expected help text
     */
    private static final String EXPECTED_HELP_TEXT = "The Command Queue allows the user to see any commands entered into the currently running execution.  There are buttons to inject new commands into the execution, delete commands from the queue and inject vehicles.";

    ///////////////////
    // Checkers
    ///////////////////

    /**
     * Checks to see if the command queue help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCommandQueueHelpHeaderDisplayed() {
        return isHeaderDisplayed(COMMAND_QUEUE_HELP_HEADER_TEXT);

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
        return isHelpOKBtnDisplayed(COMMAND_QUEUE_HELP_HEADER_TEXT);
    }

    //////////////
    // Utilities
    //////////////

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(COMMAND_QUEUE_HELP_HEADER_TEXT);
    }

}
