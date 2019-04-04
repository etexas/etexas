package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;

import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;

/**
 * Page class representing the devices help window
 *
 * @author llaroussini
 */
public class DevicesHelpWindow extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public DevicesHelpWindow(WebDriver driver) {
        super(driver);
    }
    ///////////////////
    // ID's & Locators
    ///////////////////

    /**
     * Text displayed in the devices help header
     */
    private static final String DEVICES_HELP_HEADER_TEXT = "Devices Help";

    /**
     * Expected help text
     */
    private static final String EXPECTED_HELP_TEXT = "The Devices list allows the user to see the simulation devices, with the applications on the device, for an execution.  To view the parameter details of the application click on the green icon on the right side of the display for the device and application.";

    ///////////////////
    // Checkers
    ///////////////////

    /**
     * Checks to see if the devices help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isDevicesHelpHeaderDisplayed() {
        return isHeaderDisplayed(DEVICES_HELP_HEADER_TEXT);

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
        return isHelpOKBtnDisplayed(DEVICES_HELP_HEADER_TEXT);
    }

    //////////////
    // Utilities
    //////////////

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(DEVICES_HELP_HEADER_TEXT);
    }

}
