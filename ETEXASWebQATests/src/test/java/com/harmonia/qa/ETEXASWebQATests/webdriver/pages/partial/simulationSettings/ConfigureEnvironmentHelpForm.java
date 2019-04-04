package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings;

import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;

/**
 * Page class representing the help window for the configure environment window
 *
 * @author llaroussini
 */
public class ConfigureEnvironmentHelpForm extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public ConfigureEnvironmentHelpForm(WebDriver driver) {
        super(driver);
    }

    ///////////////////
    // ID's & Locators
    ///////////////////

    /**
     * Text displayed in the configure environment help header
     */
    private static final String ENVIRONMENT_CONFIG_HELP_HEADER_TEXT = "Configure Environment Help";

    /**
     * Expected help text
     */
    private static final String EXPECTED_HELP_TEXT = "Configure the simulation environment by editing the placement of detectors (Detectors) and cell towers (Cell Towers). The simulation can be further configured by setting the center point (latitude/longitude) of the intersection, choosing the communications model, and selecting the coordinate conversion method (Options). The user can set the width, length, and distance from the stop line when adding new detectors to selected lanes, specify the cellular provider and location when placing new cell towers, and view the RSE coverage when configuring additional simulation options.";

    ///////////////////
    // Checkers
    ///////////////////

    /**
     * Checks to see if the environment configuration help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEnvironmentConfigHelpHeaderDisplayed() {
        return isHeaderDisplayed(ENVIRONMENT_CONFIG_HELP_HEADER_TEXT);

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
        return isHelpOKBtnDisplayed(ENVIRONMENT_CONFIG_HELP_HEADER_TEXT);
    }

    //////////////
    // Utilities
    //////////////

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(ENVIRONMENT_CONFIG_HELP_HEADER_TEXT);
    }

}
