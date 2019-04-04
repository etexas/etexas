package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps;

import java.util.List;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.AppParameter;
import com.harmonia.qa.ETEXASWebQATests.entities.EmbeddedApp;
import com.harmonia.qa.ETEXASWebQATests.entities.UserJarApp;
import com.harmonia.qa.ETEXASWebQATests.entities.UserNativeApp;
import com.harmonia.qa.ETEXASWebQATests.entities.UserRemoteApp;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the Application Parameters modal
 *
 * @author llaroussini
 */
public class ApplicationParametersModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public ApplicationParametersModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    /**
     * App Parameter Column headers
     *
     * @author llaroussini
     */
    public enum AppParamColumnHeader {
        /**
         * Parameter Name column header
         */
        PARAM_NAME("Name"),
        /**
         * Default Value column header
         */
        DEFAULT_VALUE("Default Value");

        /**
         * The label of the column headers as displayed in the UI
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        AppParamColumnHeader(String label) {
            this.label = label;
        }

        /**
         * Gets the label of the column header as displayed in the UI
         *
         * @return The label of the column header
         */
        public String getLabel() {
            return this.label;
        }
    }

    ///////////
    //Element ID's & Identifiers
    ///////////

    /**
     * Text prefix to the Application Parameters header
     */
    private static final String APP_PARAMETERS_HEADER_TEXT = "Application Parameters";

    /**
     * Text for the Application Parameters Help header
     */
    private static final String APP_PARAMETERS_HELP_HEADER_TEXT = "Application Parameters Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "These are the modifiable parameters and their default values for the selected application. The value of these parameters can be changed when setting the applications for a particular device or device profile in the simulation or composite settings windows, respectively.";

    /**
     * Xpath of the Close button
     */
    private static final String CLOSE_BUTTON_XPATH = ".//div[contains(@id, 'application-parameter-profiles-window')]//span[contains(@id, 'close-button')][text()='Close']/ancestor::a";

    /**
     * Xpath for the Close Icon
     */
    protected static final String CLOSE_ICON_XPATH = "//div[contains(@data-qtip, 'Close dialog')]/div";

    /**
     * Xpath prefix to app parameter column header cell
     */
    private static final String COLUMN_HEADER_XPATH_PREFIX = "//div[contains(@id, 'ETexas-application-view-parameter-List')]//span[text()='";

    /**
     * Xpath prefix to app parameter Name column header cell
     */
    private static final String NAME_COLUMN_HEADER_XPATH_PREFIX = "//div[contains(@id, 'application-parameter-profile-grid')]//div[contains(@id, 'name-column')]//span[text()='";

    /**
     * Xpath prefix to app parameter Default Value column header cell
     */
    private static final String DEFAULT_VALUE_COLUMN_HEADER_XPATH_PREFIX = "//div[contains(@id, 'application-parameter-profile-grid')]//div[contains(@id, 'default-value-column')]//span[text()='";

    /**
     * Xpath to Application Parameter table
     */
    private static final String APP_PARAMETER_TABLE_XPATH = "//div[contains(@id, 'application-parameter-profile-grid')]//table";

    /**
     * Xpath to the area displayed when app has no parameters
     */
    private static final String NO_PARAMETERS_AREA_XPATH = "//div[@class='x-grid-empty']";

    /**
     * Text displayed in the No Parameters aresa
     */
    private static final String NO_PARAMETERS_TEXT = "There are no parameters defined for this application.";

    /**
     * Xpath prefix to row in Parameters table
     */
    private static final String PARAMETER_ROW_XPATH_PREFIX = "//div[contains(@id, 'application-parameter-profile-grid')]//table//div[contains(text(), '";

    ///////////////////
    // Getters
    ///////////////////

    /**
     * Gets from the Application Parameters header to the help icon
     *
     * @return the help icon
     */
    private El getAppParametersHelpIcon() {
        return getHelpIcon(APP_PARAMETERS_HEADER_TEXT);
    }

    /**
     * Gets given column header
     *
     * @param header - the app parameter column header to get
     * @return the column header cell element
     */
    private El getColumnHeader(AppParamColumnHeader header) {
        if (header.label.equalsIgnoreCase("Name")) {
            return el(By.xpath(NAME_COLUMN_HEADER_XPATH_PREFIX + header.label + "']"));
        }
        else {
            return el(By.xpath(DEFAULT_VALUE_COLUMN_HEADER_XPATH_PREFIX + header.label + "']"));
        }
    }

    /**
     * Gets the row in App Parameters table with the given parameter
     *
     * @param parameterName -the name of the parameter expected
     * @return the row with the given parameter
     */
    private El getAppParameterRow(String parameterName) {
        return el(By.xpath(PARAMETER_ROW_XPATH_PREFIX + parameterName + "')]/ancestor::tr"));
    }

    /**
     * Gets the Application Parameters table
     *
     * @return the table element
     */
    private El getAppParameterTable() {
        return el(By.xpath(APP_PARAMETER_TABLE_XPATH));
    }

    /**
     * Gets the area displayed when no parameters are defined
     *
     * @return the no parameters area element
     */
    private El getNoParametersArea() {
        return el(By.xpath(NO_PARAMETERS_AREA_XPATH));
    }

    /**
     * Gets the close button
     *
     * @return the close button
     */
    private El getCloseButton() {
        return el(By.xpath(CLOSE_BUTTON_XPATH));
    }

    ///////////////////
    // Checkers
    ///////////////////

    /**
     * Checks to see if the application parameters header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isAppParametersHeaderDisplayed() {
        return isHeaderDisplayed(APP_PARAMETERS_HEADER_TEXT);
    }

    /**
     * Checks to see if the application parameters help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isAppParametersHelpHeaderDisplayed() {
        return isHeaderDisplayed(APP_PARAMETERS_HELP_HEADER_TEXT);

    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isAppParametersHelpIconDisplayed() {
        return isHelpIconDisplayed(APP_PARAMETERS_HEADER_TEXT);
    }

    /**
     * Checks to see if the close icon is displayed
     *
     * @return true if the close icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isAppParametersCloseIconDisplayed() {
        return isCloseIconDisplayed(APP_PARAMETERS_HEADER_TEXT);
    }

    /**
     * Checks if the help content is displayed
     *
     * @return true if displayed, false if not
     */
    public boolean isAppParametersHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Check if application parameters help ok button is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(APP_PARAMETERS_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the given column header cell is displayed
     *
     * @param header -the app parameter column header expected
     * @return true if header cell displayed, false otherwise
     */
    public boolean isColumnHeaderCellDisplayed(AppParamColumnHeader header) {
        return isElementDisplayed(getColumnHeader(header));
    }

    /**
     * Checks to see if the expected app parameter is displayed in the App
     * Parameters table
     *
     * @param parameterName -the name of the parameter expected
     * @return true if displayed, false otherwise
     */
    public boolean isAppParameterDisplayed(String parameterName) {
        return isElementDisplayed(getAppParameterRow(parameterName));
    }

    /**
     * Checks to see if the expected app parameters is displayed in the App
     * Parameters table based on an AppParameter
     *
     * @param parameter - the Parameter being checked for
     * @return true if the element is displayed, otherwise false
     */
    public boolean isAppParameterDisplayed(AppParameter parameter) {
        return isAppParameterDisplayed(parameter.getParameterName());
    }

    /**
     * Checks if parameters table is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isAppParameterTableDisplayed() {
        return isElementDisplayed(getAppParameterTable());
    }

    /**
     * Checks if no parameters area is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isNoParametersAreaDisplayed() {
        return isElementDisplayed(getAppParameterTable());
    }

    /**
     * Checks to see if the close button is displayed in window
     *
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCloseBtnDisplayed() {
        return isElementDisplayed(getCloseButton());
    }

    /////////////////
    // Interaction
    /////////////////

    /**
     * Click the Application Parameters Help icon
     */
    public void clickAppParametersHelpIcon() {
        getAppParametersHelpIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(APP_PARAMETERS_HELP_HEADER_TEXT);
    }

    /**
     * Click the Close button and waits for window to close
     */
    public void clickCloseButton() {
        getCloseButton().click();
        waitForElementToBeInvisible(By.xpath(TITLE_XPATH_PREFIX + APP_PARAMETERS_HEADER_TEXT + "']"));
    }

    /**
     * Clicks the Close icon
     */
    public void clickCloseIcon() {
        clickCloseIcon(APP_PARAMETERS_HEADER_TEXT);
    }

    /////////////////
    // Utilities
    ////////////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkAppParametersHeaderIcons() {
        checkHeaderIcons(APP_PARAMETERS_HEADER_TEXT);
    }

    /**
     * Checks for presence of Application Parameters help header and content
     */
    public void checkHelpModal() {
        Assert.assertTrue("Application Parameters Help header is not displayed as expected.", isAppParametersHelpHeaderDisplayed());
        Assert.assertTrue("Application Parameters Help content is not displayed as expected.", isAppParametersHelpContentDisplayed());
    }

    /**
     * Checks for display of the column header cells (Parameter Name and Default
     * Value)
     */
    public void checkColumnHeaderCells() {
        Assert.assertTrue("Paramter Name column header cell not displayed as expected.", isColumnHeaderCellDisplayed(AppParamColumnHeader.PARAM_NAME));
        Assert.assertTrue("Default Value column header cell not displayed as expected.", isColumnHeaderCellDisplayed(AppParamColumnHeader.DEFAULT_VALUE));
    }

    /**
     * Check for presence of all app parameters in the app parameters table. If
     * an app has no parameters, checks that the no parameters section displays
     * as expected
     *
     * @param app -the user native app whose parameters are being viewed
     */
    public void checkAppParameters(UserNativeApp app) {
        List<AppParameter> params = app.getParameters();
        if (params == null) {
            Assert.assertTrue("No parameters area could not be found for app with no parameters.", isNoParametersAreaDisplayed());
            String displayedText = getNoParametersArea().getText();
            Assert.assertEquals("Text in No Parameters area not displayed as expected.", NO_PARAMETERS_TEXT, displayedText);
        }
        else {
            for (AppParameter param : params) {
                Assert.assertTrue("App parameter with name, " + param.getParameterName() + ", could not be found in App Parameters table.", isAppParameterDisplayed(param));
            }
        }
    }

    /**
     * Check for presence of all app parameters in the app parameters table. If
     * an app has no parameters, check that the no parameters section displays
     * as expected
     *
     * @param app -the user remote app whose parameters are being viewed
     */
    public void checkAppParameters(UserRemoteApp app) {
        List<AppParameter> params = app.getParameters();
        if (params == null) {
            Assert.assertTrue("No parameters area could not be found for app with no parameters.", isNoParametersAreaDisplayed());
            String displayedText = getNoParametersArea().getText();
            Assert.assertEquals("Text in No Parameters area not displayed as expected.", NO_PARAMETERS_TEXT, displayedText);
        }
        else {
            for (AppParameter param : params) {
                Assert.assertTrue("App parameter with name, " + param.getParameterName() + ", could not be found in App Parameters table.", isAppParameterDisplayed(param));
            }
        }
    }

    /**
     * Check for presence of all app parameters in the app parameters table. If
     * an app has no parameters, check that the no parameters section displays
     * as expected
     *
     * @param app -the built in app whose parameters are being viewed
     */
    public void checkAppParameters(EmbeddedApp app) {
        List<AppParameter> params = app.getParameters();
        if (params == null) {
            Assert.assertFalse("Parameters area was displayed found for app, " + app.getName() + ", which has no parameters.", isNoParametersAreaDisplayed());
        }
        else {
            for (AppParameter param : params) {
                Assert.assertTrue("App parameter with name, " + param.getParameterName() + ", could not be found in App Parameters table.", isAppParameterDisplayed(param));
            }
        }
    }

    /**
     * Check for presence of all app parameters in the app parameters table. If
     * an app has no parameters, check that the no parameters section displays
     * as expected
     *
     * @param app -the built in app whose parameters are being viewed
     */
    public void checkAppParameters(UserJarApp app) {
        List<AppParameter> params = app.getParameters();
        if (params == null) {
            Assert.assertTrue("No parameters area could not be found for app with no parameters.", isNoParametersAreaDisplayed());
            String displayedText = getNoParametersArea().getText();
            Assert.assertEquals("Text in No Parameters area not displayed as expected.", NO_PARAMETERS_TEXT, displayedText);
        }
        else {
            for (AppParameter param : params) {
                Assert.assertTrue("App parameter with name, " + param.getParameterName() + ", could not be found in App Parameters table.", isAppParameterDisplayed(param));
            }
        }
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(DEFAULT_VALUE_COLUMN_HEADER_XPATH_PREFIX + AppParamColumnHeader.DEFAULT_VALUE.label + "']"), 20);
    }
}
