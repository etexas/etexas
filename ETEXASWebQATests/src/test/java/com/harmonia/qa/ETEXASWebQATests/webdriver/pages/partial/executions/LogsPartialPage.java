package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal.Btn;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * The partial page displayed as a part of the Execution Details page when the
 * Logs tab is selected
 *
 * @author llaroussini
 */
public class LogsPartialPage extends ExecutionPartialPage {

    /**
     * Default constructor
     *
     * @param driver the web driver
     */
    public LogsPartialPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    /**
     * Search criteria options that are selectable
     *
     * @author llaroussini
     */
    public enum SearchSelectionOption {
        /**
         * The Devices search option
         */
        DEVICES("Devices"),
        /**
         * The Apps search option
         */
        APPS("Apps"),
        /**
         * The Keys search option
         */
        KEYS("Keys");

        /**
         * The label of the titles of the selectable search options as they
         * appear in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        SearchSelectionOption(String label) {
            this.label = label;
        }

        /**
         * Gets the label associated with the selectable search option as it is
         * displayed in the Web UI
         *
         * @return The label of the header cell
         */
        public String getLabel() {
            return this.label;
        }
    }

    /**
     * App Log header cell titles
     *
     * @author llaroussini
     */
    public enum AppLogHeaderCells {
        /**
         * The Device title for header cell
         */
        DEVICE("Device"),
        /**
         * The App title for header cell
         */
        APP("App"),
        /**
         * The Sim Time title for header cell
         */
        SIM_TIME("Sim Time"),
        /**
         * The Key title for header cell
         */
        KEY("Key"),
        /**
         * The Data title for header cell
         */
        DATA("Data");

        /**
         * The label of the titles of the header cells as they appear in the
         * application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        AppLogHeaderCells(String label) {
            this.label = label;
        }

        /**
         * Gets the label (title) associated with the header cell as it is
         * displayed in the Web UI
         *
         * @return The label of the header cell
         */
        public String getLabel() {
            return this.label;
        }
    }

    ///////////
    //Element ID's & Identifiers
    ///////////
    /**
     * Xpath to the Search Criteria area
     */
    private static final String SEARCH_CRITERIA_AREA_XPATH = "//div[text()='App Log Search Criteria']/ancestor::node()[5]";

    /**
     * Xpath to the App Log area
     */
    protected static final String APP_LOG_AREA_XPATH = "//div[contains(@id, 'appLogTable')]";

    /**
     * Xpath of icon displayed when App Logs table is loading
     */
    private static final String APP_LOGS_VIEWER_LOADING_ICON_XPATH = "//div[contains(@id, 'appLogsViewer')][contains(@class, 'x-tabpanel-child')]/div[contains(@id, 'loadmask')]";

    /**
     * Xpath prefix to select buttons (to be used in conjunction with search
     * criteria area xpath)
     */
    private static final String SELECT_BTN_XPATH_PREFIX = "//label[text()='";

    /**
     * Xpath suffix to select buttons
     */
    private static final String SELECT_BTN_XPATH_SUFFIX = "']/ancestor::div[1]//span[text()='Select']";

    /**
     * Xpath to the Min sim time text box
     */
    private static final String MIN_TEXT_BOX_XPATH = "//span[text()='Min:']//ancestor::div[contains(@id, 'numberfield')]//input";

    /**
     * Xpath to the Max sim time text box
     */
    private static final String MAX_TEXT_BOX_XPATH = "//span[text()='Max:']//ancestor::div[contains(@id, 'numberfield')]//input";

    /**
     * Xpath prefix to search button
     */
    private static final String SEARCH_BTN_XPATH = "//span[@data-ref='btnInnerEl'][text()='Search']/ancestor::a";

    /**
     * Xpath suffix to reset button
     */
    private static final String RESET_BTN_XPATH = "//span[@data-ref='btnInnerEl'][text()='Reset']/ancestor::a";

    /**
     * Xpath suffix to export to CSV file button
     */
    private static final String EXPORT_BTN_XPATH = "//span[@data-ref='btnInnerEl'][text()='Export to CSV File']";

    /**
     * Xpath prefix to app log column header cells
     */
    private static final String APP_LOG_COLUMN_HEADER_CELL_XPATH_PREFIX = "//div[contains(@id, 'headercontainer')]//span[text()='";

    /**
     * Xpath to App Logs Availability Warning window
     */
    private static final String APP_LOGS_AVAILABILITY_WARNING_WINDOW_XPATH = "//div[contains(@class, 'x-title-item')][@data-ref='textEl'][contains(text(),'App Logs Availability Warning')]//ancestor::div";

    /**
     * Xpath prefix to button in App Logs Availability Warning Window
     */
    private static final String APP_LOGS_AVAILABILITY_WINDOW_BTN_XPATH_PREFIX = APP_LOGS_AVAILABILITY_WARNING_WINDOW_XPATH + "//div[contains(@id, 'toolbar')]//span[contains(text(), '";

    /**
     * Xpath to loading icon in Search Criteria options section
     */
    private static final String SEARCH_CRITERIA_OPTIONS_LOADING_ICON_XPATH = "//div[contains(@id, 'appLogsViewer')]//div[contains(@id, 'panel')]//div[contains(@id, 'loadmask')]";

    /**
     * Xpath prefix to check if a Device, App, or Log columns are empty in Logs
     * tab
     */
    private static final String EMPTY_LOG_COLUMN_PREFIX = "//div[text()='None']/ancestor::div//label[text()='";

    /**
     * Xpath suffix to check if Device, App, or Log columns are empty in Logs
     * tab
     */
    private static final String EMPTY_LOG_COLUMN_SUFFIX = "']";

    /**
     * Xpath to the invalid sim time range error
     */
    private static final String INVALID_SIM_TIME_RANGE_XPATH = "//div[contains(@data-errorqtip, 'The max sim time cannot be less than the min sim time')]/ancestor::div//span[contains(text(),'Max:')]/ancestor::div[contains(@class, 'x-field')]//input";

    ///////////
    //Element Getters
    ///////////
    /**
     * Gets the search criteria area
     *
     * @return the area element
     */
    private El getSearchCriteriaArea() {
        return el(By.xpath(SEARCH_CRITERIA_AREA_XPATH));
    }

    /**
     * Gets the app log area
     *
     * @return the area element
     */
    protected El getAppLogSearchResultsArea() {
        return el(By.xpath(APP_LOG_AREA_XPATH));
    }

    /**
     * Gets the select button for the given selectable search option
     *
     * @param option -the search option to get the select button for
     * @return the button element
     */
    private El getSearchSelectionBtn(SearchSelectionOption option) {
        return el(By.xpath(SEARCH_CRITERIA_AREA_XPATH + SELECT_BTN_XPATH_PREFIX + option.label + SELECT_BTN_XPATH_SUFFIX));
    }

    /**
     * Gets the search button
     *
     * @return the button element
     */
    private El getSearchBtn() {
        return el(By.xpath(SEARCH_BTN_XPATH));
    }

    /**
     * Gets the Min sim time text box
     *
     * @return the text box element
     */
    private El getMinSimTimeTextBox() {
        return el(By.xpath(MIN_TEXT_BOX_XPATH));
    }

    /**
     * Gets the Max sim time text box
     *
     * @return the text box element
     */
    private El getMaxSimTimeTextBox() {
        return el(By.xpath(MAX_TEXT_BOX_XPATH));
    }

    /**
     * Gets the reset button
     *
     * @return the button element
     */
    private El getResetBtn() {
        return el(By.xpath(RESET_BTN_XPATH));
    }

    /**
     * Gets the Export to CSV button
     *
     * @return the export button element
     */
    private El getExportToCSVBtn() {
        return el(By.xpath(EXPORT_BTN_XPATH));
    }

    /**
     * Gets the given column header cell
     *
     * @param headerCell -the header cell to get
     * @return the cell element
     */
    private El getHeaderCell(AppLogHeaderCells headerCell) {
        return el(By.xpath(APP_LOG_COLUMN_HEADER_CELL_XPATH_PREFIX + headerCell.label + COLUMN_HEADER_CELL_XPATH_SUFFIX));
    }

    /**
     * Gets the App Logs Availability Warning window
     *
     * @return the window element
     */
    private El getAppLogsAvailabilityWarningWindow() {
        return el(By.xpath(APP_LOGS_AVAILABILITY_WARNING_WINDOW_XPATH));
    }

    /**
     * Gets button in App Logs Availability Warning Window
     *
     * @param button - the button to get
     * @return the button element
     */
    private El getAppLogsAvailablityWindowBtn(Btn button) {
        return el(By.xpath(APP_LOGS_AVAILABILITY_WINDOW_BTN_XPATH_PREFIX + button.getLabel() + "')]"));
    }

    /**
     * Gets xpath for log table with heading specified by input.
     *
     * @param heading - the String value for the heading to search, such as
     *        Devices, Apps, or Keys
     * @return the Element associated with the xpath and heading
     */
    public El getEmptyColumnByXpath(String heading) {
        return el(By.xpath(EMPTY_LOG_COLUMN_PREFIX + heading + EMPTY_LOG_COLUMN_SUFFIX));
    }

    /**
     * Gets the displayed Min sim time from Min sim time text box
     *
     * @return the displayed Min sim time
     */
    public String getDisplayedMinTime() {
        return getMinSimTimeTextBox().getAttribute("value");
    }

    /**
     * Gets the displayed Max sim time from Min sim time text box
     *
     * @return the displayed Max sim time
     */
    public String getDisplayedMaxTime() {
        return getMaxSimTimeTextBox().getAttribute("value");
    }

    /**
     * Gets the invalid sim time range error element displayed next to Max Sim
     * Time field
     *
     * @return the error element
     */
    public El getInvalidSimTimeRangeError() {
        return el(By.xpath(INVALID_SIM_TIME_RANGE_XPATH));
    }

    ///////////
    //Element Checkers
    ///////////
    /**
     * Checks if search criteria area is displayed
     *
     * @return true if displayed, false if not found or not visible
     */
    public boolean isSearchCriteriaAreaDisplayed() {
        return isElementDisplayed(getSearchCriteriaArea());
    }

    /**
     * Checks if select button is displayed for the given search option
     *
     * @param option -the search option where select button is expected
     * @return true if displayed, false otherwise
     */
    public boolean isSelectBtnDisplayed(SearchSelectionOption option) {
        return isElementDisplayed(getSearchSelectionBtn(option));
    }

    /**
     * Checks if Min sim time text box is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isMinSimTimeTextBoxDisplayed() {
        return isElementDisplayed(By.xpath(MIN_TEXT_BOX_XPATH));
    }

    /**
     * Checks if Max sim time text box is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isMaxSimTimeTextBoxDisplayed() {
        return isElementDisplayed(By.xpath(MAX_TEXT_BOX_XPATH));
    }

    /**
     * Checks if search button is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isSearchBtnDisplayed() {
        return isElementDisplayed(getSearchBtn());
    }

    /**
     * Checks if reset button is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isResetBtnDisplayed() {
        return isElementDisplayed(getResetBtn());
    }

    /**
     * Checks if export to CSV button is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isExportToCSVBtnDisplayed() {
        return getExportToCSVBtn() != null;
    }

    /**
     * Checks if given header cell is displayed
     *
     * @param headerCell -the column header cell expected
     * @return true if displayed, false if not found or not visible
     */
    public boolean isHeaderCellDisplayed(AppLogHeaderCells headerCell) {
        return isElementDisplayed(getHeaderCell(headerCell));
    }

    /**
     * Checks if App Logs Availability Window is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isAppLogsAvailabilityWarningDisplayed() {
        return isElementDisplayed(getAppLogsAvailabilityWarningWindow());
    }

    /**
     * Checks if the " is not a valid number" error appears for either the Min
     * or Max Sim Times based on the input value
     *
     * @param minMax - String value for which sim time to check, should be Min
     *        or Max
     * @return true if the error is displayed, otherwise false
     */
    public boolean isMaxMinInvalidNumberErrorDisplayed(String minMax) {
        return isInvalidNumberErrorDisplayed(minMax);
    }

    /**
     * Checks if the invalid range error is displayed with the Max Sim Time
     * field
     *
     * @return true if the error is displayed, otherwise false
     */
    public boolean isInvalidSimTimeRangeErrorDisplayed() {
        return isElementDisplayed(getInvalidSimTimeRangeError());
    }

    /**
     * Checks if column given by String is empty by xpath
     *
     * @param heading - String value that should be Devices, Apps, or Keys
     * @return true if 'None' is displayed in column, false if not found or not
     *         visible
     */
    public boolean isLogColumnEmpty(String heading) {
        return isElementDisplayed(getEmptyColumnByXpath(heading));
    }

    /////////////
    // Interaction
    /////////////
    /**
     * Clicks the Search button and waits for loading icon to display and
     * disappear
     */
    public LogSearchResultsPartialPage clickSearchBtn() {
        getSearchBtn().click();
        ETexasCommonUtils.sleep(500); //allows time for AppLogsAvailablityWarning to display
        if (isAppLogsAvailabilityWarningDisplayed() == true) {
            getAppLogsAvailablityWindowBtn(Btn.YES).click();
            waitForElementToBeInvisible(By.xpath(APP_LOGS_VIEWER_LOADING_ICON_XPATH), PAGE_LOAD_TIMEOUT);
        }
        else {
            waitForElementToBeInvisible(By.xpath(APP_LOGS_VIEWER_LOADING_ICON_XPATH), PAGE_LOAD_TIMEOUT);
        }
        return getPage(LogSearchResultsPartialPage.class);
    }

    /**
     * Sets the min sim time
     *
     * @param time -the time (as string) to set
     */
    public void setMinSimTime(String time) {
        getMinSimTimeTextBox().setText(time);
    }

    /**
     * Sets the max sim time
     *
     * @param time -the time (as string) to set
     */
    public void setMaxSimTime(String time) {
        getMaxSimTimeTextBox().setText(time);
    }

    /**
     * Clicks the Reset button
     */
    public void clickReset() {
        getResetBtn().click();
        waitForElementToBeInvisible(By.xpath(SEARCH_CRITERIA_OPTIONS_LOADING_ICON_XPATH));
    }

    /////////////
    // Utilities
    /////////////
    /**
     * Checks for presence of search criteria and app log areas
     */
    public void checkLogSections() {
        Assert.assertTrue("Search Criteria area could not be found.", isSearchCriteriaAreaDisplayed());
        Assert.assertTrue("App Logs area could not be found.", isAppLogSearchResultsAreaDisplayed());
    }

    /**
     * Checks for presence of Min and Max sim time text boxes
     */
    public void checkAllSearchOptions() {
        Assert.assertTrue("Min sim time text box not displayed.", isMinSimTimeTextBoxDisplayed());
        Assert.assertTrue("Max sim time text box not displayed.", isMaxSimTimeTextBoxDisplayed());
    }

    /**
     * Checks for presence of all search buttons (Search, Reset, and Export)
     */
    public void checkAllSearchBtns() {
        Assert.assertTrue("Search button could not be found.", isSearchBtnDisplayed());
        Assert.assertTrue("Reset button could not be found.", isResetBtnDisplayed());
        Assert.assertTrue("Export to CSV file button could not be found.", isExportToCSVBtnDisplayed());
    }

    /**
     * Checks for presence of all column header cells in App Logs area (Device,
     * App, Sim Time, Key, Data)
     */
    public void checkAllAppLogsColumnHeaderCells() {
        Assert.assertTrue("Device column header cell could not be found or is not visible.", isHeaderCellDisplayed(AppLogHeaderCells.DEVICE));
        Assert.assertTrue("App column header cell could not be found or is not visible.", isHeaderCellDisplayed(AppLogHeaderCells.APP));
        Assert.assertTrue("Sim Time column header cell could not be found or is not visible.", isHeaderCellDisplayed(AppLogHeaderCells.SIM_TIME));
        Assert.assertTrue("Key column header cell could not be found or is not visible.", isHeaderCellDisplayed(AppLogHeaderCells.KEY));
        Assert.assertTrue("Data column header cell could not be found or is not visible.", isHeaderCellDisplayed(AppLogHeaderCells.DATA));
    }

    /**
     * Checks that displayed values match expected values for min and max sim
     * times
     *
     * @param minTime -the min sim time expected
     * @param maxTime -the max sim time expected
     */
    public void checkDisplayedSimTims(String minTime, String maxTime) {
        Assert.assertEquals("Min sim time value not displayed as expected.", minTime, getDisplayedMinTime());
        Assert.assertEquals("Max sim time value not displayed as expected.", maxTime, getDisplayedMaxTime());
    }

    /**
     * Checks that displayed values are cleared/reset.
     */
    public void checkClearedFields() {
        Assert.assertEquals("Min sim time text box was not cleared as expected.", "", getDisplayedMinTime());
        Assert.assertEquals("Max sim time text box was not cleared as expected.", "", getDisplayedMaxTime());
    }

    /////////
    // Waits
    /////////
    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(APP_LOG_AREA_XPATH));
    }
}
