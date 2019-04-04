package com.harmonia.qa.ETEXASWebQATests.webdriver.pages;

import junit.framework.Assert;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.Execution;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.enums.ExecutionDetailsTabs;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ErrorsWindow;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.WarningsWindow;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions.CommandHistoryPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions.LaneGeometryPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions.LogsPartialPage;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the eTexas Executions Page displayed for a completed
 * execution
 *
 * @author llaroussini
 */
public class CompletedExecutionPage extends DashboardPage {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public CompletedExecutionPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //Element ID's & Identifiers
    ///////////

    /**
     * Xpath assigned to the page header
     */
    protected static final String EXECUTION_DETAILS_HEADER_XPATH = "//div[contains(@class, 'x-title-text')][text()='Execution Details']";

    /**
     * Xpath to the subheader displaying the simulation name
     */
    private static final String SIMULATION_NAME_SUBHEADER_XPATH = "//span[text()='Simulation:']/ancestor::div[contains(@class, 'x-hbox-form-item')]//div[@role='textbox']";

    /**
     * Xpath to the subheader displaying the execution name
     */
    private static final String EXECUTION_NAME_SUBHEADER_XPATH = "//span[text()='Execution:']/ancestor::div[contains(@class, 'x-hbox-form-item')]//div[@role='textbox']";

    /**
     * Xpath to the View Warnings button
     */
    private static final String VIEW_WARNINGS_BTN_XPATH = "//span[@data-ref='btnInnerEl'][text()='View Warnings']";

    /**
     * Xpath to the View Errors button
     */
    private static final String VIEW_ERRORS_BTN_XPATH = "//span[@data-ref='btnInnerEl'][text()='View Errors']";

    /**
     * Xpath prefix for tabs
     */
    private static final String TAB_XPATH_PREFIX = "//span[contains(@class, 'x-tab-inner-default')][text()='";

    /**
     * Xpath suffix for tabs
     */
    private static final String TAB_XPATH_SUFFIX = "']/ancestor::a";

    /**
     * Xpath to the alert displayed when tab is loading
     */
    protected static final String LOADING_TAB_ALERT_XPATH = "//div[contains(@class, 'x-title-text')][text()='Execution Details']//ancestor::div[5]/div[2]/div/div/div/div/div[3]/div[2]/div[2]/div[2]/div[2]//div[text()='Loading...']"; //TODO update xpath when unique identifiers are put in place

    /**
     * Xpath to the App Log area
     */
    private static final String APP_LOG_AREA_XPATH = "//div[contains(@id, 'appLogTable')]";

    ///////////
    //Element Getters
    ///////////

    /**
     * Gets execution details page header
     *
     * @return the header element
     */
    private El getHeader() {
        return el(By.xpath(EXECUTION_DETAILS_HEADER_XPATH));
    }

    /**
     * Gets the subheader section displaying the simulation name
     *
     * @return the simulation name subheader element
     */
    private El getSimulationNameSubheader() {
        return el(By.xpath(SIMULATION_NAME_SUBHEADER_XPATH));
    }

    /**
     * Gets the subheader section displaying the execution name
     *
     * @return the execution name subheader element
     */
    private El getExecutionNameSubheader() {
        return el(By.xpath(EXECUTION_NAME_SUBHEADER_XPATH));
    }

    /**
     * Gets the View Warnings button
     *
     * @return the button element
     */
    private El getViewWarningsBtn() {
        return el(By.xpath(VIEW_WARNINGS_BTN_XPATH));
    }

    /**
     * Gets the View Errors button
     *
     * @return the button element
     */
    private El getViewErrorsBtn() {
        return el(By.xpath(VIEW_ERRORS_BTN_XPATH));
    }

    /**
     * Gets the given details tab element
     *
     * @param tab -the details tab to get
     * @return the tab element
     */
    private El getDetailsTab(ExecutionDetailsTabs tab) {
        return el(By.xpath(TAB_XPATH_PREFIX + tab.getLabel() + TAB_XPATH_SUFFIX));
    }

    /**
     * Gets the Alert displayed when tab is loading
     *
     * @return the alert element
     */
    public El getTabLoadingAlert() {
        return el(By.xpath(LOADING_TAB_ALERT_XPATH));
    }

    /**
     * Gets the app log area
     *
     * @return the area element
     */
    private El getAppLogSearchResultsArea() {
        return el(By.xpath(APP_LOG_AREA_XPATH));
    }

    ///////////
    //Element Checkers
    ///////////

    /**
     * Checks to see if the execution details header is displayed
     *
     * @return true if header is displayed, false if it cannot be found or is
     *         not visible
     */
    public boolean isExecutionDetailsHeaderDisplayed() {
        return isElementDisplayed(getHeader());
    }

    /**
     * Checks if the given execution details tab is displayed
     *
     * @param tab -the tab expected
     * @return true if displayed, false if cannot be found or is not visible
     */
    public boolean isTabDisplayed(ExecutionDetailsTabs tab) {
        return isElementDisplayed(getDetailsTab(tab));
    }

    /**
     * Checks if the View Warnings button is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isViewWarningsBtnDisplayed() {
        return isElementDisplayed(getViewWarningsBtn());
    }

    /**
     * Checks if the View Errors button is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isViewErrorsBtnDisplayed() {
        return isElementDisplayed(getViewErrorsBtn());
    }

    /**
     * Checks if app search results area is displayed
     *
     * @return true if displayed, false if not found or not visible
     */
    public boolean isAppLogSearchResultsAreaDisplayed() {
        return isElementDisplayed(getAppLogSearchResultsArea());
    }

    /////////////////
    // Interactions
    ////////////////

    /**
     * Clicks the Lane Geometry tab and waits for alert loading message to be
     * invisible
     *
     * @return the Lane Geometry Partial Page
     */
    public LaneGeometryPartialPage clickLaneGeometryTab() {
        getDetailsTab(ExecutionDetailsTabs.LANE_GEOMETRY_TAB).click();
        waitUntilAlertIsNotPresent(By.xpath(LOADING_TAB_ALERT_XPATH), 10);
        return getPage(LaneGeometryPartialPage.class);
    }

    /**
     * Clicks the Logs tab and waits for alert loading message to be invisible
     *
     * @return the Logs Partial Page
     */
    public LogsPartialPage clickLogsTab() {
        getDetailsTab(ExecutionDetailsTabs.LOGS_TAB).click();
        waitUntilAlertIsNotPresent(By.xpath(LOADING_TAB_ALERT_XPATH), 10);
        return getPage(LogsPartialPage.class);
    }

    /**
     * Clicks the Command History tab and waits for alert loading message to be
     * invisible
     *
     * @return the Command History Partial Page
     */
    public CommandHistoryPartialPage clickCommandHistoryTab() {
        getDetailsTab(ExecutionDetailsTabs.COMMAND_HISTORY_TAB).click();
        waitUntilAlertIsNotPresent(By.xpath(LOADING_TAB_ALERT_XPATH), 10);
        return getPage(CommandHistoryPartialPage.class);
    }

    ////////////////
    // Utilities
    ////////////////

    /**
     * Checks for presence of all tabs (Lane Geometry, Logs, and Command
     * History)
     */
    public void checkAllCompletedExecTabsDisplayed() {
        Assert.assertTrue("Lane Geometry tab could not be found or is not visible.", isTabDisplayed(ExecutionDetailsTabs.LANE_GEOMETRY_TAB));
        Assert.assertTrue("Logs tab could not be found or is not visible.", isTabDisplayed(ExecutionDetailsTabs.LOGS_TAB));
        Assert.assertTrue("Command History tab could not be found or is not visible.", isTabDisplayed(ExecutionDetailsTabs.COMMAND_HISTORY_TAB));
    }

    /**
     * Checks that the name displayed in the Simulation name subheader matches
     * the given simulation name
     *
     * @param simName -the simulation name expected
     */
    public void checkSimulationNameSubheader(String simName) {
        String displayedName = getSimulationNameSubheader().getText();
        Assert.assertEquals("Simulation name not displayed as exepected in Simulation subheader section.", simName, displayedName);
    }

    /**
     * Checks that the name displayed in the Simulation name subheader matches
     * the name of the given simulation
     *
     * @param sim -the simulation expected
     */
    public void checkSimulationNameSubheader(TemplateSimulation sim) {
        String simName = sim.getName();
        checkSimulationNameSubheader(simName);
    }

    /**
     * Checks that the name displayed in the Execution name subheader matches
     * the given execution name
     *
     * @param executionName -the execution name expected
     */
    public void checkExecutionNameSubheader(String executionName) {
        String displayedName = getExecutionNameSubheader().getText();
        Assert.assertEquals("Execution name not displayed as exepected in Execution subheader section.", executionName, displayedName);
    }

    /**
     * Checks that the name displayed in the Execution name subheader matches
     * the name of the given simulation
     *
     * @param execution -the execution expected
     */
    public void checkExecutionNameSubheader(Execution execution) {
        String executionName = execution.getName();
        checkExecutionNameSubheader(executionName);
    }

    /**
     * Clicks the View Warnings button
     *
     * @return the Warnings Window
     */
    public WarningsWindow clickViewWarnings() {
        getViewWarningsBtn().click();
        return getPage(WarningsWindow.class);
    }

    /**
     * Clicks the View Errors button
     *
     * @return the Errors Window
     */
    public ErrorsWindow clickViewErrors() {
        getViewErrorsBtn().click();
        return getPage(ErrorsWindow.class);
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(APP_LOG_AREA_XPATH), 120);
    }
}
