package com.harmonia.qa.ETEXASWebQATests.ReportManagement;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.Execution;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasExecutionUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class which executes steps for the View Log Details tests, TC-046 and
 * ITC-033
 *
 * @author llaroussini
 * @author saistrop
 */
public class ViewingLogDetailsTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Simulation object used throughout the test case, with and without report
     * app
     */
    private TemplateSimulation testSim;

    /**
     * Simulation used in internal test case with no report apps configured
     */
    private TemplateSimulation testSimNoReportApp;

    /**
     * Execution object used throughout the test case, with and without report
     * app
     */
    private Execution execution;

    /**
     * Execution used in internal test case with no report apps configured
     */
    private Execution executionNoReport;

    /**
     * Report App associated with report device
     */
    private String appName = "ReportMOEApp";

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        ETexasTestBase.setJsonFileName(ETEXAS_JSON_DATA_FILE);
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get test user and test simulation
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        testSim = ETexasEntityManager.getTemplateSimulation(defaultEx05Sim); //get default EX_05 simulation
        testSim.setUser(testUser);
        testSim.setName(RandomStringGenerator.nextLetterString(5));
        ETexasEntityManager.addEntities(testUser, testSim);

        //Create test simulation entity without report app
        testSimNoReportApp = SimulationFactory.getTemplateSimulation(testUser, true);
        testSimNoReportApp.setUser(testUser);
        testSimNoReportApp.setName(RandomStringGenerator.nextLetterString(5));
        ETexasEntityManager.addEntities(testUser, testSimNoReportApp);

        //Register user and create new simulation from template
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        List<String> appNames = new ArrayList<String>(1);
        appNames.add(appName);
        ETexasSimulationUtils.createTemplateSimulationWithDefaultReportDevice(testSim, appNames);

        //Create Simulation without report app
        ETexasSimulationUtils.createTemplateSimulation(testSimNoReportApp);

        //Create an execution with an in-progress status and one with no report app
        String execName = ETexasExecutionUtils.createAndFinishExecution(testSim);
        String execNoReport = ETexasExecutionUtils.createAndFinishExecution(testSimNoReportApp);

        //Create execution entity
        List<Execution> execList = new ArrayList<Execution>(3);
        execution = new Execution();
        execution.setSimulation(testSim);
        execution.setName(execName);
        execList.add(execution);

        //Create execution entity without report app
        executionNoReport = new Execution();
        executionNoReport.setSimulation(testSimNoReportApp);
        executionNoReport.setName(execNoReport);
        execList.add(executionNoReport);

        //Set execution with associated simulation
        testSim.setExecutions(execList);
        ETexasEntityManager.addEntities(execution);

        //User is logged in
        landing.loginAs(testUser);
    }

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Test steps for TC-046
    //	 */
    //	@Test
    //	public void viewLogDetailsExternal() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");
    //
    //		//Ensure the Simulations page is loaded
    //		SimulationsPage simPage = getPage(SimulationsPage.class);
    //
    //		//Select execution, click view details, and verify Executions Details page displays
    //		simPage.selectSim(testSim, true);
    //		simPage.selectExecution(execution, true);
    //		CompletedExecutionPage execPage = simPage.clickViewDetails();
    //		Assert.assertTrue("Executions header could not be found.", execPage.isExecutionDetailsHeaderDisplayed());
    //
    //		//Verify Lane Geometry, Logs, and Command History tabs display
    //		execPage.checkAllCompletedExecTabsDisplayed();
    //
    //		//Click Logs and verify Search Criteria and App Logs table display
    //		LogsPartialPage logs = execPage.clickLogsTab();
    //		logs.checkLogSections();
    //
    //		//Verify expected buttons display in Search Criteria section
    //		logs.checkAllSearchOptions(); //Add min/max selectors/text boxes
    //		logs.checkAllSearchBtns();
    //
    //		//Click Devices Select option, verify devices associated with execution display, select desired device to be included in search //TODO select options not currently implemented, update once functional
    //		//Click Apps Select option, verify apps associated with execution display, select desired app to be included in search
    //		//Click Keys Select option, verify keys associated with execution display, select desired key to be included in search
    //		//Set a min and max time search filter
    //		String min = "50";
    //		String max = "60";
    //		logs.setMinSimTime(min);
    //		logs.setMaxSimTime(max);
    //
    //		//Disabled due to BUG 12913
    //		//Click Reset and verify filters clear
    //		//		logs.checkDisplayedSimTims(min, max);
    //		//		logs.clickReset();
    //		//		logs.checkDisplayedSimTims("", "");
    //
    //		//Set a new min and max time search filter //TODO incorporate other search filters once implemented
    //		logs.setMinSimTime(min);
    //		logs.setMaxSimTime(max);
    //
    //		//Click Search
    //		LogSearchResultsPartialPage logSearchResultsPage = logs.clickSearchBtn();
    //
    //		//Verify expected columns display in App Logs section
    //		logSearchResultsPage.checkAllAppLogsColumnHeaderCells();
    //
    //		//Verify search results display
    //		int totalPages = logSearchResultsPage.totalPages();
    //		int countOfLogs = logSearchResultsPage.getCountOfAppLogSearchRowsDisplayed();
    //		Assert.assertTrue("No results are displayed in App Log Results table following a search.", countOfLogs > 0);
    //
    //		//Verify pagination elements display (icons, page identifier text box, entries per page drop down)
    //		logSearchResultsPage.checkPaginationControlsDisplayed();
    //
    //		//Verify results display for only the specified min/max sim time values
    //		logSearchResultsPage.checkFirstRow(min);
    //		logSearchResultsPage.clickLastPage();
    //		logSearchResultsPage.checkLastRow(max);
    //		logSearchResultsPage.clickFirstPage();
    //
    //		//Check pagination
    //		if (totalPages == 1) {
    //			logSearchResultsPage.checkPagination();
    //		}
    //		else {
    //			logSearchResultsPage.clickFirstPage();
    //			logSearchResultsPage.checkPagination();
    //			logSearchResultsPage.checkPaginationFunctionality();
    //		}
    //
    //		//Log out
    //		logSearchResultsPage.logout(testUser);
    //	}
    //
    //	/**
    //	 * Test steps for ITC-033
    //	 */
    //	@Test
    //	public void viewLogDetailsInternal() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");
    //
    //		//Ensure the Simulations page is loaded
    //		SimulationsPage simPage = getPage(SimulationsPage.class);
    //
    //		//Select execution with no report apps, click view details
    //		simPage.selectSim(testSimNoReportApp, true);
    //		simPage.selectExecution(executionNoReport, true);
    //
    //		//Verifies that Device, App, and Key headings were empty when a simulation with no report app was used
    //		CompletedExecutionPage execPage = simPage.clickViewDetails();
    //		LogsPartialPage logPage = execPage.clickLogsTab();
    //		Assert.assertTrue("A device was present in the Devices list when viewing execution details when none were expected.", logPage.isLogColumnEmpty("Devices"));
    //		Assert.assertTrue("An app was present in the Devices list when viewing execution details when none were expected.", logPage.isLogColumnEmpty("Apps"));
    //		Assert.assertTrue("A key was present in the Devices list when viewing execution details when none were expected.", logPage.isLogColumnEmpty("Keys"));
    //		LogSearchResultsPartialPage searchLogPage = logPage.clickSearchBtn();
    //		Assert.assertTrue("Results were displayed in the search menu where none were expected.", searchLogPage.checkNoSearchData());
    //
    //		//Returns to Simulations and selects a different simulation that was configured with a report app
    //		simPage = searchLogPage.clickSimulations();
    //		simPage.selectSim(testSim, true);
    //		simPage.selectExecution(execution, true);
    //
    //		//Verifies that Devices, Apps, and Keys are present after searching
    //		execPage = simPage.clickViewDetails();
    //		logPage = execPage.clickLogsTab();
    //		Assert.assertFalse("A device was not present in the Devices list when viewing execution details when expected.", logPage.isLogColumnEmpty("Devices"));
    //		Assert.assertFalse("An app was not present in the Devices list when viewing execution details when expected.", logPage.isLogColumnEmpty("Apps"));
    //		Assert.assertFalse("A key was not present in the Devices list when viewing execution details when expected.", logPage.isLogColumnEmpty("Keys"));
    //		searchLogPage = logPage.clickSearchBtn();
    //		Assert.assertFalse("Results were not displayed in the search menu where expected.", searchLogPage.checkNoSearchData());
    //
    //		//Enter a non-numerical value in the Min Sim Times text box.
    //		searchLogPage.setMinSimTime("----");
    //		//Verify an error is displayed associated with the Min Sim Times text box indicating the values is not a valid number.
    //		Assert.assertTrue("Invalid Min Sim Time range error not displayed as expected when min sim time entered is non-numeric.", searchLogPage.isMaxMinInvalidNumberErrorDisplayed("Min:"));
    //		//Enter a non-numerical value in the Max Sim Times text box.
    //		searchLogPage.setMaxSimTime("----");
    //		//Verify an error is displayed associated with the Max Sim Times text box indicating the values is not a valid number.
    //		Assert.assertTrue("Invalid Max Sim Time range error not displayed as expected when max sim time entered is non-numeric.", searchLogPage.isMaxMinInvalidNumberErrorDisplayed("Max:"));
    //
    //		//Enter a valid value in the Min Sim Times text box.
    //		searchLogPage.setMinSimTime("25");
    //		//Enter a value that is less than the entered min sim time value in the Max Sim Times text box.
    //		searchLogPage.setMaxSimTime("0");
    //		//Verify an error is displayed associated with the Max Sim Times text box indicating the max sim time cannot be less than the min sim time.
    //		Assert.assertTrue("Invalid Sim Time range error not displayed as expected when max sim time entered is less than min sim time.", searchLogPage.isInvalidMinSimTimeErrorDisplayed());
    //
    //		//Enter a valid value that this outside the sim time range of the execution in the Min Sim Time text box.
    //		String randomMinSimTime = RandomStringGenerator.nextNumStringOfLength(5);
    //		searchLogPage.setMinSimTime(randomMinSimTime);
    //		//Enter a valid value that this outside the sim time range of the execution in the Max Sim Time text box.
    //		searchLogPage.setMaxSimTime(randomMinSimTime + RandomStringGenerator.nextNumStringOfLength(2));
    //		//Click the Search button.
    //		searchLogPage.clickSearchBtn();
    //		//Verify no search results are displayed.
    //		Assert.assertTrue("Results were displayed in the search menu after a sim time range outside the range of the execution was entered.", searchLogPage.checkNoSearchData());
    //
    //		//Clear the values from the Min Sim Times and Max Sim Times text boxes.
    //		searchLogPage.setMinSimTime("");
    //		searchLogPage.setMaxSimTime("");
    //
    //		//Log out
    //		searchLogPage.logout(testUser);
    //	}
}