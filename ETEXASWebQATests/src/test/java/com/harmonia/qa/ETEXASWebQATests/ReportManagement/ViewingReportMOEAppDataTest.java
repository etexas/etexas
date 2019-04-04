package com.harmonia.qa.ETEXASWebQATests.ReportManagement;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.Execution;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;

/**
 * Test class which executes steps for the Viewing ReportMOEApp Data test,
 * TC-060
 *
 * @author llaroussini
 */
public class ViewingReportMOEAppDataTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Simulation object used throughout the test case
     */
    private TemplateSimulation testSim;

    /**
     * Execution object used throughout the test case
     */
    private Execution execution;

    /**
     * Report App associated with report device
     */
    private String appName = "ReportMOEApp";

    /**
     * Test setup.
     */
    //TODO - update for version 3.0
    //	@Before
    //	public void warmUp() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");
    //
    //		//Get test user and test simulation
    //		testUser = ETexasUserFactory.getUser(true); //Get a random user.
    //		testSim = ETexasEntityManager.getTemplateSimulation(defaultEx05Sim); //get default EX_05 simulation
    //		testSim.setUser(testUser);
    //		ETexasEntityManager.addEntities(testUser, testSim);
    //
    //		//Register user and create new simulation from template (with report device)
    //		LandingPage landing = ETexasUserUtils.userRegistration(testUser);
    //		List<String> appNames = new ArrayList<String>(1);
    //		appNames.add(appName);
    //		ETexasSimulationUtils.createTemplateSimulationWithDefaultReportDevice(testSim, appNames);
    //
    //		//Create an execution with a completed status
    //		String execName = ETexasExecutionUtils.createAndFinishExecutionSetRandomSeed(testSim, "1234");
    //
    //		//Create execution entity
    //		List<Execution> execList = new ArrayList<Execution>(3);
    //		execution = new Execution();
    //		execution.setSimulation(testSim);
    //		execution.setName(execName);
    //		execList.add(execution);
    //
    //		//Set execution with associated simulation
    //		testSim.setExecutions(execList);
    //		ETexasEntityManager.addEntities(execution);
    //
    //		//User is logged in
    //		landing.loginAs(testUser);
    //	}

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Test steps for TC-060
    //	 */
    //	@Test
    //	public void viewReportMOEAppData() {
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
    //		//Click Logs tab, then Search button
    //		LogsPartialPage logsPage = execPage.clickLogsTab();
    //		logsPage.setMinSimTime("74.5");
    //		logsPage.setMaxSimTime("74.5");
    //		ETexasCommonUtils.sleep(30000); ///ZZZ allows time for some logs to be written - takes up to 5 minutes for all logs to be compiled following execution completion
    //		LogSearchResultsPartialPage logSearchResultsPage = logsPage.clickSearchBtn();
    //
    //		//Verify search results display
    //		int countOfLogs = logSearchResultsPage.getCountOfAppLogSearchRowsDisplayed();
    //		Assert.assertTrue("No results are displayed in App Log Results table following a search.", countOfLogs > 0);
    //
    //		//Use the < and > icons to navigate through the pages of results.
    //		logSearchResultsPage.checkPaginationControlsDisplayed();
    //		if (logSearchResultsPage.totalPages() != 1) {
    //			logSearchResultsPage.clickNextPage();
    //			Assert.assertEquals("Current page does not display as expected when Next is clicked.", 2, logSearchResultsPage.currentPage());
    //			logSearchResultsPage.clickPreviousPage();
    //			Assert.assertEquals("Current page does not display as expected when Previous is clicked.", 1, logSearchResultsPage.currentPage());
    //		}
    //		logSearchResultsPage.selectEntriesPerPage(EntriesPerPage.ONE_HUNDRED);
    //		logSearchResultsPage.clickSearchBtn();
    //
    //		//Verify all entries have a value populated in the Device column.
    //		logSearchResultsPage.checkAllRowsDeviceColumnPopulated();
    //
    //		//Verify all entries have the text “SimApp” in the App column.
    //		logSearchResultsPage.checkAllRowsAppColumn(appName);
    //
    //		//Verify all entries have a value populated in the Sim Time column.
    //		logSearchResultsPage.checkAllRowsSimTimeColumnPopulated();
    //
    //		//Verify entries exist for Total Phase Fails Right Now in the Key column (known value based on random seed for execution)
    //		logSearchResultsPage.checkKeyValueInRowDisplayed("Phase Fails Right Now");
    //		//Verify the Data column displays an expected failure value (known value based on random seed for execution)
    //		logSearchResultsPage.checkKeyAndDataValueInRowDisplayed("Phase Fails Right Now", "0");
    //
    //		//Verify entries exist for Phase Fails This Step in the Key column (known value based on random seed for execution)
    //		logSearchResultsPage.checkKeyValueInRowDisplayed("Phase Fails This Step");
    //		//Verify the Data column displays an expected failure value (known value based on random seed for execution)
    //		logSearchResultsPage.checkKeyAndDataValueInRowDisplayed("Phase Fails This Step", "0");
    //
    //		//Verify entries exist for Queue Length Lane ID = 1 in the Key column (known value based on random seed for execution)
    //		logSearchResultsPage.checkKeyValueInRowDisplayed("Queue Length Lane Id = 10");
    //		//Verify the Data column displays an expected length value (known value based on random seed for execution)
    //		logSearchResultsPage.checkKeyAndDataValueInRowDisplayed("Queue Length Lane Id = 10", "1181.3557094876678");
    //
    //		//Set times to a time when queue back expansion is recorded
    //		logsPage.setMinSimTime("64");
    //		logsPage.setMaxSimTime("64");
    //		logSearchResultsPage = logsPage.clickSearchBtn();
    //
    //		//Verify entries exist for QueueBackExpansionMOE in the Key column (known value based on random seed for execution)
    //		logSearchResultsPage.checkKeyValueInRowDisplayed("QueueBackExpansionMOE");
    //		//Verify the Data column displays an expected value (known value based on random seed for execution)
    //		logSearchResultsPage.checkKeyAndDataValueInRowDisplayed("QueueBackExpansionMOE", "5, 0.0");
    //
    //		//Set times to a time when a vehicle travel time is recorded
    //		logsPage.setMinSimTime("66");
    //		logsPage.setMaxSimTime("66");
    //		logSearchResultsPage = logsPage.clickSearchBtn();
    //
    //		//Verify entries exist for Travel Time for Vehicle: 23, starting lane Lane:5:2:0 in the Key column (known value based on random seed for execution)
    //		logSearchResultsPage.checkKeyValueInRowDisplayed("Travel time for Vehicle:23, starting lane Lane:5:2:0");
    //		//Verify the Data column displays an expected time value (known value based on random seed for execution)
    //		logSearchResultsPage.checkKeyAndDataValueInRowDisplayed("Travel time for Vehicle:23, starting lane Lane:5:2:0", "60.07405324334427");
    //
    //		//Logout
    //		logSearchResultsPage.logout(testUser);
    //	}
}
