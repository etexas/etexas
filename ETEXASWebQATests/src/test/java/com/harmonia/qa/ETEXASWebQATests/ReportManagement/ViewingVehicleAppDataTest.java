package com.harmonia.qa.ETEXASWebQATests.ReportManagement;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.Execution;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;

/**
 * Test class which executes steps for the Viewing Vehicle App Data, TC-058
 *
 * @author llaroussini
 */
public class ViewingVehicleAppDataTest extends ETexasAfterTestResetTestBase {

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
     * VehicleID App associated with report device
     */
    private String vehicleIDAppName = "VehicleInfoByIdApp";

    /**
     * Vehicle Lane App associated with report device
     */
    private String vehicleLaneAppName = "VehicleInfoByLaneApp";

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
    //		List<String> appNames = new ArrayList<String>(2);
    //		appNames.add(vehicleIDAppName);
    //		appNames.add(vehicleLaneAppName);
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
    //	 * Test steps for TC-058
    //	 */
    //	@Test
    //	public void viewVehicleAppDataExternal() {
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
    //		logsPage.setMinSimTime("10");
    //		logsPage.setMaxSimTime("12");
    //		ETexasCommonUtils.sleep(30000); ///ZZZ allows time for some logs to be written - takes up to 5 minutes for all logs to be compiled following execution completion
    //		LogSearchResultsPartialPage logSearchResultsPage = logsPage.clickSearchBtn();
    //
    //		//Verify search results display
    //		int countOfLogs = logSearchResultsPage.getCountOfAppLogSearchRowsDisplayed();
    //		Assert.assertTrue("No results are displayed in App Log Results table following a search.", countOfLogs > 0);
    //
    //		//Use the < and > icons to navigate through the pages of results.
    //		logSearchResultsPage.checkPaginationControlsDisplayed();
    //		logSearchResultsPage.clickNextPage();
    //		Assert.assertEquals("Current page does not display as expected when Next is clicked.", 2, logSearchResultsPage.currentPage());
    //		logSearchResultsPage.clickPreviousPage();
    //		Assert.assertEquals("Current page does not display as expected when Previous is clicked.", 1, logSearchResultsPage.currentPage());
    //
    //		//Select ReportDevice from Devices list and Vehicle ID from keys list, click Search
    //		//logSearchResultsPage.selectOptionInSearchCriteriaList(SearchCriteriaListName.DEVICES, "ReportDevice"); TODO waiting on implementation of device names in list
    //		logSearchResultsPage.selectOptionInSearchCriteriaList(SearchCriteriaListName.APPS, vehicleIDAppName);
    //		logSearchResultsPage.clickSearchBtn();
    //
    //		//Verify all entries have a value populated in the Device column.
    //		logSearchResultsPage.checkAllRowsDeviceColumnPopulated();
    //
    //		//Verify all entries display the VehicleInfoByIdApp in App column
    //		logSearchResultsPage.checkAllRowsAppColumn(vehicleIDAppName);
    //
    //		//Verify all entries have a value populated in the Sim Time column.
    //		logSearchResultsPage.checkAllRowsSimTimeColumnPopulated();
    //
    //		//Verify the Key and Data column displays an expected value (known value based on random seed for execution)
    //		logSearchResultsPage.checkKeyAndDataValueInRowDisplayed("Vehicle: 1 (ID, X, Y, Z, Lane, Speed)", "Vehicle:1, -182.880000, 17365.675200, 0.000000, 1, 12.801600");
    //
    //		//Disabled due to BUG 12913
    //		//		//Click Reset
    //		//		logSearchResultsPage.clickReset();
    //		//
    //		//		//Verify search selections and search results are cleared
    //		//		Assert.assertEquals("Results are still displayed in App Log Results table after clicking Reset.", logSearchResultsPage.getCountOfAppLogSearchRowsDisplayed(), 0);
    //
    //		//Select ReportDevice from Devices list and VehicleInfoByLaneApp from apps list, click Search
    //		//logSearchResultsPage.selectOptionInSearchCriteriaList(SearchCriteriaListName.DEVICES, "ReportDevice"); TODO waiting on implementation of device names in list
    //		logSearchResultsPage.selectOptionInSearchCriteriaList(SearchCriteriaListName.APPS, vehicleLaneAppName);
    //		logsPage.setMinSimTime("3.5");
    //		logsPage.setMaxSimTime("3.5");
    //		logSearchResultsPage.clickSearchBtn();
    //
    //		//Verify all entries have a value populated in the Device column.
    //		logSearchResultsPage.checkAllRowsDeviceColumnPopulated();
    //
    //		//Verify all entries display the VehicleInfoByLaneApp in App column
    //		logSearchResultsPage.checkAllRowsAppColumn(vehicleLaneAppName);
    //
    //		//Verify all entries have a value populated in the Sim Time column.
    //		logSearchResultsPage.checkAllRowsSimTimeColumnPopulated();
    //
    //		//Verify the Key and Data column displays an expected value (known value based on random seed for execution)
    //		logSearchResultsPage.checkKeyAndDataValueInRowDisplayed("Lane: 1 (ID, X, Y, Z, Lane, Speed)", "Vehicle:1, -182.880000, 25686.715200, 0.000000, 1, 12.801600");
    //
    //		//Disabled due to BUG 12913
    //
    //		//Click Reset
    //		//		logSearchResultsPage.clickReset();
    //		//
    //		//		//Verify search selections and search results are cleared
    //		//		Assert.assertEquals("Results are still displayed in App Log Results table after clicking Reset.", logSearchResultsPage.getCountOfAppLogSearchRowsDisplayed(), 0);
    //
    //		//Click Search
    //		logsPage.setMinSimTime("3.5");
    //		logsPage.setMaxSimTime("3.5");
    //		logSearchResultsPage.clickSearchBtn();
    //
    //		//Verify results for both VehicleInfoByIDApp and VehicleInfoByLaneApp are displayed
    //		Assert.assertTrue("VehicleInfoByIDApp could not be found in results.", logSearchResultsPage.isAppDisplayedInResults(vehicleIDAppName));
    //		Assert.assertTrue("VehicleInfoByLaneApp could not be found in results.", logSearchResultsPage.isAppDisplayedInResults(vehicleLaneAppName));
    //
    //		//Logout
    //		logSearchResultsPage.logout(testUser);
    //	}
}
