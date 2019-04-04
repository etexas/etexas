package com.harmonia.qa.ETEXASWebQATests.ReportManagement;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.Execution;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;

/**
 * Test class which executes steps for the Viewing Message App Data test, TC-059
 *
 * @author llaroussini
 */
public class ViewingMessageAppDataTest extends ETexasAfterTestResetTestBase {

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
     * MsgRxAppByDestination App associated with report device
     */
    private String msgRxDestAppName = "MsgRxAppByDestination";

    /**
     * MsgRxAppBySource App associated with report device
     */
    private String msgRxSourceAppName = "MsgRxAppBySource";

    /**
     * MsgTxAppByDestination App associated with report device
     */
    private String msgTxDestAppName = "MsgTxAppByDestination";

    /**
     * MsgTxAppBySource App associated with report device
     */
    private String msgTxSourceAppName = "MsgTxAppBySource";

    /**
     * Test setup.
     */
    //TODO - update for version 3.0
    //	@Before
    //	public void warmUp() {
    //		ETexasTestBase.setJsonFileName(ETEXAS_JSON_DATA_FILE);
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");
    //
    //		//Get test user and test simulation
    //		testUser = ETexasUserFactory.getUser(true); //Get a random user.
    //		testSim = ETexasEntityManager.getTemplateSimulation(defaultEx05Sim); //get default EX_05 simulation
    //		testSim.setUser(testUser);
    //		ETexasEntityManager.addEntities(testUser, testSim);
    //
    //		//Register user
    //		LandingPage landing = ETexasUserUtils.userRegistration(testUser);
    //
    //		//Setup for simulation -- apps and devices
    //		List<String> reportAppNames = new ArrayList<String>(2);
    //		reportAppNames.add(msgRxDestAppName);
    //		reportAppNames.add(msgRxSourceAppName);
    //		reportAppNames.add(msgTxDestAppName);
    //		reportAppNames.add(msgTxSourceAppName);
    //		List<String> obuAppNames = new ArrayList<String>(1);
    //		obuAppNames.add("BSMVerboseProducerApp");
    //		OBUDevice obu = OBUDeviceFactory.getOBUDevice(true);
    //		obu.setOBUPercent("100");
    //
    //		//Create simulation with report device and OBU
    //		ETexasSimulationUtils.createTemplateSimulationWithDefaultReportDeviceAndOBU(testSim, reportAppNames, obu, obuAppNames);
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
    //	public void viewMessageData() {
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
    //		//Select ReportDevice from Devices list and MsgRxDestinationApp from Apps list, click Search
    //		//logSearchResultsPage.selectOptionInSearchCriteriaList(SearchCriteriaListName.DEVICES, "ReportDevice"); TODO waiting on implementation of device names in list
    //		logSearchResultsPage.selectOptionInSearchCriteriaList(SearchCriteriaListName.APPS, msgRxDestAppName);
    //		logsPage.setMinSimTime("4");
    //		logsPage.setMaxSimTime("4");
    //		logSearchResultsPage.clickSearchBtn();
    //
    //		//Verify all entries have a value populated in the Device column.
    //		logSearchResultsPage.checkAllRowsDeviceColumnPopulated();
    //
    //		//Verify all entries display the MsgRxAppByDestination in App column
    //		logSearchResultsPage.checkAllRowsAppColumn(msgRxDestAppName);
    //
    //		//Verify all entries have a value populated in the Sim Time column.
    //		logSearchResultsPage.checkAllRowsSimTimeColumnPopulated();
    //
    //		//Verify the Key and Data column displays an expected value (known value based on random seed for execution)
    //		logSearchResultsPage.checkKeyAndDataValueInRowDisplayed("885646870310 (Source, Dest, Type, Size)", "916776591151, 885646870310, BasicSafetyMessageVerbose, 39");
    //
    //		//Click Reset
    //		logSearchResultsPage.clickReset();
    //
    //		//Verify search selections and search results are cleared
    //		Assert.assertFalse("Search criteria selection are not cleared as expected after clicking Reset.", logSearchResultsPage.areSearchCriteriaOptionsSelected());
    //		Assert.assertEquals("Results are still displayed in App Log Results table after clicking Reset.", logSearchResultsPage.getCountOfAppLogSearchRowsDisplayed(), 0);
    //
    //		//Select ReportDevice from Devices list and MsgRxSourceApp from Apps list, click Search
    //		//logSearchResultsPage.selectOptionInSearchCriteriaList(SearchCriteriaListName.DEVICES, "ReportDevice"); TODO waiting on implementation of device names in list
    //		logSearchResultsPage.selectOptionInSearchCriteriaList(SearchCriteriaListName.APPS, msgRxSourceAppName);
    //		logsPage.setMinSimTime("4");
    //		logsPage.setMaxSimTime("4");
    //		logSearchResultsPage.clickSearchBtn();
    //
    //		//Verify all entries have a value populated in the Device column.
    //		logSearchResultsPage.checkAllRowsDeviceColumnPopulated();
    //
    //		//Verify all entries display the MsgRxAppBySource in App column
    //		logSearchResultsPage.checkAllRowsAppColumn(msgRxSourceAppName);
    //
    //		//Verify all entries have a value populated in the Sim Time column.
    //		logSearchResultsPage.checkAllRowsSimTimeColumnPopulated();
    //
    //		//Verify the Key and Data column displays an expected value (known value based on random seed for execution)
    //		logSearchResultsPage.checkKeyAndDataValueInRowDisplayed("916776591151 (Source, Dest, Type, Size)", "916776591151, 885646870310, BasicSafetyMessageVerbose, 39");
    //
    //		//Click Reset
    //		logSearchResultsPage.clickReset();
    //
    //		//Verify search selections and search results are cleared
    //		Assert.assertFalse("Search criteria selection are not cleared as expected after clicking Reset.", logSearchResultsPage.areSearchCriteriaOptionsSelected());
    //		Assert.assertEquals("Results are still displayed in App Log Results table after clicking Reset.", logSearchResultsPage.getCountOfAppLogSearchRowsDisplayed(), 0);
    //
    //		//Select ReportDevice from Devices list and MsgTxDestinationApp from Apps list, click Search
    //		//logSearchResultsPage.selectOptionInSearchCriteriaList(SearchCriteriaListName.DEVICES, "ReportDevice"); TODO waiting on implementation of device names in list
    //		logSearchResultsPage.selectOptionInSearchCriteriaList(SearchCriteriaListName.APPS, msgTxDestAppName);
    //		logsPage.setMinSimTime("4");
    //		logsPage.setMaxSimTime("4");
    //		logSearchResultsPage.clickSearchBtn();
    //
    //		//Verify all entries have a value populated in the Device column.
    //		logSearchResultsPage.checkAllRowsDeviceColumnPopulated();
    //
    //		//Verify all entries display the MsgTxAppByDestination in App column
    //		logSearchResultsPage.checkAllRowsAppColumn(msgTxDestAppName);
    //
    //		//Verify all entries have a value populated in the Sim Time column.
    //		logSearchResultsPage.checkAllRowsSimTimeColumnPopulated();
    //
    //		//Verify the Key and Data column displays an expected value (known value based on random seed for execution)
    //		logSearchResultsPage.checkKeyAndDataValueInRowDisplayed("281474976710655 (Source, Dest, Type, Size)", "916776591151, 281474976710655, BasicSafetyMessageVerbose, 39");
    //
    //		//Click Reset
    //		logSearchResultsPage.clickReset();
    //
    //		//Verify search selections and search results are cleared
    //		Assert.assertFalse("Search criteria selection are not cleared as expected after clicking Reset.", logSearchResultsPage.areSearchCriteriaOptionsSelected());
    //		Assert.assertEquals("Results are still displayed in App Log Results table after clicking Reset.", logSearchResultsPage.getCountOfAppLogSearchRowsDisplayed(), 0);
    //
    //		//Select ReportDevice from Devices list and MsgTxSourceApp from Apps list, click Search
    //		//logSearchResultsPage.selectOptionInSearchCriteriaList(SearchCriteriaListName.DEVICES, "ReportDevice"); TODO waiting on implementation of device names in list
    //		logSearchResultsPage.selectOptionInSearchCriteriaList(SearchCriteriaListName.APPS, msgTxSourceAppName);
    //		logsPage.setMinSimTime("4");
    //		logsPage.setMaxSimTime("4");
    //		logSearchResultsPage.clickSearchBtn();
    //
    //		//Verify all entries have a value populated in the Device column.
    //		logSearchResultsPage.checkAllRowsDeviceColumnPopulated();
    //
    //		//Verify all entries display the MsgTxAppBySource in App column
    //		logSearchResultsPage.checkAllRowsAppColumn(msgTxSourceAppName);
    //
    //		//Verify all entries have a value populated in the Sim Time column.
    //		logSearchResultsPage.checkAllRowsSimTimeColumnPopulated();
    //
    //		//Verify the Key and Data column displays an expected value (known value based on random seed for execution)
    //		logSearchResultsPage.checkKeyAndDataValueInRowDisplayed("916776591151 (Source, Dest, Type, Size)", "916776591151, 281474976710655, BasicSafetyMessageVerbose, 39");
    //
    //		//Click Reset
    //		logSearchResultsPage.clickReset();
    //
    //		//Verify search selections and search results are cleared
    //		Assert.assertFalse("Search criteria selection are not cleared as expected after clicking Reset.", logSearchResultsPage.areSearchCriteriaOptionsSelected());
    //		Assert.assertEquals("Results are still displayed in App Log Results table after clicking Reset.", logSearchResultsPage.getCountOfAppLogSearchRowsDisplayed(), 0);
    //
    //		//Click Search
    //		logsPage.setMinSimTime("4");
    //		logsPage.setMaxSimTime("4");
    //		logSearchResultsPage.clickSearchBtn();
    //
    //		//Verify results for both VehicleInfoByIDApp and VehicleInfoByLaneApp are displayed
    //		Assert.assertTrue("MsgRxAppByDestination could not be found in results.", logSearchResultsPage.isAppDisplayedInResults(msgRxDestAppName));
    //		Assert.assertTrue("MsgRxAppBySource could not be found in results.", logSearchResultsPage.isAppDisplayedInResults(msgRxSourceAppName));
    //		Assert.assertTrue("MsgTxAppByDestination could not be found in results.", logSearchResultsPage.isAppDisplayedInResults(msgTxDestAppName));
    //		Assert.assertTrue("MsgTxAppBySource could not be found in results.", logSearchResultsPage.isAppDisplayedInResults(msgTxSourceAppName));
    //
    //		//Logout
    //		logSearchResultsPage.logout(testUser);
    //	}
}
