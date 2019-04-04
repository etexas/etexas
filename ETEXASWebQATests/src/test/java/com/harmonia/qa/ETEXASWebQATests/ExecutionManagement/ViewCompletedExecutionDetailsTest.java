package com.harmonia.qa.ETEXASWebQATests.ExecutionManagement;

import com.harmonia.qa.ETEXASWebQATests.entities.Execution;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.VehicleInjectionCommand;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;

/**
 * Test class which executes steps for the View Completed Execution Details
 * test, TC-019
 *
 * @author llaroussini
 */
public class ViewCompletedExecutionDetailsTest extends ETexasAfterTestResetTestBase {

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
     * Command used throughout the test case
     */
    private VehicleInjectionCommand command;

    /**
     * Report App associated with report device
     */
    private String appName = "ReportMOEApp";

    /**
     * Random seed for execution used in test case
     */
    private String rndmSeed = "1234";
}

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
//		//Register user and create new simulation from template
//		ETexasUserUtils.userRegistration(testUser);
//		List<String> appNames = new ArrayList<String>(1);
//		appNames.add(appName);
//		ETexasSimulationUtils.createTemplateSimulationWithDefaultReportDevice(testSim, appNames);
//
//		//Create an execution with an in-progress status
//		String execName = ETexasExecutionUtils.createInProgressExecutionWithKnownRandomSeed(testSim, rndmSeed);
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
//		//Create command entity
//		List<VehicleInjectionCommand> commandList = new ArrayList<VehicleInjectionCommand>(1);
//		command = new VehicleInjectionCommand();
//		command.setCommandType(CommandType.INJECT_VEHICLE);
//		command.setLane("1");
//		command.setSpeed("10");
//		command.setVehicleType("CAR");
//		command.setExecution(execution);
//		execution.setVehicleInjectionCommands(commandList);
//		commandList.add(command);
//
//		//Add command to InProgress execution and finished execution
//		String simTime = ETexasExecutionUtils.injectVehicleCommandIntoInProgressExecutionAndFinish(testSim, execName, command);
//		command.setSimTime(simTime);
//
//		//User is logged in
//		//landing.loginAs(testUser); //BUG 12228 - update to log out user when resolved
//	}
//
//	/**
//	 * Test steps for TC-019
//	 */
//	@Test
//	public void viewCompletedExecDetails() {
//		//Set screenshot
//		screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");
//
//		//Ensure the Simulations page is loaded
//		SimulationsPage simPage = getPage(SimulationsPage.class);
//
//		//Select execution, click view details, and verify Executions Details page displays
//		simPage.selectSim(testSim, true);
//		simPage.selectSim(testSim, false); //Remove de-selection and re-selection steps following resolution of BUG 12583
//		simPage.selectSim(testSim, true);
//		simPage.selectExecution(execution, true);
//		CompletedExecutionPage execPage = simPage.clickViewDetails();
//		Assert.assertTrue("Executions header could not be found.", execPage.isExecutionDetailsHeaderDisplayed());
//
//		//Verify selected simulation name and execution name display
//		execPage.checkSimulationNameSubheader(testSim);
//		execPage.checkExecutionNameSubheader(execution);
//
//		//Verify Lane Geometry, Logs, and Command History tabs display
//		execPage.checkAllCompletedExecTabsDisplayed();
//
//		//Verify View Warnings button displays
//		Assert.assertTrue("View Warnings button not displayed as expected.", execPage.isViewWarningsBtnDisplayed());
//
//		//Click View Warnings and verify View Warnings Window displays
//		WarningsWindow warnings = execPage.clickViewWarnings();
//		Assert.assertTrue("Warnings window header not displayed as expected.", warnings.isWarningsHeaderDisplayed());
//
//		//Verify warning content displays
//		Assert.assertTrue("Warnings window content area not displayed as expected.", warnings.isWarningsContentDisplayed());
//		warnings.checkContentText();
//
//		//Verify OK button displays, click OK, and verify window closes
//		Assert.assertTrue("OK button not displayed in Warnings window as expected.", warnings.isWarningsOKBtnDisplayed());
//		warnings.clickWarningsOKBtn();
//		Assert.assertFalse("Warnings header is still displayed after clicking OK.", warnings.isWarningsHeaderDisplayed());
//
//		//Check if View Errors button is displayed, if it is verify displays as expected
//		if (execPage.isViewErrorsBtnDisplayed() == true) {
//
//			//Click View Errors and verify View Errors window displays
//			ErrorsWindow errors = execPage.clickViewErrors();
//			Assert.assertTrue("Errors window header not displayed as expected.", errors.isErrorsHeaderDisplayed());
//
//			//Verify error content displays
//			Assert.assertTrue("Errors window content area not displayed as expected.", errors.isErrorsContentDisplayed());
//			errors.checkContentText();
//
//			//Verify OK button displays, click OK, and verify window closes
//			Assert.assertTrue("OK button not displayed in Errors window as expected.", errors.isErrorsOKBtnDisplayed());
//			errors.clickErrorsOKBtn();
//			Assert.assertFalse("Errors header is still displayed after clicking OK.", errors.isErrorsHeaderDisplayed());
//		}
//		execPage.waitUntilLoaded();
//
//		//Click Lane Geometry and verify all lanes display with associated data (Lane ID, Approach, Type, Movements, Speed Limit, Nodes, and (i) icon)
//		LaneGeometryPartialPage lanes = execPage.clickLaneGeometryTab();
//		lanes.checkAllColumnHeaderCells();
//		lanes.checkAllLanes(testSim);
//
//		//Click (i) icon for a lane, verify Viewing Nodes window displays
//		Lane laneOne = testSim.getLanes().get(0);//lane with signal expected (and associated data) is known by static execution created in warm-up
//		String laneID = laneOne.getLaneID().getLabel();
//		Assert.assertTrue("Info icon could not be found for lane with ID of " + laneID + ".", lanes.isLaneInfoIconDisplayed(laneOne));
//		lanes.clickInfoIcon(laneOne);
//		Assert.assertTrue("Viewing nodes window could not be found after clicking Info icon for lane with ID of " + laneID + ".", lanes.isViewingNodesWindowDisplayed(laneID));
//
//		//Verify node values display in window
//		lanes.checkViewingNodesContent(laneOne);
//
//		//Verify OK button displays, click OK, and verify window closes
//		Assert.assertTrue("OK button could not be found in Viewing Nodes window for lane with ID of " + laneID + ".", lanes.isLaneOKBtnDisplayed(laneOne));
//		lanes.clickOKBtn(laneOne);
//		Assert.assertFalse("Viewing nodes window is still displayed after OK button was clicked.", lanes.isViewingNodesWindowDisplayed(laneID));
//
//		//Click Logs and verify Search Criteria and App Logs table display
//		LogsPartialPage logs = lanes.clickLogsTab();
//		logs.checkLogSections();
//
//		//Verify expected buttons display in Search Criteria section
//		logs.checkAllSearchOptions();
//		logs.checkAllSearchBtns();
//
//		//Verify expected columns display in App Logs section
//		logs.checkAllAppLogsColumnHeaderCells();
//
//		//Click Search
//		LogSearchResultsPartialPage logSearchResultsPage = logs.clickSearchBtn();
//
//		//Verify results display
//		Assert.assertTrue("No results are displayed in App Log Results table following a search.", logSearchResultsPage.getCountOfAppLogSearchRowsDisplayed() > 0);
//
//		//Click Command History
//		CommandHistoryPartialPage commandHistory = logs.clickCommandHistoryTab();
//
//		//Verify expected columns display
//		commandHistory.checkAllColumnHeaderCells();
//
//		//Verify commands made during execution display
//		commandHistory.checkVehicleInjectionCommand(command, command.getSimTime());
//
//		//Log out
//		commandHistory.logout(testUser);
//	}
//}
