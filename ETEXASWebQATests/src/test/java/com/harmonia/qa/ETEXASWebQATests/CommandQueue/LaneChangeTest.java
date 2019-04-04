package com.harmonia.qa.ETEXASWebQATests.CommandQueue;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.Execution;
import com.harmonia.qa.ETEXASWebQATests.entities.LaneChangeCommand;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;

/**
 * Test class which executes steps for the Insert Lane Change Command test,
 * TC-050
 *
 * @author llaroussini
 */
public class LaneChangeTest extends ETexasAfterTestResetTestBase {

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
     * Lane change command used throughout the test case
     */
    private LaneChangeCommand command;

    /**
     * Random seed for execution used in test case
     */
    private String rndmSeed = "1234";

    /**
     * Test setup.
     */
    //TODO - update for version 3.0
    //	@Before
    //	public void warmUp() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");
    //		//Get test user and test simulation
    //		testUser = ETexasUserFactory.getUser(true); //Get a random user.
    //		testSim = ETexasEntityManager.getTemplateSimulation(defaultEx05Sim); //get default EX_05 simulation
    //		testSim.setUser(testUser);
    //		ETexasEntityManager.addEntities(testUser, testSim);
    //
    //		//Register user and create new simulation from template
    //		LandingPage landing = ETexasUserUtils.userRegistration(testUser);
    //		ETexasSimulationUtils.createTemplateSimulation(testSim);
    //
    //		//Create an execution with an in-progress status
    //		String execName = ETexasExecutionUtils.createNewExecutionWithKnownRandomSeed(testSim, rndmSeed);
    //
    //		//Create execution entity
    //		List<Execution> execList = new ArrayList<Execution>(1);
    //		execution = new Execution();
    //		execution.setSimulation(testSim);
    //		execution.setName(execName);
    //		execution.setStatus(Status.NOT_STARTED);
    //		execList.add(execution);
    //
    //		//Create command entity
    //		List<LaneChangeCommand> commandList = new ArrayList<LaneChangeCommand>(1);
    //		command = new LaneChangeCommand();
    //		command.setCommandType(CommandType.LANE_CHANGE);
    //		command.setVehicleID("1");
    //		command.setLaneChangeCommandType(LaneChangeCommandType.CHANGE_LANE_LEFT);
    //		command.setExecution(execution);
    //		execution.setLaneChangeCommands(commandList);
    //		commandList.add(command);
    //
    //		//Set execution with associated simulation
    //		testSim.setExecutions(execList);
    //		ETexasEntityManager.addEntities(execution, command);
    //
    //		//User is logged in
    //		landing.loginAs(testUser);
    //	}

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Test steps for TC-050
    //	 */
    //	@Test
    //	public void laneChangeCommandTestExternal() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");
    //		//Ensure the Simulations page is loaded
    //		SimulationsPage simPage = getPage(SimulationsPage.class);
    //
    //		//Select execution, click control execution, and verify Executions Details page displays
    //		simPage.selectSimCheckBox(testSim, true);
    //		simPage.selectNewestExecutionCheckBox(true);
    //		ExecutionsPage execPage = simPage.clickControlExecution();
    //		Assert.assertTrue("Executions header could not be found.", execPage.isExecutionDetailsHeaderDisplayed());
    //
    //		//Step through execution by 25 steps
    //		execPage.enterSteps("25");
    //		execPage.clickNextStep();
    //		String vehicleId = command.getVehicleID();
    //		VehiclesPartialPage vehicles = execPage.clickVehiclesTab();
    //		Assert.assertTrue("Vehicle rows do not display after 25 steps into execution.", vehicles.areVehicleRowsDisplayed());
    //		Assert.assertTrue("Vehicle row containing Vehicle ID of " + vehicleId + " could not be found.", vehicles.isVehicleRowDisplayed(vehicleId));
    //		String rows = vehicles.getVehicleRows();
    //		String originalSimTime = execPage.getSimTime().substring(0, 2);
    //		String originalLaneID = vehicles.getVehicleLaneID(vehicleId);
    //
    //		//Click Commands button, select Lane Change - verify Inject Lane Change window displays
    //		execPage.clickCommandsBtn();
    //		LaneChangeForm laneChange = execPage.clickLaneChange();
    //		Assert.assertTrue("Inject Lane Change window could not be found.", laneChange.isLaneChangeHeaderDisplayed());
    //
    //		//Verify help and close icons display in header
    //		laneChange.checkLaneChangeHeaderIcons();
    //
    //		//Click the help icon, verify help window displays
    //		laneChange.clickLaneChangeHelpIcon();
    //		Assert.assertTrue("Lane Change help window is not displayed when Help icon is clicked.", laneChange.isLaneChangeHelpHeaderDisplayed());
    //		Assert.assertTrue("Lane Change help content is not displayed when Help icon is clicked.", laneChange.isLaneChangeHelpContentDisplayed());
    //
    //		//Verify OK button displays, click OK, verify help window closes
    //		Assert.assertTrue("OK button could not be found in help window.", laneChange.isHelpOKBtnDisplayed());
    //		laneChange.clickHelpOKBtn();
    //		Assert.assertFalse("Lane Change help window is still displayed after OK button from Help window is clicked.", laneChange.isLaneChangeHelpHeaderDisplayed());
    //
    //		//Verify expected fields (Vehicle ID drop down and Command drop down) display in Inject Lane Change window
    //		laneChange.checkFields();
    //
    //		//Verify expected buttons (OK, Reset, Cancel) display in Inject Lane Change window
    //		laneChange.checkBtns();
    //
    //		//Click Vehicle ID drop down and verify ID's displayed match ID's displayed in Vehicles tab
    //		laneChange.clickVehicleIdDropdown();
    //		laneChange.checkVehicleListSize(rows);
    //		laneChange.clickVehicleIdDropdown(); //click again to de-select the drop down
    //
    //		//Select a Vehicle ID
    //		laneChange.selectVehicleId(command);
    //
    //		//Click Command drop down and verify all expected commands display (Change Lane Left and Change Lane Right)
    //		laneChange.clickCommandDropdown();
    //		laneChange.checkLaneCommandList();
    //		laneChange.clickCommandDropdown(); //click again to de-select the drop down
    //
    //		//Select a command option
    //		laneChange.selectLaneChangeCommandType(command);
    //
    //		//Verify fields display as expected
    //		laneChange.checkDisplayedSetFields(command);
    //
    //		//Click Reset
    //		laneChange.clickReset();
    //
    //		//Verify all fields are cleared
    //		laneChange.checkClearedFields();
    //
    //		//Enter valid values, click Cancel, verify command is not displayed
    //		laneChange.setCommandInfo(command);
    //		laneChange.clickCancel();
    //		Assert.assertFalse("Command queue list is not empty after clicking cancel for lane change command.", execPage.isCommandQueueListDisplayed());
    //
    //		//Click Commands, select Speed Change, enter valid values, click OK, verify command is displayed
    //		execPage.clickCommandsBtn();
    //		execPage.clickLaneChange();
    //		laneChange.setCommandInfo(command);
    //		laneChange.clickLaneChangeOK();
    //		Assert.assertTrue("Command could not be found in command queue list.", execPage.isCommandQueueRowDisplayed(command.getLaneChangeCommandType().getLabel()));
    //
    //		//Click Next Step
    //		execPage.clickNextStep();
    //
    //		//Verify command no longer displays in Command Queue
    //		Assert.assertFalse("Command still displayed in command queue list after advancing the execution by 25 steps.",
    //				execPage.isCommandQueueRowDisplayed(command.getLaneChangeCommandType().getLabel()));
    //
    //		//Click Command History tab, verify command displays with sim time command was injected
    //		CommandHistoryPartialPage commands = execPage.clickCommandHistoryTab();
    //		commands.checkLaneCommand(command, originalSimTime);
    //
    //		//Step through the execution by 25 and verify speed changes accordingly, verify speed stabilizes to match speed entered previously
    //		execPage.clickNextStep();
    //		execPage.clickVehiclesTab();
    //		Assert.assertTrue("Vehicle row containing Vehicle ID of " + vehicleId + " could not be found.", vehicles.isVehicleRowDisplayed(vehicleId));
    //		String newLaneID = vehicles.getVehicleLaneID(vehicleId);
    //		Assert.assertNotSame("Vehicle lane ID did not update after lane change command was injected.", originalLaneID, newLaneID);
    //
    //		//Click Simulations, select simulation and execution, click Finish, verify status updated to completed
    //		execPage.clickSimulations();
    //		simPage.selectSimCheckBox(testSim, true);
    //		simPage.selectNewestExecutionCheckBox(true);
    //		simPage.clickFinish();
    //		simPage.isExecutionAndStatusDisplayed(execution.getName(), ExecutionStatus.COMPLETED);
    //
    //		//Log out
    //		simPage.logout(testUser);
    //
    //	}

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Test Steps for ITC-021
    //	 */
    //	@Test
    //	public void laneChangeCommandTestInternal(){
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");
    //		//Ensure the Simulations page is loaded
    //		SimulationsPage simPage = getPage(SimulationsPage.class);
    //
    //		//Select execution, click control execution, and verify Executions Details page displays
    //		simPage.selectSimCheckBox(testSim, true);
    //		simPage.selectNewestExecutionCheckBox(true);
    //		ExecutionsPage execPage = simPage.clickControlExecution();
    //		Assert.assertTrue("Executions header could not be found.", execPage.isExecutionDetailsHeaderDisplayed());
    //
    //		//Step through execution by 25 steps
    //		execPage.enterSteps("25");
    //		execPage.clickNextStep();
    //
    //		//Click Commands button, select Speed Change - verify Inject Speed Change window displays
    //		execPage.clickCommandsBtn();
    //		LaneChangeForm laneChange = execPage.clickLaneChange();
    //
    //		//Clicks the OK button in the Lange Change Form with all fields blank.
    //		laneChange.clickLaneChangeOK();
    //
    //		//Assertions to verify that the 'This field is required' error appears when using no data in any field and clicking OK.
    //		laneChange.checkRequiredFieldErrorAllFields();
    //
    //		//Selects a Vehicle and a command.
    //		laneChange.selectVehicleId(command);
    //		laneChange.selectLaneChangeCommandType(command);
    //
    //		//Clicks the OK button to inject the lane change.
    //		laneChange.clickLaneChangeOK();
    //
    //		//Asserts that the command appears in the command queue after clicking OK.
    //		Assert.assertTrue("Command could not be found in command queue list.", execPage.isCommandQueueRowDisplayed(command.getLaneChangeCommandType().getLabel()));
    //
    //		//Clicks the Finish button in the Executions page and then clicks the Simulations link to return to Simulations.
    //		execPage.clickFinish();
    //		execPage.clickSimulations();
    //
    //		//Selects the check-box for the created simulation, and then selects the check-box for the newest execution for that simulation.
    //		simPage.selectSimCheckBox(testSim, true);
    //		simPage.selectNewestExecutionCheckBox(true);
    //
    //		//Verifies that the execution status is listed as 'Completed' after finishing the execution from the Simulations page.
    //		Assert.assertTrue("The execution is not listed as Completed after selecting Finish in the Executions page",
    //				simPage.isExecutionAndStatusDisplayed(execution.getName(), ExecutionStatus.COMPLETED));
    //
    //		//Log out
    //		simPage.logout(testUser);
    //
    //
    //	}
}
