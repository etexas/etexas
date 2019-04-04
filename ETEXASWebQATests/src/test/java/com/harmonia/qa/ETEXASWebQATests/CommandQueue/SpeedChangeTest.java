package com.harmonia.qa.ETEXASWebQATests.CommandQueue;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.Execution;
import com.harmonia.qa.ETEXASWebQATests.entities.SpeedCommand;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;

/**
 * Test class which executes steps for the Speed Change test, TC-049
 *
 * @author llaroussini
 */
public class SpeedChangeTest extends ETexasAfterTestResetTestBase {

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
    private SpeedCommand command;

    /**
     * Random seed for execution used in test case
     */
    private String rndmSeed = "1234";

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Test setup.
    //	 */
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
    //		List<SpeedCommand> commandList = new ArrayList<SpeedCommand>(1);
    //		command = new SpeedCommand();
    //		command.setCommandType(CommandType.SPEED_CHANGE);
    //		command.setSpeed("0.4");
    //		command.setVehicleID("1");
    //		command.setSpeedChangeCommand(SpeedChangeCommand.MAX_DECELERATE);
    //		command.setExecution(execution);
    //		execution.setSpeedCommands(commandList);
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
    //	 * Test steps for TC-049
    //	 */
    //	@Test
    //	public void speedChangeCommandExternalTest() {
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
    //		String currentSimTime = execPage.getSimTime();
    //		Double currentSpeed = Double.parseDouble(vehicles.getVehicleSpeed(vehicleId).substring(0, 3));
    //		Double currentDistanceToIntersection = Double.parseDouble(vehicles.getVehicleDistanceToIntersection(vehicleId).substring(0, 8));
    //		Double currentTimeToIntersection = Double.parseDouble(vehicles.getVehicleTimeToIntersectionPosition(vehicleId).substring(0, 5));
    //
    //		//Click Commands button, select Speed Change - verify Inject Speed Change window displays
    //		execPage.clickCommandsBtn();
    //		SpeedChangeForm speedChange = execPage.clickSpeedChange();
    //		Assert.assertTrue("Inject Speed Change window could not be found.", speedChange.isSpeedChangeHeaderDisplayed());
    //
    //		//Verify help and close icons display in header
    //		speedChange.checkSpeedChangeHeaderIcons();
    //
    //		//Click the help icon, verify help window displays
    //		speedChange.clickSpeedChangeHelpIcon();
    //		Assert.assertTrue("Speed Change help window is not displayed when Help icon is clicked.", speedChange.isSpeedChangeHelpHeaderDisplayed());
    //
    //		//Verify OK button displays, click OK, verify help window closes
    //		Assert.assertTrue("OK button could not be found in help window.", speedChange.isHelpOKBtnDisplayed());
    //		speedChange.clickHelpOKBtn();
    //		Assert.assertFalse("Speed Change help window is still displayed after OK button from Help window is clicked.", speedChange.isSpeedChangeHelpHeaderDisplayed());
    //
    //		//Verify expected fields (Vehicle ID drop down, Command drop down, and Speed text box) display in Inject Speed Change window
    //		speedChange.checkFields();
    //
    //		//Verify expected buttons (OK, Reset, Cancel) display in Inject Speed Change window
    //		speedChange.checkBtns();
    //
    //		//Click Vehicle ID drop down and verify ID's displayed match ID's displayed in Vehicles tab
    //		speedChange.clickVehicleIdDropdown();
    //		speedChange.checkVehicleListSize(rows);
    //		speedChange.clickVehicleIdDropdown(); //click again to de-select the drop down
    //
    //		//Select a Vehicle ID
    //		speedChange.selectVehicleId(command);
    //
    //		//Click Command drop down and verify all expected commands display (Normal Accelerate, Max Accelerate, Normal Decelerate, and Max Decelerate)
    //		speedChange.clickCommandDropdown();
    //		speedChange.checkSpeedCommandList();
    //		speedChange.clickCommandDropdown(); //click again to de-select the drop down
    //
    //		//Select a command option
    //		speedChange.selectSpeedChangeCommand(command);
    //
    //		//Enter value in Speed text box
    //		speedChange.setSpeed(command);
    //
    //		//Verify fields display as expected
    //		speedChange.checkDisplayedSetFields(command);
    //
    //		//Click Reset
    //		speedChange.clickReset();
    //
    //		//Verify all fields are cleared
    //		speedChange.checkClearedFields();
    //
    //		//Enter valid values, click Cancel, verify command is not displayed
    //		speedChange.setCommandInfo(command);
    //		speedChange.clickCancel();
    //		Assert.assertFalse("Command queue list is not empty after clicking cancel for speed change command.", execPage.isCommandQueueListDisplayed());
    //
    //		//Click Commands, select Speed Change, enter valid values, click OK, verify command is displayed
    //		execPage.clickCommandsBtn();
    //		execPage.clickSpeedChange();
    //		speedChange.setCommandInfo(command);
    //		speedChange.clickSpeedChangeOK();
    //		Assert.assertTrue("Command could not be found in command queue list.", execPage.isCommandQueueRowDisplayed(SpeedChangeCommand.MAX_DECELERATE.getLabel()));
    //
    //		//Click Next Step
    //		execPage.clickNextStep();
    //
    //		//Verify command no longer displays in Command Queue
    //		Assert.assertFalse("Command still displayed in command queue list after advancing the execution by 25 steps.",
    //				execPage.isCommandQueueRowDisplayed(command.getSpeedChangeCommand().getLabel()));
    //
    //		//Click Command History tab, verify command displays with sim time command was injected
    //		CommandHistoryPartialPage commands = execPage.clickCommandHistoryTab();
    //		commands.checkSpeedCommand(command, currentSimTime);
    //
    //		//Step through the execution by 25 and verify speed changes accordingly, verify speed stabilizes to match speed entered previously
    //		execPage.clickNextStep();
    //		execPage.clickVehiclesTab();
    //		Assert.assertTrue("Vehicle row containing Vehicle ID of " + vehicleId + " could not be found.", vehicles.isVehicleRowDisplayed(vehicleId));
    //		Double newSpeed = Double.parseDouble(vehicles.getVehicleSpeed(vehicleId).substring(0, 3));
    //		Double newDistanceToIntersection = Double.parseDouble(vehicles.getVehicleDistanceToIntersection(vehicleId).substring(0, 8));
    //		String newTimeToIntersection = vehicles.getVehicleTimeToIntersectionPosition(vehicleId).substring(0, 5);
    //		Assert.assertTrue("Vehicle speed value did not update as expected.", currentSpeed > newSpeed);
    //		Assert.assertNotSame("Vehicle speed value did not update as expected.", currentDistanceToIntersection < newDistanceToIntersection);
    //		Assert.assertNotSame("Vehicle speed value did not update as expected.", currentTimeToIntersection, newTimeToIntersection);
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
    //	}

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Test Steps for ITC-020
    //	 */
    //	@Test
    //	public void speedChangeCommandTestInternal(){
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
    //		SpeedChangeForm speedChange = execPage.clickSpeedChange();
    //
    //		//Click the OK button to make changes to the Speed.
    //		speedChange.clickSpeedChangeOK();
    //
    //		//Assertions to verify that the 'This field is required' error appears when using no data in any field and clicking OK.
    //		speedChange.checkRequiredFieldErrorAllFields();
    //
    //		//Selects a vehicle by ID and a speed change command.
    //		speedChange.selectVehicleId(command.getVehicleID());
    //		speedChange.selectSpeedChangeCommand(command);
    //
    //		//Attempts to check that the error is reported when attempting to use null data for the speed with the Vehcile ID and Command set.
    //		speedChange.clickSpeedChangeOK();
    //		Assert.assertTrue("'This field is required' does not appear when entering empty data into Speed field.", speedChange.isSpeedRequiredErrorDisplayed());
    //
    //		//Checks that inputting non-numerical data into the Speed box throws an 'is not a valid number.' error with the Vehicle ID and Command set.
    //		speedChange.setSpeed("speed");
    //		Assert.assertTrue("An error was not displayed when entering non-numerical data into the speed field.", speedChange.isInvalidNumberSpeedErrorDisplayed());
    //
    //		//Sets a speed for the Speed Change command and then clicks OK.
    //		speedChange.setSpeed(command.getSpeed());
    //		speedChange.clickSpeedChangeOK();
    //
    //		//Asserts that the command appears in the command queue after clicking OK.
    //		Assert.assertTrue("Command could not be found in command queue list.", execPage.isCommandQueueRowDisplayed(command.getSpeedChangeCommand().getLabel()));
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
    //	}
}
