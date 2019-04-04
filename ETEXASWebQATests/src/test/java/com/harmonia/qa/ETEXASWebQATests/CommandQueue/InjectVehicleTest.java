package com.harmonia.qa.ETEXASWebQATests.CommandQueue;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.Execution;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.VehicleInjectionCommand;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;

/**
 * Test class which executes steps for the Inject Vehicle test, TC-052
 *
 * @author llaroussini
 */
public class InjectVehicleTest extends ETexasAfterTestResetTestBase {

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
    //		//Set execution with associated simulation
    //		testSim.setExecutions(execList);
    //		ETexasEntityManager.addEntities(execution, command);
    //
    //		//User is logged in
    //		landing.loginAs(testUser);
    //	}

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Test steps for TC-052
    //	 */
    //	@Test
    //	public void injectVehicleCommandExternalTest() {
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
    //		VehiclesPartialPage vehicles = execPage.clickVehiclesTab();
    //		Assert.assertTrue("Vehicle rows do not display after 25 steps into execution.", vehicles.areVehicleRowsDisplayed());
    //		String originalSimTime = execPage.getSimTime().substring(0, 2);
    //
    //		//Click Inject Vehicle
    //		InjectVehicleForm injectVehicle = execPage.clickInjectVehicle();
    //
    //		//Verify Inject Vehicle form displays
    //		Assert.assertTrue("Inject Vehicle form window is not displayed.", injectVehicle.isInjectVehicleHeaderDisplayed());
    //
    //		//Verify icons (help and close)
    //		injectVehicle.checkInjectVehicleHeaderIcons();
    //
    //		//Verify expected elements display (Lane dropdown, Speed text box, OK button, Reset button, and Cancel button)
    //		injectVehicle.checkFields();
    //		injectVehicle.checkBtns();
    //
    //		//Click help icon and verify help window displays
    //		injectVehicle.clickInjectVehicleHelpIcon();
    //		Assert.assertTrue("Inject Vehicle Help window is not displayed.", injectVehicle.isInjectVehicleHelpHeaderDisplayed());
    //		Assert.assertTrue("Inject Vehicle help content is not displayed.", injectVehicle.isInjectVehicleHelpContentDisplayed());
    //
    //		//Verify OK button displays, click OK, verify help window closes
    //		Assert.assertTrue("OK button could not be found in Inject Vehicle help window.", injectVehicle.isHelpOKBtnDisplayed());
    //		injectVehicle.clickHelpOKBtn();
    //		Assert.assertFalse("Inject Vehicle Help window is still displayed after clicking OK.", injectVehicle.isInjectVehicleHelpHeaderDisplayed());
    //
    //		//Click Lane dropdown and verify all expected Lanes display
    //		injectVehicle.clickLaneDropdown();
    //		injectVehicle.checkLaneListSize(8);
    //		injectVehicle.clickLaneDropdown();
    //
    //		//Select a Lane
    //		injectVehicle.selectLane(command);
    //
    //		//Enter a value in the Speed text box
    //		injectVehicle.setSpeed(command);
    //
    //		//Click Reset and verify all fields clear
    //		injectVehicle.checkDisplayedSetFields(command.getLane(), command.getSpeed());
    //		injectVehicle.clickReset();
    //		injectVehicle.checkDisplayedSetFields(null, "");
    //
    //		//Select a lane and enter a speed
    //		injectVehicle.selectLane(command);
    //		injectVehicle.setSpeed(command);
    //
    //		//Click Cancel, verify window closes, verify command is not displayed in queue
    //		injectVehicle.clickCancel();
    //		Assert.assertFalse("Inject Vehicle form window is still displayed after clicking Cancel.", injectVehicle.isInjectVehicleHeaderDisplayed());
    //		Assert.assertFalse("Command queue list is not empty after clicking cancel for inject vehicle command.", execPage.isCommandQueueListDisplayed());
    //
    //		//Click Inject Vehicle button, select a lane and enter a speed
    //		execPage.clickInjectVehicle();
    //		injectVehicle.selectLane(command);
    //		injectVehicle.setSpeed(command);
    //
    //		//Click OK, verify window closes, verify command is displayed in queue
    //		injectVehicle.clickInjectVehicleOK();
    //		String commandName = command.getCommandType().getLabel();
    //		Assert.assertFalse("Inject Vehicle form window is still displayed after clicking OK.", injectVehicle.isInjectVehicleHeaderDisplayed());
    //		Assert.assertTrue("Vehicle Injection command could not be found in command queue list.", execPage.isCommandQueueRowDisplayed(commandName));
    //
    //		//Click Next Step
    //		execPage.clickNextStep();
    //
    //		//Verify command is no longer displayed in queue
    //		Assert.assertFalse("Command still displayed in command queue list after advancing the execution by 25 steps.", execPage.isCommandQueueRowDisplayed(commandName));
    //
    //		//Click Command History tab
    //		CommandHistoryPartialPage commands = execPage.clickCommandHistoryTab();
    //
    //		//Verify command displays, time of command matches sim time the command was injected and all details display as expected (Command, Vehicle ID, Injection Time, and Speed)
    //		commands.checkVehicleInjectionCommand(command, originalSimTime);
    //
    //		//Get vehicle ID
    //		String vehicleID = commands.getVehicleIDFromVehicleInjectionCommand(command);
    //
    //		//Click Vehicles tab, verify vehicle displays
    //		commands.clickVehiclesTab();
    //		Assert.assertTrue("Injected vehicle is not displayed as expected.", vehicles.isVehicleRowDisplayed(vehicleID));
    //
    //		//Verify speed matches the speed entered previously
    //		String displayedSpeed = (vehicles.getVehicleSpeed(vehicleID)).substring(0, 1);
    //		Assert.assertEquals("Speed for injected vehicle not displayed as expected", "9", displayedSpeed);
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
    //	 * Test steps for ITC-023
    //	 */
    //	@Test
    //	public void injectVehicleCommandInternalTest() {
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
    //		//Click Inject Vehicle and clicks OK
    //		InjectVehicleForm injectVehicle = execPage.clickInjectVehicle();
    //		injectVehicle.clickInjectVehicleOKErrorExpected();
    //
    //		//Asserts that errors appear when using empty data
    //		injectVehicle.checkRequiredFieldErrorAllFields();
    //
    //		//Selects a lane and enters non-numerical data into the Speed field
    //		injectVehicle.selectLane(command.getLane());
    //		injectVehicle.setSpeed("---");
    //
    //		//Asserts an error appears when entering non-numerical values into the Speed field
    //		Assert.assertTrue("Error was not displayed when inserting non-numerical values into the Speed field.", injectVehicle.isInvalidNumberSpeedErrorDisplayed());
    //
    //		//Sets the speed to a negative value and asserts an error appears.
    //		injectVehicle.setSpeed("-1");
    //		Assert.assertTrue("Error was not displayed when inserting a negative value into Speed field.", injectVehicle.isMinimumNumberSpeedErrorDisplayed());
    //
    //		//Enters a valid Speed and clicks the OK button, then verifies the command appears in the Queue
    //		injectVehicle.setSpeed(command.getSpeed());
    //		injectVehicle.clickInjectVehicleOK();
    //		Assert.assertTrue("Vehicle Injection command could not be found in command queue list.", execPage.isCommandQueueRowDisplayed(command.getCommandType().getLabel()));
    //
    //		//Clicks Finish and then goes to Simulations
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
