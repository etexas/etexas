package com.harmonia.qa.ETEXASWebQATests.CommandQueue;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.Execution;
import com.harmonia.qa.ETEXASWebQATests.entities.SignalChangeCommand;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;

/**
 * Test class which executes steps for the Insert Signal Change Command test,
 * TC-051
 *
 * @author llaroussini
 */
public class SignalChangeTest extends ETexasAfterTestResetTestBase {

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
     * Signal change command used throughout the test case
     */
    private SignalChangeCommand command;

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
    //		List<SignalChangeCommand> commandList = new ArrayList<SignalChangeCommand>(1);
    //		command = new SignalChangeCommand();
    //		command.setCommandType(CommandType.SIGNAL_CHANGE);
    //		command.setTime("20");
    //		command.setSignalChangeCommandType(SignalChangeCommandType.CHANGE_SIGNAL);
    //		command.setExecution(execution);
    //		execution.setSignalChangeCommands(commandList);
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
    //	 * Test steps for TC-051
    //	 */
    //	@Test
    //	public void signalChangeCommandExternalTest() {
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
    //		//Step through execution by 25 steps, click Signals tab and verify lanes and signals display
    //		execPage.enterSteps("25");
    //		execPage.clickNextStep();
    //		String originalSimTime = execPage.getSimTime();
    //		SignalsPartialPage signals = execPage.clickSignalsTab();
    //		Lane laneOne = testSim.getLanes().get(0);//lane with signal expected (and associated data) is known by static execution created in warm-up
    //		String laneID = laneOne.getLaneID().getLabel();
    //		Assert.assertTrue("Signal rows could not be found.", signals.areSignalRowsDisplayed());
    //		Assert.assertTrue("Signal row containing Lane ID of " + laneID + " could not be found.", signals.isSignalRowDisplayed(laneID));
    //		signals.checkSignalInfo(laneOne, "STEADY", "GREEN", "7.5");
    //
    //		//Click Commands button, select Signal Change - verify Inject Signal Change window displays
    //		execPage.clickCommandsBtn();
    //		SignalChangeForm signalForm = execPage.clickSignalChange();
    //		Assert.assertTrue("Inject Signal Change window could not be found.", signalForm.isSignalChangeHeaderDisplayed());
    //
    //		//Verify help and close icons display in header
    //		signalForm.checkSignalChangeHeaderIcons();
    //
    //		//Click the help icon, verify help window displays
    //		signalForm.clickSignalChangeHelpIcon();
    //		Assert.assertTrue("Signal Change help window is not displayed when Help icon is clicked.", signalForm.isSignalChangeHelpHeaderDisplayed());
    //		Assert.assertTrue("Signal Change help contnent is not displayed when Help icon is clicked.", signalForm.isSignalChangeHelpContentDisplayed());
    //
    //		//Verify OK button displays, click OK, verify help window closes
    //		Assert.assertTrue("OK button could not be found in help window.", signalForm.isHelpOKBtnDisplayed());
    //		signalForm.clickHelpOKBtn();
    //		Assert.assertFalse("Signal Change help window is still displayed after OK button from Help window is clicked.", signalForm.isSignalChangeHelpHeaderDisplayed());
    //
    //		//Verify expected fields (Time text box and Command drop down) display in Inject Signal Change window
    //		signalForm.checkFields();
    //
    //		//Verify expected buttons (OK, Reset, Cancel) display in Inject Signal Change window
    //		signalForm.checkBtns();
    //
    //		//Enter a time
    //		signalForm.setTime(command);
    //
    //		//Click Command drop down and verify all expected commands display (Change Signal and Hold Signal)
    //		signalForm.clickCommandDropdown();
    //		signalForm.checkSignalCommandList();
    //		signalForm.clickCommandDropdown(); //click again to de-select the drop down
    //
    //		//Select a command option
    //		signalForm.selectSignalChangeCommandType(command);
    //
    //		//Verify fields display as expected
    //		signalForm.checkDisplayedSetFields(command);
    //
    //		//Click Reset
    //		signalForm.clickReset();
    //
    //		//Verify all fields are cleared
    //		signalForm.checkClearedFields();
    //
    //		//Enter valid values, click Cancel, verify command is not displayed
    //		signalForm.setSignalCommandInfo(command);
    //		signalForm.clickCancel();
    //		Assert.assertFalse("Command queue list is not empty after clicking cancel for lane change command.", execPage.isCommandQueueListDisplayed());
    //
    //		//Click Commands, select Signal Change, enter valid values, click OK, verify command is displayed
    //		execPage.clickCommandsBtn();
    //		execPage.clickSignalChange();
    //		signalForm.setSignalCommandInfo(command);
    //		signalForm.clickSignalChangeOK();
    //		Assert.assertTrue("Command could not be found in command queue list.", execPage.isCommandQueueRowDisplayed(SignalChangeCommandType.CHANGE_SIGNAL.getLabel()));
    //
    //		//Click Next Step
    //		execPage.clickNextStep();
    //
    //		//Verify command no longer displays in Command Queue
    //		Assert.assertFalse("Command still displayed in command queue list after advancing the execution by 25 steps.",
    //				execPage.isCommandQueueRowDisplayed(SignalChangeCommandType.CHANGE_SIGNAL.getLabel()));
    //
    //		//Click Command History tab, verify command displays with sim time command was injected
    //		CommandHistoryPartialPage commands = execPage.clickCommandHistoryTab();
    //		commands.checkSignalCommand(command, originalSimTime);
    //
    //		//Step through the execution by 25 and verify signal changes accordingly
    //		execPage.clickNextStep();
    //		execPage.clickSignalsTab();
    //		Assert.assertTrue("Signal rows could not be found.", signals.areSignalRowsDisplayed());
    //		Assert.assertTrue("Signal row containing Lane ID of " + laneID + " could not be found.", signals.isSignalRowDisplayed(laneID));
    //		signals.checkSignalInfo(laneOne, "STEADY", "RED", "43");
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
    //
    //	}

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Test steps for ITC-022
    //	 */
    //	@Test
    //	public void signalChangeCommandInteranlTest() {
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
    //		//Step through execution by 25 steps, click Signals tab and verify lanes and signals display
    //		execPage.enterSteps("25");
    //		execPage.clickNextStep();
    //
    //		//Click Commands button, select Signal Change - verify Inject Signal Change window displays
    //		execPage.clickCommandsBtn();
    //		SignalChangeForm signalChange = execPage.clickSignalChange();
    //
    //		signalChange.clickSignalChangeOK();
    //
    //		//Assertions to verify that the 'This field is required' error appears when using no data in any field and clicking OK.
    //		signalChange.checkRequiredFieldErrorAllFields();
    //
    //		//Assertions to verify that invalid input into Time displays as 'not a valid number.'
    //		signalChange.setSignalCommandInfo(command);
    //		signalChange.setTime("test");
    //		Assert.assertTrue("A valid number error was not displayed when entering non-numerical values into the Time field.", signalChange.isInvalidNumberTimeErrorDisplayed());
    //
    //		//Sets signal change time and clicks OK
    //		signalChange.setTime(command.getTime());
    //		signalChange.clickSignalChangeOK();
    //
    //		//Asserts that the command appears in the command queue after clicking OK.
    //		Assert.assertTrue("Command could not be found in command queue list.", execPage.isCommandQueueRowDisplayed(command.getSignalChangeCommandType().getLabel()));
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
    //	}

}