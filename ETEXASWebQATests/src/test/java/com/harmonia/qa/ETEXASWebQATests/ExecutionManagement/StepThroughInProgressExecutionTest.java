package com.harmonia.qa.ETEXASWebQATests.ExecutionManagement;

import org.junit.Before;

import com.harmonia.qa.ETEXASWebQATests.entities.Detector;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.Execution;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;

/**
 * Test class which executes steps for the Step Through an In-Progress Execution
 * test, TC-022 and ITC-018
 *
 * @author llaroussini
 */
public class StepThroughInProgressExecutionTest extends ETexasAfterTestResetTestBase {

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
     * Execution name using in TC
     */
    private String newExec;

    /**
     * Detector used throughout test case
     */
    private Detector detector;

    /**
     * Simulation object used in the internal test case
     */
    private TemplateSimulation testITCSim;

    /**
     * Name of the execution created to be used in ITC
     */
    private String newITCExecName;

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");
        //Get test user, test simulation, and RSE device
        testUser = ETexasUserFactory.getUser(true); //Get a random user.

        //Register user and create new simulation from template
        ETexasUserUtils.userRegistration(testUser);

    }

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Test steps for TC-022
    //	 */
    //	@Test
    //	public void stepThroughExecutionExternalTest() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");
    //
    //		//Create simulation with RSE device and a new execution
    //		//Note: this cannot be done in the warm-up as only one non-completed execution can exist at any given time
    //		testSim = ETexasEntityManager.getTemplateSimulation(defaultEx05Sim); //get default EX_05 simulation
    //		testSim.setUser(testUser);
    //		ETexasEntityManager.addEntities(testUser, testSim);
    //		//Create RSE Device
    //		detector = ETexasEntityManager.getEntity(Detector.class);
    //		detector.setSimulation(testSim);
    //		ETexasEntityManager.addEntity(detector);
    //		ETexasSimulationUtils.createTemplateSimulation(testSim); //Remove and re-enable below following resolution of BUG 12582
    //		//ETexasSimulationUtils.createTemplateSimulationWithDetector(testSim, detector);
    //		newExec = ETexasExecutionUtils.createNewExecutionWithKnownRandomSeed(testSim, "1234");
    //		//Create execution entities
    //		List<Execution> execList = new ArrayList<Execution>(3);
    //		execution = new Execution();
    //		execution.setSimulation(testSim);
    //		execution.setName(newExec);
    //		execution.setStatus(Status.NOT_STARTED);
    //		execList.add(execution);
    //		//Set execution with associated simulation
    //		testSim.setExecutions(execList);
    //		ETexasEntityManager.addEntities(execution);
    //
    //		//Login and ensure the Simulations page is loaded
    //		SimulationsPage simPage = goToLandingPage().loginAs(testUser);
    //
    //		//Select execution, click control execution, and verify Executions Details page displays
    //		simPage.selectSimCheckBox(testSim.getName(), true);
    //		simPage.selectExecutionCheckBox(newExec, true);
    //		ExecutionsPage execPage = simPage.clickControlExecution();
    //		Assert.assertTrue("Executions header could not be found.", execPage.isExecutionDetailsHeaderDisplayed());
    //
    //		//Verify expected elements display (Sim Time, Remaining Steps, and all buttons)
    //		Assert.assertTrue("Sim Time area could not be found.", execPage.isSimTimeAreaDisplayed());
    //		Assert.assertTrue("Steps text box could not be found.", execPage.isStepsTextBoxDisplayed());
    //		Assert.assertTrue("Remaining Steps area could not be found.", execPage.isRemaininsStepsAreDisplayed());
    //		execPage.checkAllExecutionControlBtns();
    //
    //		//Verify sim time value is 0.0, remaining steps value is 2400, and vehicle rows are not displayed initially
    //		String startingRemainingSteps = execPage.getRemainingSteps();
    //		String startingSimTime = execPage.getSimTime();
    //		Assert.assertEquals("Starting remaining steps value not displayed as expected.", "2400", startingRemainingSteps);
    //		Assert.assertEquals("Starting sim time value not displayed as expected.", "0.0", startingSimTime);
    //		VehiclesPartialPage vehicles = execPage.clickVehiclesTab();
    //		Assert.assertFalse("Vehicle rows are displayed prior to starting the execution.", vehicles.areVehicleRowsDisplayed());
    //
    //		//Enter 25 in the Steps selector, click Next Step, verify Vehicle rows displays, Remaining Steps value decreases and Sim Time value increases
    //		execPage.enterSteps("25");
    //		execPage.clickNextStep();
    //		Assert.assertTrue("Vehicle rows do not display after 25 steps into execution.", vehicles.areVehicleRowsDisplayed());
    //		String newRemainingSteps = execPage.getRemainingSteps();
    //		String newSimTime = execPage.getSimTime();
    //		Assert.assertEquals("Displayed remaining steps value does not decrease after advancing 25 steps.", "2375", newRemainingSteps);
    //		Assert.assertEquals("Starting sim time value does not increase as expected after advancing 25 steps.", "12.5", newSimTime);
    //
    //		//Verify all expected tabs display (Vehicles, Signals, Detectors, Lane Geometry, Logs, and Command History)
    //		execPage.checkAllActiveExecTabsDisplayed();
    //
    //		//Click Vehicles tab and and verify vehicle info displays in Vehicles table (verify data in all columns)
    //		execPage.clickVehiclesTab();
    //		String vehicleId = "1"; //vehicle ID expected (and associated data) is known by static execution created in warm-up
    //		vehicles.checkAllColumnHeaderCells();
    //		Assert.assertTrue("Vehicle row containing Vehicle ID of " + vehicleId + " could not be found.", vehicles.isVehicleRowDisplayed(vehicleId));
    //		vehicles.checkVehicleInfo(vehicleId);
    //		String currentPosition = vehicles.getVehiclePosition(vehicleId);
    //		String currentSpeed = vehicles.getVehicleSpeed(vehicleId);
    //		String currentLaneId = vehicles.getVehicleLaneID(vehicleId);
    //		String currentDistanceToIntersection = vehicles.getVehicleDistanceToIntersection(vehicleId);
    //		String currentTimeToIntersection = vehicles.getVehicleTimeToIntersectionPosition(vehicleId);
    //		String currentSignalState = vehicles.getSignalState(vehicleId);
    //
    //		//Click Next Step
    //		vehicles.clickNextStep();
    //
    //		//Verify Vehicle data updates
    //		Assert.assertTrue("Vehicle row containing Vehicle ID  of " + vehicleId + " could not be found after clicking Next Step.", vehicles.isVehicleRowDisplayed(vehicleId));
    //		vehicles.checkVehicleInfo(vehicleId);
    //		String newPosition = vehicles.getVehiclePosition(vehicleId);
    //		String newSpeed = vehicles.getVehicleSpeed(vehicleId);
    //		String newLaneId = vehicles.getVehicleLaneID(vehicleId);
    //		String newDistanceToIntersection = vehicles.getVehicleDistanceToIntersection(vehicleId);
    //		String newTimeToIntersection = vehicles.getVehicleTimeToIntersectionPosition(vehicleId);
    //		String newSignalState = vehicles.getSignalState(vehicleId);
    //		Assert.assertNotSame("Vehicle position value did not update as expected.", currentPosition, newPosition);
    //		Assert.assertNotSame("Vehicle speed value did not update as expected.", currentSpeed, newSpeed);
    //		Assert.assertNotSame("Vehicle lane ID value did not update as expected.", currentLaneId, newLaneId);
    //		Assert.assertNotSame("Vehicle distance to intersection value did not update as expected.", currentDistanceToIntersection, newDistanceToIntersection);
    //		Assert.assertNotSame("Vehicle time to intersection value did not update as expected.", currentTimeToIntersection, newTimeToIntersection);
    //		Assert.assertNotSame("Signal state value did not update as expected.", currentSignalState, newSignalState);
    //
    //		//Click Signals tab and verify signal info displays in Signals table (verify data in all columns)
    //		SignalsPartialPage signals = vehicles.clickSignalsTab();
    //		signals.checkAllColumnHeaderCells();
    //		Lane laneOne = testSim.getLanes().get(0);//lane with signal expected (and associated data) is known by static execution created in warm-up
    //		String laneID = laneOne.getLaneID().getLabel();
    //		Assert.assertTrue("Signal rows could not be found.", signals.areSignalRowsDisplayed());
    //		Assert.assertTrue("Signal row containing Lane ID of " + laneID + " could not be found.", signals.isSignalRowDisplayed(laneID));
    //		signals.checkSignalInfo(laneOne, "STEADY", "RED", "43");
    //
    //		//Click Next Step
    //		signals.clickNextStep();
    //
    //		//Verify Signal data updates
    //		Assert.assertTrue("Signal row containing Lane ID of 1 could not be found after clicking Next Step.", signals.isSignalRowDisplayed("1"));
    //		signals.checkSignalInfo(laneOne, "STEADY", "RED", "30.5");
    //
    //		//Re-enable after resolution of BUG 12582
    //		//Click Detectors tab and verify all detector's associated with the execution and their info displays in Detectors table (verify data in all columns)
    //		//DetectorsPartialPage detectors = signals.clickDetectorsTab();
    //		//detectors.checkAllColumnHeaderCells();
    //		//detectors.checkDetectorInfo(detector);
    //
    //		//Click Lane Geometry tab and verify all lanes display with associated data
    //		LaneGeometryPartialPage lanes = signals.clickLaneGeometryTab(); //Remove and re-enable below following resolution of BUG 12582
    //		//LaneGeometryPartialPage lanes = detectors.clickLaneGeometryTab();
    //		lanes.checkAllColumnHeaderCells();
    //		lanes.checkLaneInfo(laneOne);
    //		//lanes.checkAllLanes(testSim); TODO troubleshoot when time is available, inconsistent failures occuring here
    //
    //		//Verify (i) icon displays with each lane row, click an (i) icon, verify Viewing Nodes window displays, click OK button
    //		Assert.assertTrue("Info icon could not be found for lane with ID of " + laneID + ".", lanes.isLaneInfoIconDisplayed(laneOne));
    //		lanes.clickInfoIcon(laneOne);
    //		Assert.assertTrue("Viewing nodes window could not be found after clicking Info icon for lane with ID of " + laneID + ".", lanes.isViewingNodesWindowDisplayed(laneID));
    //		lanes.checkViewingNodesContent(laneOne);
    //		lanes.clickOKBtn(laneOne);
    //
    //		//Verify Viewing Nodes window is no longer displayed
    //		Assert.assertFalse("Viewing nodes window is still displayed after OK button was clicked.", lanes.isViewingNodesWindowDisplayed(laneID));
    //
    //		//Click Logs tab and verify search criteria and app log table are displayed
    //		LogsPartialPage logs = lanes.clickLogsTab();
    //		logs.checkLogSections();
    //
    //		//Verify Search Criteria section displays all expected buttons
    //		logs.checkAllSearchOptions();
    //		logs.checkAllSearchBtns();
    //
    //		//Verify App Log Table displays expected columns
    //		logs.checkAllAppLogsColumnHeaderCells();
    //
    //		//Click Command History tab and verify injected commands display
    //		CommandHistoryPartialPage commandHistory = logs.clickCommandHistoryTab();
    //		commandHistory.checkSpeedCommandsDisplayed(execution);
    //
    //		//Click Simulations, select simulation and execution, click Finish, verify status updated to completed
    //		commandHistory.clickSimulations();
    //		simPage.selectSimCheckBox(testSim, true);
    //		simPage.selectExecutionCheckBox(newExec, true);
    //
    //		//Remove and re-enable below when BUG 12386 resolved
    //		DeleteWarningForm deleteForm = simPage.clickDeleteExecution();
    //		deleteForm.clickBtn(Btn.YES);
    //		//simPage.clickFinish();
    //		//Assert.assertTrue("Execution with name, " + newExec + ", does not have status of completed as expected.", simPage.isExecutionAndStatusDisplayed(newExec, ExecutionStatus.COMPLETED));
    //
    //		//Log out
    //		simPage.logout(testUser);
    //	}

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Test steps for ITC-018
    //	 */
    //	@Test
    //	public void stepThroughExecutionInternalTest() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");
    //
    //		//Create execution for ITC simulation
    //		//Note: this cannot be done in the warm-up as only one non-completed execution can exist at any given time
    //		testITCSim = SimulationFactory.getTemplateSimulation(testUser, false);
    //		testITCSim.setUser(testUser);
    //		ETexasEntityManager.addEntities(testUser, testITCSim);
    //		ETexasSimulationUtils.createTemplateSimulation(testITCSim);
    //		newITCExecName = ETexasExecutionUtils.createNewExecutionWithKnownRandomSeed(testITCSim, "1234");
    //
    //		//Login and Ensure the Simulations page is loaded
    //		SimulationsPage simPage = goToLandingPage().loginAs(testUser);
    //
    //		//Select a simulation with an in progress execution.
    //		simPage.selectSimCheckBox(testITCSim, true);
    //
    //		//Select the in progress execution, click the Control Execution button, and verify execution details page for selected execution loads.
    //		simPage.selectExecutionCheckBox(newITCExecName, true);
    //		ExecutionsPage execPage = simPage.clickControlExecution();
    //		Assert.assertTrue("Executions header could not be found.", execPage.isExecutionDetailsHeaderDisplayed());
    //
    //		//Delete the text in the Steps selector.
    //		execPage.enterSteps("");
    //
    //		//Click the Next Step button.
    //		execPage.clickNextStepErrorExpected();
    //
    //		//Verify an error message displays indicating the Steps field is required.
    //		execPage.checkStepsFieldRequiredErrorDisplayed();
    //
    //		//Copy non-numerical text and paste it into the Steps text box.
    //		execPage.copyPasteAlphabeticValueInStepsTextBox();
    //
    //		//Verify an error message is displayed indicating the field does not support non-numerical values.
    //		Assert.assertTrue("Invalid, non-numeric, steps value error tooltip could not be found after entering a non-numerical steps value.", execPage.isInvalidNonNumericStepsValueErrorDisplayed());
    //
    //		//Enter a numerical value greater than the total steps of the execution (the original value displays in the Remaining field upon beginning the execution)
    //		String valueGreaterThanStepsRemaining = execPage.getRemainingSteps() + "1";
    //		execPage.enterSteps(valueGreaterThanStepsRemaining);
    //
    //		//Click the Next Step button.
    //		execPage.clickNextStepErrorExpected();
    //
    //		//Verify an error message is displayed indicating the field does not support values greater than the total steps in the execution.
    //		Assert.assertTrue("Steps exceeding maximum error tooltip could not be found after entering a value exceeding the maximum remaining steps.", execPage.isStepsExceedingMaxErrorDisplayed());
    //
    //		//Enter a valid numerical value in Steps text box
    //		execPage.enterSteps("25");
    //		execPage.clickNextStep();
    //		String newRemainingSteps = execPage.getRemainingSteps();
    //		String newSimTime = execPage.getSimTime();
    //		Assert.assertEquals("Displayed remaining steps value does not decrease after advancing 25 steps.", "2375", newRemainingSteps);
    //		Assert.assertEquals("Sim time value does not increase as expected after advancing 25 steps.", "12.5", newSimTime);
    //
    //		//Click Finish
    //		execPage.clickFinish();
    //
    //		//Click Simulations, select simulation used in this test, verify execution from this test is displayed with completed status
    //		execPage.clickSimulations();
    //		simPage.selectSimCheckBox(testITCSim, true);
    //		simPage.selectSimCheckBox(testITCSim, false); //Remove de-selection and re-selection steps following resolution of BUG 12583
    //		simPage.selectSimCheckBox(testITCSim, true);
    //		Assert.assertTrue("Execution with name, " + newITCExecName + ", is not displayed with completed status as expected.",
    //				simPage.isExecutionAndStatusDisplayed(newITCExecName, ExecutionStatus.COMPLETED));
    //
    //		//Log out
    //		simPage.logout(testUser);
    //
    //	}
}
