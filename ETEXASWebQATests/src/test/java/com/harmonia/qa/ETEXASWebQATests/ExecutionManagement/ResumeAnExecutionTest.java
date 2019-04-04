package com.harmonia.qa.ETEXASWebQATests.ExecutionManagement;

import org.junit.Before;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasExecutionUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;

/**
 * Test class which executes steps for the Resume and Existing Execution test,
 * TC-017
 *
 * @author llaroussini
 */
public class ResumeAnExecutionTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Simulation object used throughout the test case
     */
    private TemplateSimulation testSim;

    /**
     * Name of execution used throughout the test case
     */
    private String execName;

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");
        //Get test user and test simulation
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        testSim = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        testSim.setUser(testUser);
        ETexasEntityManager.addEntities(testUser, testSim);

        //Register user and create new simulation from template
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulation(testSim);
        execName = ETexasExecutionUtils.createNewExecution(testSim);

        //User is logged in
        landing.loginAs(testUser);
    }

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Test steps for TC-017
    //	 */
    //	@Test
    //	public void resumeExecutionTest() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");
    //		//Ensure the Simulations page is loaded
    //		SimulationsPage simPage = getPage(SimulationsPage.class);
    //
    //		//Select a simulation with an existing execution that has not yet been started
    //		simPage.selectSimCheckBox(testSim, true);
    //
    //		//Select the execution
    //		simPage.selectExecutionCheckBox(execName, true);
    //
    //		//Click the Control Execution button
    //		ExecutionsPage excPage = simPage.clickControlExecution();
    //
    //		//Verify the Executions page displays
    //		Assert.assertTrue("Execution Details page header is not displayed as expected.", excPage.isExecutionDetailsHeaderDisplayed());
    //
    //		//Verify sim time value is 0.0 and remaining steps value is 2400
    //		String startingRemainingSteps = excPage.getRemainingSteps();
    //		String startingSimTime = excPage.getSimTime();
    //		Assert.assertEquals("Starting remaining steps value not displayed as expected.", "2400", startingRemainingSteps);
    //		Assert.assertEquals("Starting sim time value not displayed as expected.", "0.0", startingSimTime);
    //		VehiclesPartialPage vehicles = excPage.clickVehiclesTab();
    //		Assert.assertFalse("Vehicle rows are displayed prior to starting the execution.", vehicles.areVehicleRowsDisplayed());
    //
    //		//Advance by 50 steps
    //		excPage.enterSteps("50");
    //		excPage.clickNextStep();
    //		excPage.clickVehiclesTab();
    //		Assert.assertTrue("Vehicle rows do not display after 50 steps into execution.", vehicles.areVehicleRowsDisplayed());
    //		String newRemainingSteps = excPage.getRemainingSteps();
    //		String newSimTime = excPage.getSimTime();
    //
    //		//Click the Simulations link at the top of the page
    //		excPage.clickSimulations();
    //
    //		//Verify the Simulation page displays
    //		simPage.checkSimulationsHeaderText();
    //
    //		//Select the same simulation
    //		simPage.selectSimCheckBox(testSim, true);
    //
    //		//Verify the status of the execution updates to 'In Progress'
    //		simPage.isExecutionAndStatusDisplayed(execName, ExecutionStatus.IN_PROGRESS);
    //
    //		//Select the execution
    //		simPage.selectExecutionCheckBox(execName, true);
    //
    //		//Click the Control Execution button
    //		simPage.clickControlExecution();
    //
    //		//Verify the Executions page displays
    //		Assert.assertTrue("Execution Details page header is not displayed as expected.", excPage.isExecutionDetailsHeaderDisplayed());
    //
    //		//Verify the Execution displays at the same step displayed prior to navigating away from the page
    //		String displayedRemainingSteps = excPage.getRemainingSteps();
    //		String displayedSimTime = excPage.getSimTime();
    //		Assert.assertEquals("Displayed remaining steps value does not match remaining steps displayed previously.", newRemainingSteps, displayedRemainingSteps);
    //		Assert.assertEquals("Starting sim time value does not match sim time displayed previously.", newSimTime, displayedSimTime);
    //		excPage.clickVehiclesTab();
    //		Assert.assertTrue("Vehicle rows do not display after returning to the execution.", vehicles.areVehicleRowsDisplayed());
    //
    //		//Click the Simulations link at the top of the page
    //		excPage.clickSimulations();
    //
    //		//Verify the Simulation page displays
    //		simPage.checkSimulationsHeaderText();
    //
    //		//Select the same simulation
    //		simPage.selectSimCheckBox(testSim, true);
    //
    //		//Verify the status of the execution updates to 'In Progress'
    //		simPage.isExecutionAndStatusDisplayed(execName, ExecutionStatus.IN_PROGRESS);
    //
    //		//Select the execution
    //		simPage.selectNewestExecutionCheckBox(true);
    //
    //		//Click the Finish button
    //		simPage.clickFinish();
    //
    //		//Verify the execution status updates to Completed.
    //		simPage.isExecutionAndStatusDisplayed(execName, ExecutionStatus.COMPLETED);
    //
    //		//Log out
    //		simPage.logout(testUser);
    //	}
}
