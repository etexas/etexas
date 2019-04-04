package com.harmonia.qa.ETEXASWebQATests.ExecutionManagement;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.Execution;
import com.harmonia.qa.ETEXASWebQATests.entities.Execution.Status;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasExecutionUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;

/**
 * Test class which executes steps for the Run an Execution to Complete test,
 * TC-021
 *
 * @author llaroussini
 */
public class RunExecutionToCompleteTest extends ETexasAfterTestResetTestBase {

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
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get test user and test simulation
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        testSim = ETexasEntityManager.getTemplateSimulation(defaultEx05Sim); //get default EX_05 simulation
        testSim.setUser(testUser);
        ETexasEntityManager.addEntities(testUser, testSim);

        //Register user and create new simulation from template
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulation(testSim);

        //Create an execution with an in-progress status
        String execName = ETexasExecutionUtils.createNewExecution(testSim);

        //Create execution entities
        List<Execution> execList = new ArrayList<Execution>(3);
        execution = new Execution();
        execution.setSimulation(testSim);
        execution.setName(execName);
        execution.setStatus(Status.NOT_STARTED);
        execList.add(execution);

        //Set execution with associated simulation
        testSim.setExecutions(execList);
        ETexasEntityManager.addEntities(execution);

        //User is logged in
        landing.loginAs(testUser);
    }

    //TODO - needs to be updated based on UI changes

    //	/**
    //	 * Test steps for TC-021
    //	 */
    //	@Test
    //	public void runExecutionToComplete() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");
    //
    //		//Ensure the Simulations page is loaded
    //		SimulationsPage simPage = getPage(SimulationsPage.class);
    //
    //		//Select execution, click control execution, and verify Executions Details page displays
    //		simPage.selectSimCheckBox(testSim, true);
    //		simPage.selectNewestExecutionCheckBox(true);
    //		ExecutionsPage execPage = simPage.clickControlExecution();
    //		Assert.assertTrue("Executions header could not be found.", execPage.isExecutionDetailsHeaderDisplayed());
    //
    //		//Click Finish
    //		CompletedExecutionPage completedExec = execPage.clickFinish();
    //
    //		//Verify Execution Details page updated to display Completed Execution Details
    //		//Verifies tabs displayed during in progress execution are no longer present, and completed execution tabs are preset
    //		Assert.assertFalse("Vehicles tab is still displayed after finishing execution.", completedExec.isTabDisplayed(ExecutionDetailsTabs.VEHICLES_TAB));
    //		Assert.assertFalse("Signals tab is still displayed after finishing execution.", completedExec.isTabDisplayed(ExecutionDetailsTabs.SIGNALS_TAB));
    //		Assert.assertFalse("Detectors tab is still displayed after finishing execution.", completedExec.isTabDisplayed(ExecutionDetailsTabs.DETECTORS_TAB));
    //		completedExec.checkAllCompletedExecTabsDisplayed();
    //
    //		//Log out
    //		completedExec.logout(testUser);
    //
    //	}

}
