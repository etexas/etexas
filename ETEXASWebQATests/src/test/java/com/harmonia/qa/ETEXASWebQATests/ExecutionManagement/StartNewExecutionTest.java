package com.harmonia.qa.ETEXASWebQATests.ExecutionManagement;

import org.junit.Before;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;

/**
 * Test class which executes steps for the Start a New Execution test, TC-015
 * and ITC-013
 *
 * @author llaroussini
 */
public class StartNewExecutionTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Simulation object used throughout the test case
     */
    private TemplateSimulation testSim;

    /**
     * Simulation object with in-progress execution used in the internal test
     * case
     */
    private TemplateSimulation simWithExec;

    /**
     * Name of the in-progress execution created (to be used in ITC)
     */
    private String inProgessExecName;

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
        simWithExec = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        simWithExec.setUser(testUser);
        ETexasEntityManager.addEntities(testUser, testSim, simWithExec);

        //Register user
        ETexasUserUtils.userRegistration(testUser);
    }

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Test steps for TC-015
    //	 */
    //	@Test
    //	public void newExecutionExternalTest() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");
    //		//Create new simulation from template
    //		ETexasSimulationUtils.createTemplateSimulation(testSim);
    //		//Login and ensure the Simulations page is loaded
    //		SimulationsPage simPage = ETexasCommonUtils.goToLandingPage().loginAs(testUser);
    //
    //		//Select existing simulation
    //		simPage.selectSimCheckBox(testSim, true);
    //
    //		//Verify simulation management buttons enabled
    //		simPage.checkEnabledSimulationBtns();
    //
    //		//Click Execute
    //		simPage.clickExecuteAndVerifyExecutions();
    //
    //		//Verify new execution is listed with status of 'Not Started'
    //		simPage.checkNewExecutionDisplayed();
    //
    //		//Select newest execution, click Finish and verify execution status updates
    //		simPage.selectNewExecution(true);
    //		simPage.clickFinish();
    //		//Automation Only: update the page object to get most recent DOM
    //		simPage.waitUntilLoaded();
    //		simPage.checkNewExecutionCompleted();
    //
    //		//Select existing simulation
    //		simPage.selectSimCheckBox(testSim, true);
    //
    //		//Hover over Execute button and select Random Seed option and verify copy form window displays
    //		simPage.hoverExecuteBtn();
    //		RandomSeedForm rndmSeed = simPage.clickEnterRandomSeed();
    //		Assert.assertTrue("The Random Seed header could not be found.", rndmSeed.isRandomSeedHeaderDisplayed());
    //
    //		//Verify help and close icons display
    //		rndmSeed.checkRandomSeedHeaderIcons();
    //
    //		//Verify form fields display
    //		rndmSeed.checkFields();
    //
    //		//Verify form buttons display
    //		rndmSeed.checkBtns();
    //
    //		//Click help icon
    //		rndmSeed.clickRandomSeedHelp();
    //
    //		//Verify help window displays with expected header and content
    //		Assert.assertTrue("The Random Seed Help header could not be found.", rndmSeed.isRandomSeedHelpHeaderDisplayed());
    //
    //		//Verify OK button displays
    //		Assert.assertTrue("OK button could not be found.", rndmSeed.isHelpOKBtnDisplayed());
    //
    //		//Click OK
    //		rndmSeed.clickHelpOKBtn();
    //
    //		//Enter random seed value, click reset, verify fields are cleared
    //		String newSeed = RandomStringGenerator.nextNumStringOfLength(5);
    //		rndmSeed.setRandomSeed(newSeed);
    //		rndmSeed.checkRandomSeedField(newSeed);
    //		ETexasCommonUtils.sleep(500);
    //		rndmSeed.clickReset();
    //		rndmSeed.checkRandomSeedField("");
    //
    //		//Enter random seed value, click cancel, verify new execution is not added to executions list
    //		rndmSeed.setRandomSeed(newSeed);
    //		rndmSeed.clickCancel();
    //		int currentExecutions = simPage.getNumOfExecutions();
    //		Assert.assertFalse("A new execution was added despite cancelling the random seed.", simPage.isNewExecutionDisplayed(currentExecutions));
    //
    //		//Select previously created simulation, hover over Execute, and select the random seed option
    //		simPage.selectSimCheckBox(testSim, true);
    //		simPage.hoverExecuteBtn();
    //		simPage.clickEnterRandomSeed();
    //
    //		//Enter a random seed and click Create
    //		rndmSeed.setRandomSeed(newSeed);
    //		rndmSeed.clickCreateAndWait();
    //
    //		//Verify new execution displays in execution pane with status of execution is 'not started'
    //		simPage.checkNewExecutionDisplayed();
    //
    //		//Finish execution
    //		simPage.selectNewExecution(true);
    //		simPage.clickFinish();
    //		simPage.waitUntilLoaded();
    //		simPage.checkNewExecutionCompleted();
    //
    //		//Log out
    //		simPage.logout(testUser);
    //
    //	}

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Test steps for ITC-013
    //	 */
    //	@Test
    //	public void newExecutionInternalTest() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");
    //
    //		//Create simulation with an in-progress execution
    //		ETexasSimulationUtils.createTemplateSimulation(simWithExec);
    //		inProgessExecName = ETexasExecutionUtils.createInProgressExecution(simWithExec);
    //
    //		//Login and ensure the Simulations page is loaded
    //		SimulationsPage simPage = ETexasCommonUtils.goToLandingPage().loginAs(testUser);
    //
    //		//Select a simulation with an in-progress execution.
    //		simPage.selectSimCheckBox(simWithExec, true);
    //
    //		//Click the Execute button.
    //		simPage.clickSimBtn(SimBtns.EXECUTE_BTN);
    //
    //		//Verify an error message window is displayed indicating an execution is already running.
    //		simPage.checkExecutionFailedError();
    //
    //		//Verify an OK button is displayed in the error message window
    //		Assert.assertTrue("OK button not displayed in Execution Failed error window as expected.", simPage.isExecutionFailedErrorOKBtnDisplayed());
    //
    //		//Click the OK button.
    //		simPage.clickExecutionFailedErrorOKBtn();
    //
    //		//Select the in-progress execution in the Executions pane.
    //		simPage.selectExecutionCheckBox(inProgessExecName, true);
    //
    //		//Click the Finish button.
    //		simPage.clickFinish();
    //
    //		//Verify the execution updates to a status of Completed.
    //		Assert.assertTrue("Execution with name, " + inProgessExecName + ", does not display with completed status.",
    //				simPage.isExecutionNameAndStatusDisplayed(inProgessExecName, ExecutionStatus.COMPLETED));
    //
    //		//Click the Execute button and verify a new execution is created
    //		simPage.clickExecuteAndVerifyExecutions();
    //		String newExecName = simPage.getNewExecutionName();
    //
    //		//Click Finish
    //		simPage.clickFinish();
    //
    //		//Verify the execution updates to a status of Completed.
    //		Assert.assertTrue("Execution with name, " + newExecName + ", does not display with completed status.", simPage.isExecutionNameAndStatusDisplayed(newExecName, ExecutionStatus.COMPLETED));
    //
    //		//Log out
    //		simPage.logout(testUser);
    //
    //	}
}
