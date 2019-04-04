package com.harmonia.qa.ETEXASWebQATests.ExecutionManagement;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;

/**
 * Test class which executes steps for the Repeat An Execution test,
 * TC-057/ITC-038
 *
 * @author llaroussini
 */
public class RepeatAnExecutionTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Simulation object used throughout the test case
     */
    private TemplateSimulation testSim;

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
    //		testSim = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
    //		ETexasEntityManager.addEntities(testUser, testSim);
    //
    //		//Register user and create new simulation from template
    //		LandingPage landing = ETexasUserUtils.userRegistration(testUser);
    //		ETexasSimulationUtils.createTemplateSimulation(testSim);
    //
    //		//User is logged in
    //		landing.loginAs(testUser);
    //	}
    //
    //	/**
    //	 * Test steps for TC-057
    //	 */
    //	@Test
    //	public void repeatExecutionExternalTest() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");
    //		//Ensure the Simulations page is loaded
    //		SimulationsPage simPage = getPage(SimulationsPage.class);
    //		simPage.waitUntilLoaded();
    //
    //		//Select Simulation
    //		simPage.selectSim(testSim, true);
    //
    //		//Hover over Execute button and verify Random Seed option displays
    //		simPage.hoverExecuteBtn(); //verification occurs in method
    //
    //		//Click Random Seed option and verify Random Seed form displays
    //		RandomSeedForm randomSeedForm = simPage.clickEnterRandomSeed();
    //		Assert.assertTrue("Random Seed Form window not displayed after clicking Enter Random Seed option.", randomSeedForm.isRandomSeedHeaderDisplayed());
    //
    //		//Enter valid random seed value and click create
    //		String rndmSeed = Integer.toString(RandomNumberGenerator.nextInteger(ETexasExecutionUtils.MAX_RANDOM_SEED_VALUE));
    //		randomSeedForm.setRandomSeed(rndmSeed);
    //		randomSeedForm.clickCreateAndWait();
    //		String execName = simPage.getNewExecutionName();
    //
    //		//Verify new execution is displayed with not started status
    //		Assert.assertTrue("Execution with name, " + execName + " and not started status could not be found.", simPage.isExecutionAndStatusDisplayed(execName, ExecutionStatus.NOT_STARTED));
    //
    //		//Click Finish and verify execution completes and displays with completed status
    //		simPage.clickFinish();
    //		Assert.assertTrue("Execution with name, " + execName + " and completed status could not be found.", simPage.isExecutionAndStatusDisplayed(execName, ExecutionStatus.COMPLETED));
    //
    //		//Hover over Execute button and verify Random Seed option displays
    //		simPage.hoverExecuteBtn(); //verification occurs in method
    //
    //		//Click Random Seed option and verify Random Seed form displays
    //		simPage.clickEnterRandomSeed();
    //		Assert.assertTrue("Random Seed Form window not displayed after clicking Enter Random Seed option.", randomSeedForm.isRandomSeedHeaderDisplayed());
    //
    //		//Enter the same valid random seed value and click create
    //		randomSeedForm.setRandomSeed(rndmSeed);
    //		randomSeedForm.clickCreateAndWait();
    //		String newExecName = simPage.getNewExecutionName();
    //
    //		//Verify a new execution is displayed with not started status
    //		Assert.assertTrue("Execution with name, " + newExecName + " and not started status could not be found.", simPage.isExecutionAndStatusDisplayed(newExecName, ExecutionStatus.NOT_STARTED));
    //
    //		//Click Finish and verify the new execution completes and displays with completed status
    //		simPage.clickFinish();
    //		Assert.assertTrue("Execution with name, " + newExecName + " and completed status could not be found.", simPage.isExecutionAndStatusDisplayed(newExecName, ExecutionStatus.COMPLETED));
    //
    //		//Logout
    //		simPage.logout(testUser);
    //	}
    //
    //	/**
    //	 * Test steps for ITC-038
    //	 */
    //	@Test
    //	public void repeatExecutionInternalTest() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");
    //		//Ensure the Simulations page is loaded
    //		SimulationsPage simPage = getPage(SimulationsPage.class);
    //		simPage.waitUntilLoaded();
    //
    //		//Select Simulation
    //		simPage.selectSim(testSim, true);
    //
    //		//Hover over the Execute button and click the Enter Random Seed link.
    //		simPage.hoverExecuteBtn();
    //		RandomSeedForm randomSeedForm = simPage.clickEnterRandomSeed();
    //
    //		//With the Random Seed text box blank, click the Create button.
    //		randomSeedForm.clickCreateAndWait();
    //
    //		//Verify an error is displayed associated with the Random Seed text box indicating the field is required.
    //		randomSeedForm.checkRandomSeedFieldRequiredErrorDisplayed();
    //
    //		//Enter a non-numerical value in the Random Seed text box.
    //		randomSeedForm.setRandomSeed("---");
    //		//Verify an error is displayed associated with the Random Seed text box indicating the random seed must be a positive, numerical value
    //		Assert.assertTrue("Invalid Number error is not displayed as expected when a non-numerical value is used in the Random Seed text box.", randomSeedForm.isInvalidNumberRandomSeedErrorDisplayed());
    //
    //		//Disabled due to BUG 13185
    //		//		//Enter a negative numerical value in the Random Seed text box.
    //		//		randomSeedForm.setRandomSeed("-" + RandomStringGenerator.nextLetterString(5));
    //		//		//Verify an error is displayed associated with the Random Seed text box indicating the random seed must be a positive, numerical value
    //		//		Assert.assertTrue("Invalid Random Seed error is not displayed as expected when a negative value is used in the Random Seed text box.", randomSeedForm.isInvalidRandomSeedErrorDisplayed());
    //
    //		//Disabled due to BUG 13185
    //		//		//Enter a negative numerical value in the Random Seed text box.
    //		//		randomSeedForm.setRandomSeed("-" + RandomStringGenerator.nextLetterString(5));
    //		//		//Verify an error is displayed associated with the Random Seed text box indicating the random seed must be a positive, numerical value
    //		//		Assert.assertTrue("Invalid Random Seed error is not displayed as expected when a decimal value is used in the Random Seed text box.", randomSeedForm.isInvalidRandomSeedErrorDisplayed());
    //
    //		//Enter a numerical value greater than 2147483647 in the Random Seed text box.
    //		randomSeedForm.setRandomSeed("2147483648");
    //		//Verify an error is displayed associated with the Random Seed text box indicating the maximum value for the field is 2147483647.
    //		Assert.assertTrue("Maximum Random Seed error is not displayed as expected when the value entered exceeds the maximum of 2147483647 in the Random Seed text box.",
    //		        randomSeedForm.isMaximumRandomSeedErrorDisplayed());
    //
    //		//Enter a valid numerical value in the Random Seed text box and click the Create button.
    //		randomSeedForm.setRandomSeed(RandomStringGenerator.nextNumStringOfLength(9));
    //		randomSeedForm.clickCreateAndWait();
    //
    //		//Verify a new execution is created and displayed in the Executions pane
    //		String newExecName = simPage.getNewExecutionName();
    //		Assert.assertTrue("Execution with name, " + newExecName + " and not started status could not be found.", simPage.isExecutionAndStatusDisplayed(newExecName, ExecutionStatus.NOT_STARTED));
    //
    //		//Logout
    //		simPage.logout(testUser);
    //	}
}
