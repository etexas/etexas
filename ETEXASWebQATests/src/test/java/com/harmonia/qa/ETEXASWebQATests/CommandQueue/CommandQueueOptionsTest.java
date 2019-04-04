package com.harmonia.qa.ETEXASWebQATests.CommandQueue;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.Execution;
import com.harmonia.qa.ETEXASWebQATests.entities.Execution.Status;
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
 * Test class which executes steps for the Command Queue Options test, TC-048
 *
 * @author llaroussini
 */
public class CommandQueueOptionsTest extends ETexasAfterTestResetTestBase {

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
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");
        //Get test user and test simulation
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        testSim = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        ETexasEntityManager.addEntities(testUser, testSim);

        //Register user and create new simulation from template
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulation(testSim);

        //Create an execution
        String execName = ETexasExecutionUtils.createNewExecution(testSim);

        //Create execution entities
        List<Execution> execList = new ArrayList<Execution>(3);
        Execution execution = new Execution();
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
    //	 * Test steps for TC-048
    //	 */
    //	@Test
    //	public void commandQueueOptionsTest() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");
    //		//Ensure the Simulations page is loaded
    //		SimulationsPage simPage = getPage(SimulationsPage.class);
    //		simPage.waitUntilLoaded();
    //
    //		//Select the simulation and click Execute
    //		simPage.selectSimCheckBox(testSim, true);
    //
    //		//Select the Execution and click Control Execution, verify Execution Details page displays
    //		simPage.selectNewestExecutionCheckBox(true);
    //		ExecutionsPage excPage = simPage.clickControlExecution();
    //		Assert.assertTrue("Execution Details page header is not displayed as expected.", excPage.isExecutionDetailsHeaderDisplayed());
    //
    //		//Verify Command Queue section displays
    //		Assert.assertTrue("The Command Queue section could not be found.", excPage.isSectionDisplayed(SectionHeaders.COMMAND_QUEUE));
    //		Assert.assertTrue("The Command Queue header could not be found.", excPage.isSectionHeaderDisplayed(SectionHeaders.COMMAND_QUEUE));
    //
    //		//Verify minimize and help icons display
    //		Assert.assertTrue("The Minimize icon could not be found in the Command Queue header.", excPage.isMinimizeIconDisplayed(SectionHeaders.COMMAND_QUEUE));
    //		Assert.assertTrue("The Help icon could not be found in the Command Queue header.", excPage.isSectionHelpIconDisplayed(SectionHeaders.COMMAND_QUEUE));
    //
    //		//Click minimize, then verify section collapses and maximize icon displays
    //		excPage.clickCommandQueueMinimize(); //verification occurs within method
    //
    //		//Click maximize, then verify section expands and minimize icon displays
    //		excPage.clickCommandQueueMaximize(); //verification occurs within method
    //
    //		//Click the help icon
    //		CommandQueueOptionsHelpWindow help = excPage.clickCommandQueueHelp();
    //
    //		//Verify help window displays with expected header and content
    //		Assert.assertTrue("The Command Queue Help header is not displayed.", help.isCommandQueueHelpHeaderDisplayed());
    //		Assert.assertTrue("The Command Queue Help content is not displayed.", help.isHelpContentDisplayed());
    //
    //		//Verify OK button displays and click OK
    //		Assert.assertTrue("OK button is not displayed.", help.isHelpOKBtnDisplayed());
    //		help.clickHelpOKBtn();
    //
    //		//Verify window no longer displays
    //		Assert.assertFalse("The Command Queue Help header is still displayed after clicking OK.", help.isCommandQueueHelpHeaderDisplayed());
    //
    //		//Verify Command buttons display in Command Queue section (Commands and Inject Vehicle)
    //		excPage.checkCommandQueueBtns();
    //
    //		//Click Commands button and verify options display (Speed Change, Lane Change, and Signal Change)
    //		excPage.clickCommandsBtn();
    //		excPage.checkAllCommandOptions();
    //
    //		//Log out
    //		excPage.logout(testUser);
    //	}
}
