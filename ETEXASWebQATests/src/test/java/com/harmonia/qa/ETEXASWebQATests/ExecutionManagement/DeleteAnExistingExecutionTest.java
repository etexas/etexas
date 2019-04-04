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
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal.Btn;

/**
 * Test class which executes steps for the Delete an Existing Execution test,
 * TC-016
 *
 * @author llaroussini
 */
public class DeleteAnExistingExecutionTest extends ETexasAfterTestResetTestBase {

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

        //Register user, create new simulation from template, and create a new execution for the simulation
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulation(testSim);
        ETexasExecutionUtils.createNewExecution(testSim);

        //User is logged in
        landing.loginAs(testUser);
    }

    //TODO update when executions fully re-implemented in version 3.0
    //    /**
    //     * Test steps for TC-016
    //     */
    //    @Test
    //    public void deleteExecutionTest() {
    //        //Set screenshot
    //        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");
    //        //Ensure the Simulations page is loaded
    //        SimulationsPage simPage = getPage(SimulationsPage.class);
    //
    //        //Select simulation and verify newest execution displays with 'Not Started' status
    //        simPage.selectSim(testSim, true);
    //        simPage.checkNewExecutionDisplayed();
    //
    //        //Get total executions
    //        int totalExecutions = simPage.getNumOfExecutions();
    //
    //        //Select the execution
    //        simPage.selectNewExecution(true);
    //
    //        //Verify execution buttons are enabled
    //        simPage.checkEnabledActiveExecutionBtns();
    //
    //        //Click Delete and verify confirm delete modal displays
    //        ConfirmDeleteModal deleteWarning = simPage.clickDeleteExecution();
    //        deleteWarning.checkConfirmDeleteHeader();
    //        deleteWarning.checkDeleteWarningExecutionContent();
    //        deleteWarning.checkConfirmDeleteBtns();
    //
    //        //Click cancel, verify confirmation window closes and execution is still displayed with Not Started status
    //        deleteWarning.clickBtn(Btn.NO);
    //        Assert.assertFalse("Delete Warning window is still displayed after cancelling deletion.", deleteWarning.isConfirmDeleteHeaderDisplayed());
    //        simPage.checkNewExecutionDisplayed();
    //
    //        //Select the execution
    //        simPage.selectNewExecution(true);
    //
    //        //Click Delete and confirm
    //        simPage.clickDeleteExecution();
    //        deleteWarning.clickBtn(Btn.YES);
    //
    //        //Verify confirmation window closes and execution is no longer displayed
    //        Assert.assertFalse("Delete Warning window still displayed after clicking 'Yes'.", deleteWarning.isExecutionDeletionContentDisplayed());
    //        int reducedExecutions = simPage.getNumOfExecutions();
    //        Assert.assertEquals("Exeuction is still displayed after deletion.", reducedExecutions, totalExecutions - 1);
    //
    //        //Log out
    //        simPage.logout(testUser);
    //
    //    }
}