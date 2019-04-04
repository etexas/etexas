package com.harmonia.qa.ETEXASWebQATests.ExecutionManagement;

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
 * Test class which executes steps for the Filter Execution by Name test, TC-038
 *
 * @author llaroussini
 */
public class FilterExecutionsByNameTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Simulation object used throughout the test case
     */
    private TemplateSimulation sim;

    /**
     * Execution object used in test case
     */
    private Execution exec1;

    /**
     * Execution object used in test case
     */
    private Execution exec2;

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");
        //Get test user and test simulations
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        sim = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        ETexasEntityManager.addEntities(testUser, sim);

        //Register user and create new simulation from template
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulation(sim);

        //Create two executions with completed statuses
        String exec1Name = ETexasExecutionUtils.createAndFinishExecution(sim);
        //Create a new execution
        String exec2Name = ETexasExecutionUtils.createAndFinishExecution(sim);

        //Create execution entities
        List<Execution> execList = new ArrayList<Execution>(3);
        exec1 = new Execution();
        exec1.setSimulation(sim);
        exec1.setName(exec1Name);
        exec1.setStatus(Status.COMPLETED);
        execList.add(exec1);
        exec2 = new Execution();
        exec2.setSimulation(sim);
        exec2.setName(exec2Name);
        exec2.setStatus(Status.NOT_STARTED);
        execList.add(exec2);
        ETexasEntityManager.addEntities(exec1, exec2);

        //Set executions for simulation
        sim.setExecutions(execList);

        //User is logged in
        landing.loginAs(testUser);
    }

    //TODO update when executions fully re-implemented in version 3.0
    //    /**
    //     * Test steps for TC-038
    //     */
    //    @Test
    //    public void filterNameTest() {
    //        //Set screenshot
    //        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");
    //        //Ensure the Simulations page is loaded
    //        SimulationsPage simPage = getPage(SimulationsPage.class);
    //
    //        //Select a simulation with an existing execution that has not yet been started
    //        simPage.selectSim(sim, true);
    //
    //        //Hover over Name column header and verify arrow icon displays
    //        Assert.assertTrue("Column header cell with the text Name could not be found.", simPage.isExecutionCellDisplayed(ColumnName.NAME));
    //        simPage.hoverOverExecutionColumn(ColumnName.NAME);
    //
    //        //Click arrow icon and verify sorting options display (ascending and descending)
    //        simPage.clickExecutionCellArrowIcon(ColumnName.NAME);
    //        Assert.assertTrue("Sorting menu is not displayed as expected after arrow icon is clicked.", simPage.isSortingMenuDisplayed());
    //
    //        //Verify menu options display (columns and filters)
    //        simPage.checkSortingOptionsWithFilter();
    //
    //        //Hover over Columns and verify check boxes display (ID, name, status, and date created)
    //        simPage.hoverOverColumnsOption();
    //        simPage.checkColumnOptions();
    //
    //        //Hover over Filters option and verify text box displays
    //        simPage.hoverOverFiltersOption();
    //        Assert.assertTrue("Execution search box is not displayed when Filters option is hovered over.", simPage.isSortingNameTextBoxDisplayed());
    //
    //        //Enter completed execution name in the text box and verify results are filtered
    //        String exec1Name = exec1.getName();
    //        String exec2Name = exec2.getName();
    //        simPage.searchForExecution(exec1);
    //        Assert.assertTrue("First execution is not displayed when first execution name, " + exec1Name + ", is entered in search box.", simPage.isExecutionDisplayed(exec1Name));
    //        Assert.assertFalse("Second execution (named " + exec2Name + ") is still displayed when First execution name, " + exec1Name + ", is entered in search box.",
    //                simPage.isExecutionDisplayed(exec2Name));
    //
    //        //Enter not started execution name in the text box and verify results are filtered
    //        simPage.searchForExecution(exec2);
    //        Assert.assertTrue("Second execution is not displayed when Second execution name, " + exec2Name + ", is enterd in search box.", simPage.isExecutionDisplayed(exec2Name));
    //        Assert.assertFalse("First execution (named " + exec1Name + ") is still displayed when Second execution name, " + exec2Name + ", is entered in search box.",
    //                simPage.isExecutionDisplayed(exec1Name));
    //
    //        //Log out
    //        simPage.logout(testUser);
    //
    //    }
}