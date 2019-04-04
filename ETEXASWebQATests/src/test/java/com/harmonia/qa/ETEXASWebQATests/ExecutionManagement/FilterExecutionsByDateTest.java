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
 * Test class which executes steps for the Filter Execution by Date Created
 * test, TC-063
 *
 * @author llaroussini
 */
public class FilterExecutionsByDateTest extends ETexasAfterTestResetTestBase {

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
    //     * Test steps for TC-063
    //     */
    //    @Test
    //    public void filterByDateTest() {
    //        //Set screenshot
    //        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");
    //        //Ensure the Simulations page is loaded
    //        SimulationsPage simPage = getPage(SimulationsPage.class);
    //
    //        //Select a simulation with multiple executions
    //        simPage.selectSim(sim, true);
    //
    //        //Hover over the Date Created column header in the Executions pane and verify an arrow is displayed on the right side.
    //        Assert.assertTrue("Column header cell with the text Date Created could not be found.", simPage.isExecutionCellDisplayed(ColumnName.DATE_CREATED));
    //        simPage.hoverOverExecutionColumn(ColumnName.DATE_CREATED);
    //
    //        //Click the arrow and verify a menu is displayed with the following sort options: Sort Ascending and Sort Descending AND an option for Columns
    //        simPage.clickExecutionCellArrowIcon(ColumnName.DATE_CREATED);
    //        simPage.checkSortingOptionsWithoutFilter();
    //
    //        //Verify Soft Ascending and Sort Descending options are displayed
    //        Assert.assertTrue("Sort Ascending options could not be found.", simPage.isSortingItemCheckBoxDisplayed(SortingOption.SORT_ASCENDING));
    //        Assert.assertTrue("Sort Descending options could not be found.", simPage.isSortingItemCheckBoxDisplayed(SortingOption.SORT_DESCENDING));
    //
    //        //Click the Columns option and verify the following checkboxes display: ID, Name, Status, and Date Created.
    //        simPage.hoverOverColumnsOption();
    //        simPage.checkColumnOptions();
    //
    //        //Hover over Columns option, de-select an option and verify column no longer displayed
    //        simPage.hoverOverColumnsOption();
    //        simPage.selectSortingOptionCheckBox(SortingOption.NAME, false);
    //        Assert.assertFalse("Column header cell with the text Name is still displayed after being de-selected.", simPage.isExecutionCellDisplayed(ColumnName.NAME));
    //
    //        //Re-select option and verify column is again displayed
    //        simPage.selectSortingOptionCheckBox(SortingOption.NAME, true);
    //        Assert.assertTrue("Column header cell with the text Name could not be found.", simPage.isExecutionCellDisplayed(ColumnName.NAME));
    //
    //        //Click sort ascending option
    //        simPage.clickSortAscending();
    //
    //        //Verify Executions are displayed in ascending order
    //        int exec1Position = simPage.getExecutionPosition(exec1.getName());
    //        int exec2Position = simPage.getExecutionPosition(exec2.getName());
    //        Assert.assertTrue("Executions are not displayed in ascending order as expected.", exec1Position < exec2Position);
    //
    //        //Click column header and verify Executions are displayed in descending order
    //        simPage.clickExecutionColumn(ColumnName.DATE_CREATED);
    //        Assert.assertTrue("Executions are not displayed in descending order as expected.", exec2Position > exec1Position);
    //
    //        //Log out
    //        simPage.logout(testUser);
    //    }
}
