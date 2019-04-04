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
 * Test class which executes steps for the Filter Execution by ID test, TC-062
 *
 * @author llaroussini
 */
public class FilterExecutionsByIDTest extends ETexasAfterTestResetTestBase {

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
    //     * Test steps for TC-062
    //     */
    //    @Test
    //    public void filterIDTest() {
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
    //        //Click arrow icon
    //        simPage.clickExecutionCellArrowIcon(ColumnName.NAME);
    //
    //        //Verify menu options display (columns and filters)
    //        simPage.checkSortingOptionsWithFilter();
    //
    //        //Hover over Columns and select ID
    //        simPage.hoverOverColumnsOption();
    //        simPage.selectSortingOptionCheckBox(SortingOption.ID, true);
    //
    //        //Verify ID column is displayed
    //        Assert.assertTrue("Column header cell with the text ID could not be found.", simPage.isExecutionCellDisplayed(ColumnName.ID));
    //
    //        //Hover over ID column
    //        simPage.hoverOverExecutionColumn(ColumnName.ID);
    //
    //        //Click arrow icon and verify sorting menu is displayed
    //        simPage.clickExecutionCellArrowIcon(ColumnName.ID);
    //        Assert.assertTrue("Sorting menu is not displayed as expected after arrow icon is clicked.", simPage.isSortingMenuDisplayed());
    //
    //        //Verify menu options display (sorting ascending/descending and columns)
    //        simPage.checkSortingOptionsWithoutFilter();
    //
    //        //Hover over Columns and verify check boxes display (ID, name, status, and date created)
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
    //        simPage.clickExecutionColumn(ColumnName.ID);
    //        Assert.assertTrue("Executions are not displayed in descending order as expected.", exec2Position > exec1Position);
    //
    //        //Log out
    //        simPage.logout(testUser);
    //
    //    }
}
