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
 * Test class which executes steps for the Filter Execution by Status test,
 * TC-018
 *
 * @author llaroussini
 */
public class FilterExecutionsByStatusTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Simulation object used throughout the test case
     */
    private TemplateSimulation sim;

    /**
     * Execution object with status of not started used in test case
     */
    private Execution notStartedExec;

    /**
     * Execution object with status of completed used in test case
     */
    private Execution completedExec;

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

        //Create an execution with a completed status
        String completedName = ETexasExecutionUtils.createAndFinishExecution(sim);
        //Create a new execution
        String notStartedName = ETexasExecutionUtils.createNewExecution(sim);

        //Create execution entities
        List<Execution> execList = new ArrayList<Execution>(3);
        completedExec = new Execution();
        completedExec.setSimulation(sim);
        completedExec.setName(completedName);
        completedExec.setStatus(Status.COMPLETED);
        execList.add(completedExec);
        notStartedExec = new Execution();
        notStartedExec.setSimulation(sim);
        notStartedExec.setName(notStartedName);
        notStartedExec.setStatus(Status.NOT_STARTED);
        execList.add(notStartedExec);

        //Set executions for simulation
        sim.setExecutions(execList);
        ETexasEntityManager.addEntities(notStartedExec, completedExec);

        //User is logged in
        landing.loginAs(testUser);
    }

    //TODO update when executions fully re-implemented in version 3.0
    //    /**
    //     * Test steps for TC-018
    //     */
    //    @Test
    //    public void filterStatusTest() {
    //        //Set screenshot
    //        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");
    //        //Ensure the Simulations page is loaded
    //        SimulationsPage simPage = getPage(SimulationsPage.class);
    //
    //        //Select a simulation with an existing execution that has not yet been started
    //        simPage.selectSim(sim, true);
    //
    //        //Hover over Status column header and verify arrow icon displays
    //        Assert.assertTrue("Column header cell with the text Status could not be found.", simPage.isExecutionCellDisplayed(ColumnName.STATUS));
    //        simPage.hoverOverExecutionColumn(ColumnName.STATUS);
    //
    //        //Click arrow icon and verify sorting options display (ascending and descending)
    //        simPage.clickExecutionCellArrowIcon(ColumnName.STATUS);
    //        Assert.assertTrue("Sorting menu is not displayed as expected after arrow icon is clicked.", simPage.isSortingMenuDisplayed());
    //
    //        //Verify menu options display (columns and filters)
    //        simPage.checkSortingOptionsWithFilter();
    //
    //        //Hover over Columns and verify check boxes display (ID, name, status, and date created)
    //        simPage.hoverOverColumnsOption();
    //        simPage.checkColumnOptions();
    //
    //        //Select ID check box and verify ID column displays
    //        simPage.selectSortingOptionCheckBox(SortingOption.ID, true);
    //        Assert.assertTrue("ID header cell is not displayed after column option is selected.", simPage.isExecutionCellDisplayed(ColumnName.ID));
    //
    //        //De-select ID check box and verify ID column disappears
    //        simPage.selectSortingOptionCheckBox(SortingOption.ID, false);
    //        Assert.assertFalse("ID header cell is still displayed after column option is de-selected.", simPage.isExecutionCellDisplayed(ColumnName.ID));
    //
    //        //De-select Name check box and verify Name column disappears
    //        simPage.selectSortingOptionCheckBox(SortingOption.NAME, false);
    //        Assert.assertFalse("Name header cell is still displayed after column option is de-selected.", simPage.isExecutionCellDisplayed(ColumnName.NAME));
    //
    //        //Select Name check box and verify Name column displays
    //        simPage.selectSortingOptionCheckBox(SortingOption.NAME, true);
    //        Assert.assertTrue("Name header cell is not displayed after column option is selected.", simPage.isExecutionCellDisplayed(ColumnName.NAME));
    //
    //        //De-select Status check box and verify Status column disappears
    //        simPage.selectSortingOptionCheckBox(SortingOption.STATUS, false);
    //        Assert.assertFalse("Status header cell is still displayed after column option is de-selected.", simPage.isExecutionCellDisplayed(ColumnName.STATUS));
    //
    //        //Navigate to column options in Name column(since status column is no longer displayed), select status check box and verify Status column displays
    //        simPage.hoverOverExecutionColumn(ColumnName.NAME);
    //        simPage.clickExecutionCellArrowIcon(ColumnName.NAME);
    //        simPage.hoverOverColumnsOption();
    //        simPage.selectSortingOptionCheckBox(SortingOption.STATUS, true);
    //        Assert.assertTrue("Status header cell is not displayed after column option is selected.", simPage.isExecutionCellDisplayed(ColumnName.STATUS));
    //
    //        //Click Column option to de-select
    //        simPage.clickExecutionColumn(ColumnName.NAME);
    //
    //        //Hover over Filter and verify status filter check boxes display (Not Started, In Progress, Completed, Error)
    //        simPage.hoverOverExecutionColumn(ColumnName.STATUS);
    //        simPage.clickExecutionCellArrowIcon(ColumnName.STATUS);
    //        simPage.hoverOverFiltersOption();
    //        simPage.checkFilterOptions();
    //
    //        //Select Not Started and verify only the completed execution displays
    //        String completed = completedExec.getName();
    //        String notStarted = notStartedExec.getName();
    //        simPage.selectSortingOptionCheckBox(SortingOption.NOT_STARTED, true);
    //        Assert.assertTrue("Not started execution is not displayed when Not Started filter check box is selected.", simPage.isExecutionDisplayed(notStarted));
    //        Assert.assertFalse("Completed execution is still displayed when Not Started filter check box is selected.", simPage.isExecutionDisplayed(completed));
    //
    //        //De-select Not Started, select Completed and verify only the completed execution displays
    //        simPage.selectSortingOptionCheckBox(SortingOption.NOT_STARTED, false);
    //        simPage.selectSortingOptionCheckBox(SortingOption.COMPLETED, true);
    //        Assert.assertFalse("Not started execution is still displayed when Completed filter check box is selected.", simPage.isExecutionDisplayed(notStarted));
    //        Assert.assertTrue("Completed execution is not displayed when Completed filter check box is selected.", simPage.isExecutionDisplayed(completed));
    //
    //        //De-select Completed, select In Progress and verify no executions display
    //        simPage.selectSortingOptionCheckBox(SortingOption.COMPLETED, false);
    //        simPage.selectSortingOptionCheckBox(SortingOption.IN_PROGRESS, true);
    //        Assert.assertFalse("Not started execution is still displayed when In Progress filter check box is selected.", simPage.isExecutionDisplayed(notStarted));
    //        Assert.assertFalse("Completed execution is still displayed when In Progress filter check box is selected.", simPage.isExecutionDisplayed(completed));
    //
    //        //De-select In Progress and verify all executions display
    //        simPage.selectSortingOptionCheckBox(SortingOption.IN_PROGRESS, false);
    //        Assert.assertTrue("Not started execution is not displayed when all filter check boxes are de-selected.", simPage.isExecutionDisplayed(notStarted));
    //        Assert.assertTrue("Completed execution is not displayed when all filter check boxes are de-selected.", simPage.isExecutionDisplayed(completed));
    //
    //        //Click Status column and verify executions are sorted in descending order by status
    //        simPage.clickExecutionColumn(ColumnName.STATUS);
    //        Assert.assertEquals("List is not in descending order, Not Started execution is not listed fist.", 0, simPage.getExecutionPosition(notStarted));
    //        Assert.assertEquals("List is not in descending order, Completed execution is not listed second.", 1, simPage.getExecutionPosition(completed));
    //
    //        //Click Status column and verify executions are sorted in ascending order by status
    //        simPage.clickExecutionColumn(ColumnName.STATUS);
    //        Assert.assertEquals("List is not in ascending order, Completed execution is not listed fist.", 0, simPage.getExecutionPosition(completed));
    //        Assert.assertEquals("List is not in ascending order, Not Started execution is not listed second.", 1, simPage.getExecutionPosition(notStarted));
    //
    //        //Finish the not started execution
    //        simPage.selectExecution(notStartedExec, true);
    //        simPage.clickFinish();
    //        Assert.assertTrue("Not started execution not updated to completed after selecting and clicking Finish.", simPage.isExecutionAndStatusDisplayed(notStarted, ExecutionStatus.COMPLETED));
    //
    //        //Log out
    //        simPage.logout(testUser);
    //
    //    }
}
