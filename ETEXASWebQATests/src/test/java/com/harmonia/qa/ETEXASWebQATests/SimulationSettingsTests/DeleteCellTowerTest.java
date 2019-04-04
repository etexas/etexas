package com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CellTower;
import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.CellTowerFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal.Btn;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.ConfigureCellTowersPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;

/**
 * Test class which executes steps for the Delete a Cell Tower Test,
 * TC-077/ITC-058
 *
 * @author llaroussini
 */
public class DeleteCellTowerTest extends ETexasAfterTestResetTestBase {

    /**
     * The user used in the test case
     */
    private ETexasUser testuser;

    /**
     * The simulation used in the test case.
     */
    private TemplateSimulation simulation;

    /**
     * Composite object used throughout the test case
     */
    private CompositeSimulation composite;

    /**
     * Simulation object used throughout the internal test case
     */
    private TemplateSimulation itcTestSim;

    /**
     * The cell tower which will be deleted
     */
    private CellTower cellTower;

    /**
     * The provider associated with the cell tower
     */
    private String cellTowerProvider;

    /**
     * Prerequisite steps/test setup
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get test user, test simulation, and cell tower
        testuser = ETexasUserFactory.getUser(true); //Get a random user.
        simulation = SimulationFactory.getTemplateSimulation(testuser, true); //get a random template simulation
        composite = simulation.getComposite();
        cellTower = CellTowerFactory.getCellTower(true); //get a random cell tower
        cellTowerProvider = cellTower.getProvider();
        ETexasEntityManager.addEntities(testuser, simulation, cellTower);

        //Simulation created with cell tower
        LandingPage landing = ETexasUserUtils.userRegistration(testuser);
        ETexasSimulationUtils.createTemplateSimulationWithCellTower(simulation, cellTower);

        //Create additional sim to be used in ITC
        //TODO - needs to be updated once exeuctions are re-incorporated into latest eTEXAS UI
        //itcTestSim = SimulationFactory.getTemplateSimulation(testUser, true); //gets random simulation
        //ETexasEntityManager.addEntities(itcTestSim);
        //ETexasSimulationUtils.createTemplateSimulation(itcTestSim);

        //Start execution for internal test case
        //TODO - needs to be updated once exeuctions are re-incorporated into latest eTEXAS UI
        //ETexasExecutionUtils.createNewExecution(itcTestSim);

        //User logged in
        landing.loginAs(testuser);
    }

    /**
     * Test steps for TC-077
     */
    @Test
    public void deleteCellTowerExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select an existing simulation with no executions and at least one cellular tower.
        simPage.expandComposite(composite, true);
        simPage.selectSim(simulation, true);

        //Click the Edit button, then click the Simulation Settings option
        simPage.clickEdit();
        SimulationSettingsModal simSettingsModal = simPage.clickSimulationSettings();

        //Click the Cell Towers tab.
        ConfigureCellTowersPartialPage towersTab = simSettingsModal.clickCellTowersTab();

        //Verify cell tower is displayed and select the cell tower
        Assert.assertTrue("The expected cell tower associated with provider, " + cellTowerProvider + ", is not displayed.", towersTab.isCellTowerDisplayed(cellTowerProvider));
        towersTab.selectRow(cellTowerProvider, true);

        //Verify the Delete button is enabled.
        Assert.assertTrue("The delete button is not enabled as expected when a cell tower is selected.", towersTab.isCellTowerDeleteBtnEnabled());

        //Click the Delete button.
        ConfirmDeleteModal deleteWarning = towersTab.clickDeleteCellTowerBtn();

        //Verify a Confirm Delete modal is displayed with text confirming deletion of the selected tower.
        deleteWarning.checkConfirmDeleteHeader();
        deleteWarning.checkDeleteWarningCellTowerContent(cellTower);

        //Verify an 'x' icon is displayed in the Confirm Delete modal
        Assert.assertTrue("The 'x' icon is not displayed in Confirm Delete modal as expected.", deleteWarning.isCloseIconDisplayed());

        //Verify that Yes and No buttons are displayed at the bottom of the modal.
        deleteWarning.checkConfirmDeleteBtns();

        //Click the No button.
        deleteWarning.clickBtn(Btn.NO);

        //Verify modal is no longer displayed.
        Assert.assertFalse("Delete Warning window is still displayed after clicking the No button.", deleteWarning.isCellTowerDeletionContentDisplayed(cellTower));

        //Verify the cell towers list is unchanged.
        Assert.assertTrue("Cell tower is not displayed in list after deletion is cancelled.", towersTab.isCellTowerDisplayed(cellTowerProvider));

        //Verify cell tower is selected, if not, select cell tower
        if (towersTab.isCellTowerRowSelected(cellTower) == false) {
            towersTab.selectRow(cellTowerProvider, true);
        }

        //Click the Delete button.
        deleteWarning = towersTab.clickDeleteCellTowerBtn();

        //Verify the Delete Warning modal is displayed
        deleteWarning.checkConfirmDeleteHeader();

        //Click the 'x' icon
        deleteWarning.clickCloseIcon();

        //Verify modal is no longer displayed.
        Assert.assertFalse("Delete Warning window is still displayed after clicking 'x' icon.", deleteWarning.isCellTowerDeletionContentDisplayed(cellTower));

        //Verify the cell towers list is unchanged.
        Assert.assertTrue("Cell tower is not displayed in list after deletion is cancelled.", towersTab.isCellTowerDisplayed(cellTowerProvider));

        //Verify cell tower is selected, if not, select cell tower
        if (!towersTab.isCellTowerRowSelected(cellTower)) {
            towersTab.selectRow(cellTowerProvider, true);
        }

        //Click the Delete button.
        deleteWarning = towersTab.clickDeleteCellTowerBtn();

        //Verify a Confirm Delete modal is displayed
        deleteWarning.checkConfirmDeleteHeader();

        //Click the Yes button.
        deleteWarning.clickBtn(Btn.YES);
        towersTab.waitUntilLoaded();

        //Verify modal is no longer displayed.
        Assert.assertFalse("Delete Warning window is still displayed after clicking the Yes button.", deleteWarning.isCellTowerDeletionContentDisplayed(cellTower));

        //Verify the cell tower is no longer displayed in the Cell Towers list.
        Assert.assertFalse("Cell tower is still displayed in list after deletion.", towersTab.isCellTowerDisplayed(cellTowerProvider));

        //Click Close button
        towersTab.clickClose();

        //Log out
        simPage.logout(testuser);
    }

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Test steps for ITC-058
    //	 */
    //	@Test
    //	public void deleteCellTowerInternalTest() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");
    //
    //		//Ensure Simulations page is loaded
    //		SimulationsPage simPage = getPage(SimulationsPage.class);
    //		simPage.waitUntilLoaded();
    //
    //		//Select multiple existing simulations.
    //		simPage.selectAllSimulations(true);
    //
    //		//Verify the Configure button is disabled.
    //		Assert.assertFalse("The configure button is not disabled as expected when more than one simulation is selected.", simPage.isSimBtnEnabled(SimBtns.CONFIGURE_BTN));
    //
    //		//Select an existing simulation with existing executions.
    //		simPage.selectAllSimulations(false);
    //		simPage.selectSimCheckBox(itcTestSim, true);
    //
    //		//Verify the Configure button is disabled.
    //		Assert.assertFalse("The configure button is not disabled as expected when a simulation with an existing execution is selected.", simPage.isSimBtnEnabled(SimBtns.CONFIGURE_BTN));
    //
    //		//De-select the execution.
    //		simPage.selectSimCheckBox(itcTestSim, false);
    //
    //		//Select an execution with no existing executions and at least one cell tower.
    //		simPage.selectSimCheckBox(simulation, true);
    //
    //		//Hover over the Configure button.
    //		simPage.clickEdit();
    //
    //		//Click the Environment option.
    //		SimulationSettingsModal environmentForm = simPage.clickCompositeSettings();
    //
    //		//Click the Cell Towers tab.
    //		ConfigureCellTowersPartialPage towersTab = environmentForm.clickCellTowersTab();
    //
    //		//Select a listed cell tower.
    //		towersTab.selectCheckBox(cellTowerProvider, true);
    //
    //		//Click the Delete button.
    //		DeleteWarningForm deleteWarning = towersTab.clickDeleteCellTowerBtn();
    //
    //		//Click the Yes button to confirm deletion.
    //		deleteWarning.clickBtn(Btn.YES);
    //
    //		//Verify the cell tower is no longer displayed.
    //		Assert.assertFalse("Cell tower is still displayed in list after deletion.", towersTab.isCellTowerDisplayed(cellTowerProvider));
    //
    //		//Click Close button
    //		towersTab.clickCloseBtn();
    //
    //		//Log out
    //		simPage.logout(testuser);
    //	}
}
