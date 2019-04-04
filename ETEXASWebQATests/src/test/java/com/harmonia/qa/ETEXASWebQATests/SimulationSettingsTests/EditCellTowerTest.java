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
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.ConfigureCellTowersPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.EditCellTowerForm;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;

/**
 * Test class which executes steps for the Edit a Cell Tower test,
 * TC-076/ITC-057
 *
 * @author llaroussini
 */
public class EditCellTowerTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Simulation object used throughout the test case
     */
    private TemplateSimulation testSim;

    /**
     * Composite object used throughout the test case
     */
    private CompositeSimulation composite;

    /**
     * Simulation object used throughout the internal test case
     */
    private TemplateSimulation itcTestSim;

    /**
     * Cell tower object used throughout the test case
     */
    private CellTower testCellTower;

    /**
     * The provider associated with the cell tower object used throughout the
     * test case
     */
    private String testCellTowerProvider;

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");
        //Get test user, test simulation, and test cell tower device
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        testSim = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        composite = testSim.getComposite();
        testCellTower = CellTowerFactory.getCellTower(true); //get a random cell tower
        testCellTowerProvider = testCellTower.getProvider();
        ETexasEntityManager.addEntities(testUser, testSim, testCellTower);

        //Create additional sim to be used in ITC
        //TODO - needs to be updated once exeuctions are re-incorporated into latest eTEXAS UI
        //		itcTestSim = SimulationFactory.getTemplateSimulation(testUser, true); //gets random simulation
        //		ETexasEntityManager.addEntities(itcTestSim);
        //		//Create additional simulation with started execution to be used in ITC
        //		ETexasSimulationUtils.createTemplateSimulation(itcTestSim);
        //		ETexasExecutionUtils.createNewExecution(itcTestSim);

        //Register user and create new simulation from template with cell tower
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulationWithCellTower(testSim, testCellTower);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-076
     */
    @Test
    public void editCellTowerExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select simulation
        simPage.expandComposite(composite, true);
        simPage.selectSim(testSim, true);

        //Click Edit
        simPage.clickEdit();
        simPage.checkEditOptions();

        //Click Simulation Settings option and verify Simulation Settings modal displays
        SimulationSettingsModal simSettings = simPage.clickSimulationSettings();

        //Click the Cell Towers tab.
        ConfigureCellTowersPartialPage cellTab = simSettings.clickCellTowersTab();

        //Select an existing cell tower.
        cellTab.selectRow(testCellTowerProvider, true);
        Assert.assertTrue("The cell tower with an ID of " + testCellTower.getID() + " is not selected as expected.", cellTab.isCellTowerRowSelected(testCellTower));

        //Verify the Edit button is enabled.
        cellTab.isCellTowerEditBtnEnabled();

        //Click the Edit button.
        EditCellTowerForm editTowerForm = cellTab.clickEdit();

        //Verify the Edit Cell Tower modal is displayed.
        Assert.assertTrue("Edit Cell Tower header is not displayed as expected.", editTowerForm.isEditCellTowerHeaderDisplayed());

        //Verify that a ‘?’ icon and an ‘x’ icon are displayed in the upper right corner of the modal.
        editTowerForm.checkEditCellTowerHeaderIcons();

        //Click the ‘?’ icon.
        editTowerForm.clickEditCellTowerHelp();

        //Verify that that the Edit Cell Tower Help modal is displayed with instructions for editing cell towers.
        editTowerForm.checkHelpModal();

        //Verify an OK button is displayed in the modal and click the OK button.
        Assert.assertTrue("An OK button is not displayed in the Edit Cell Tower Help window.", editTowerForm.isEditCellTowerHelpOKBtnDisplayed());
        editTowerForm.clickEditCellTowerHelpOKBtn();

        //Verify the Help modal closes.
        Assert.assertFalse("Edit Cell Tower Help header is still displayed after clicking OK to close.", editTowerForm.isEditCellTowerHelpHeaderDisplayed());

        //Verify the following text boxes are displayed in the Edit Cell Tower modal: X (cm), Y (cm), Z (cm), and Provider.
        editTowerForm.checkFieldsDisplayed();

        //Verify the fields are populated with the cell tower’s associated information.
        editTowerForm.checkFieldValues(testCellTower);

        //Verify the following buttons are displayed at the bottom of the window: Update, Reset, and Cancel.
        editTowerForm.checkEditCellTowerBtns();

        //Change the values in all fields
        CellTower newCellTower = CellTowerFactory.getCellTower(true); //create another random cell tower object to use to update values
        editTowerForm.setAllFields(newCellTower);

        //Click the Reset button.
        editTowerForm.clickReset();

        //Verify all values are reset to their default values.
        editTowerForm.checkFieldValues(testCellTower);

        //Change the values in all fields
        editTowerForm.setAllFields(newCellTower);

        //Click the Cancel button.
        editTowerForm.clickCancel();

        //Verify the Edit Cell Tower modal closes.
        Assert.assertFalse("Edit Cell Tower header is still displayed after clicking Cancel.", editTowerForm.isEditCellTowerHeaderDisplayed());

        //Verify the cell tower is still displayed in the Cell Towers list.
        Assert.assertTrue("Original Cell Tower is not displayed in list after update is canceled.", cellTab.isCellTowerDisplayed(testCellTowerProvider));

        //Verify the cell tower still displays the original associated information in the Cell Tower list.
        Assert.assertTrue("Original Cell Tower provider value is not displayed in list after update is canceled.", cellTab.isCellTowerDisplayed(testCellTowerProvider));
        Assert.assertTrue("Original Cell Tower X Coordiante value is not displayed in list after update is canceled.", cellTab.isCellTowerDisplayed(testCellTower.getXCoordinate()));
        Assert.assertTrue("Original Cell Tower Y Coordiate value is not displayed in list after update is canceled.", cellTab.isCellTowerDisplayed(testCellTower.getYCoordinate()));
        Assert.assertTrue("Original Cell Tower Z Coordiate value is not displayed in list after update is canceled.", cellTab.isCellTowerDisplayed(testCellTower.getZCoordinate()));

        //Verify cell tower is selected, if not, select the tower
        if (cellTab.isCellTowerRowSelected(testCellTower) == false) {
            cellTab.selectRow(testCellTowerProvider, true);
        }

        //Click the Edit button again.
        editTowerForm = cellTab.clickEdit();

        //Change the values in all fields.
        editTowerForm.setAllFields(newCellTower);

        //Click the Close icon.
        editTowerForm.clickCloseIcon();

        //Verify the Edit Cell Tower modal closes.
        Assert.assertFalse("Edit Cell Tower header is still displayed after clicking Cancel.", editTowerForm.isEditCellTowerHeaderDisplayed());

        //Verify the cell tower is still displayed in the Cell Towers list.
        Assert.assertTrue("Original Cell Tower is not displayed in list after update is canceled.", cellTab.isCellTowerDisplayed(testCellTowerProvider));

        //Verify the cell tower still displays the original associated information in the Cell Tower list.
        Assert.assertTrue("Original Cell Tower provider value is not displayed in list after update is canceled.", cellTab.isCellTowerDisplayed(testCellTowerProvider));
        Assert.assertTrue("Original Cell Tower X Coordiante value is not displayed in list after update is canceled.", cellTab.isCellTowerDisplayed(testCellTower.getXCoordinate()));
        Assert.assertTrue("Original Cell Tower Y Coordiate value is not displayed in list after update is canceled.", cellTab.isCellTowerDisplayed(testCellTower.getYCoordinate()));
        Assert.assertTrue("Original Cell Tower Z Coordiate value is not displayed in list after update is canceled.", cellTab.isCellTowerDisplayed(testCellTower.getZCoordinate()));

        //Verify cell tower is selected, if not, select the tower
        if (cellTab.isCellTowerRowSelected(testCellTower) == false) {
            cellTab.selectRow(testCellTowerProvider, true);
        }

        //Click the Edit button again.
        editTowerForm = cellTab.clickEdit();

        //Change the values in all fields.
        editTowerForm.setAllFields(newCellTower);

        //Click the Update button.
        editTowerForm.clickUpdate(true);

        //Verify the Edit Cell Tower modal closes.
        Assert.assertFalse("Edit Cell Tower header is still displayed after clicking Update.", editTowerForm.isEditCellTowerHeaderDisplayed());

        //Verify the original cell tower is no longer displayed in the Cell Towers list.
        Assert.assertFalse("Original Cell Tower is still displayed in list after update.", cellTab.isCellTowerDisplayed(testCellTowerProvider));

        //Verify the updated cell tower is displayed with the associated updated information in the Cell Towers list.
        Assert.assertTrue("Updated Cell Tower is not displayed in list after update.", cellTab.isCellTowerDisplayed(newCellTower.getProvider()));

        //Verify cell tower ID is unchanged
        Assert.assertEquals("The ID of the updated Cell Tower did not remain static.", testCellTower.getID(), cellTab.getCellTowerID(newCellTower));

        //Click close
        cellTab.clickClose();

        //Log out
        simPage.logout(testUser);
    }

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Test steps for ITC-057
    //	 */
    //	@Test
    //	public void editCellTowerInternalTest() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");
    //
    //		//Verify the Simulations page is displayed.
    //		SimulationsPage simPage = getPage(SimulationsPage.class);
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
    //		simPage.selectSimCheckBox(testSim, true);
    //
    //		//Hover over the Configure button and click the Environment option.
    //		simPage.hoverOverConfigure();
    //		ConfigureEnvironmentPartialPage enviroPage = simPage.clickEnvironment();
    //
    //		//Click the Cell Towers tab.
    //		ConfigureCellTowersPartialPage cellTab = enviroPage.clickCellTowersTab();
    //
    //		//Select an existing cell tower.
    //		cellTab.selectCheckBox(testCellTowerProvider, true);
    //
    //		//Click the Edit button, delete the pre-populated values in all fields, and click the Update button
    //		EditCellTowerForm editForm = cellTab.clickEdit();
    //		editForm.setAllFields("", "", "", "");
    //		editForm.clickUpdate(false);
    //		//Verify field required errors are displayed with all fields
    //		editForm.checkRequiredFieldErrorAllFields();
    //
    //		//Enter one or more space characters in the Cell Tower Provider text box and valid values in the remaining fields.
    //		editForm.setAllFields(testCellTower);
    //		editForm.setCellTowerProvider("  ");
    //		editForm.clickUpdate(false);
    //		//Verify an error is displayed associated with the Cell Tower Provider text box indicating the field does not support leading/trailing whitespace.
    //		Assert.assertTrue("Whitespace error is not displayed as expected when whitespace only is entered in Cell Tower Provider field.", editForm.isCellTowerProviderWhitespaceErrorDisplayed());
    //
    //		//Enter a value in the Cell Tower Provider text box that contains special characters, not including dashes (e.g., ‘T3$t’).
    //		editForm.setCellTowerProvider("T#$t!");
    //		editForm.clickUpdate(false);
    //		//Verify an error is displayed associated with the Cell Tower Provider text box indicating the acceptable values for the field.
    //		Assert.assertTrue("Invalid Cell Tower Provider error is not displayed as expected when special characters are used in the Cell Tower Provider field.",
    //				editForm.isInvalidCellTowerProviderErrorDisplayed());
    //
    //		//Enter a value in the Cell Tower Provider text box that contains consecutive spaces within the entered text (e.g., ‘Te    st’).
    //		editForm.setCellTowerProvider(RandomStringGenerator.nextLetterString(3) + "  " + RandomStringGenerator.nextLetterString(2));
    //		editForm.clickUpdate(false);
    //		//Verify an error is displayed associated with the Cell Tower Provider text box indicating the acceptable values for the field.
    //		Assert.assertTrue("Invalid Cell Tower Provider error is not displayed as expected when consecutive spaces are used in the Cell Tower Provider field.",
    //				editForm.isInvalidCellTowerProviderErrorDisplayed());
    //
    //		//Enter a value in the Cell Tower Provider text box that begins with a space (e.g., ‘ Test’).
    //		editForm.setCellTowerProvider(" " + RandomStringGenerator.nextLetterString(5));
    //		editForm.clickUpdate(false);
    //		//Verify an error is displayed associated with the Cell Tower Provider text box indicating the acceptable values for the field.
    //		Assert.assertTrue("Whitespace error is not displayed as expected when a leading space is entered in Cell Tower Provider field.", editForm.isCellTowerProviderWhitespaceErrorDisplayed());
    //
    //		//Enter a value in the Cell Tower Provider text box that ends with a space (e.g., ‘Test ’).
    //		editForm.setCellTowerProvider(RandomStringGenerator.nextLetterString(5) + " ");
    //		editForm.clickUpdate(false);
    //		//Verify an error is displayed associated with the Cell Tower Provider text box indicating the acceptable values for the field.
    //		Assert.assertTrue("Whitespace error is not displayed as expected when a trailing space is entered in Cell Tower Provider field.", editForm.isCellTowerProviderWhitespaceErrorDisplayed());
    //
    //		//Enter a value in the Cell Tower Provider text box that begins with a hyphen/dash (e.g., ‘-Test’).
    //		editForm.setCellTowerProvider("-" + RandomStringGenerator.nextLetterString(5));
    //		editForm.clickUpdate(false);
    //		//Verify an error is displayed associated with the Cell Tower Provider text box indicating the acceptable values for the field.
    //		Assert.assertTrue("Invalid Cell Tower Provider error is not displayed as expected when a leading '-' is used in the Cell Tower Provider field.",
    //				editForm.isInvalidCellTowerProviderErrorDisplayed());
    //
    //		//Enter a value in the Cell Tower Provider text box that ends with a hyphen/dash (e.g., ‘Test-’).
    //		editForm.setCellTowerProvider(RandomStringGenerator.nextLetterString(5) + "-");
    //		editForm.clickUpdate(false);
    //		//Verify an error is displayed associated with the Cell Tower Provider text box indicating the acceptable values for the field.
    //		Assert.assertTrue("Invalid Cell Tower Provider error is not displayed as expected when a trailing '-' is used in the Cell Tower Provider field.",
    //				editForm.isInvalidCellTowerProviderErrorDisplayed());
    //
    //		//Enter a different, but valid name in the Cell Tower Provider text box and enter a non-numerical value in the X Coordinate text box (e.g., ‘---’).
    //		String newProvider = RandomStringGenerator.nextLetterString(5);
    //		editForm.setCellTowerProvider(newProvider);
    //		editForm.setXCoordinate("---");
    //		//Verify an error is displayed associated with the X Coordinate text box indicating the value entered is not a valid number.
    //		Assert.assertTrue("Invalid X Coordinate error is not displayed as expected when a non-numeric value is used in the X Coordinate field.", editForm.isInvalidXCoordinateErrorDisplayed());
    //
    //		//Enter a different, but valid numerical value in the X Coordinate text box.
    //		String newX = RandomStringGenerator.nextNumStringOfLength(2);
    //		editForm.setXCoordinate(newX);
    //		editForm.setYCoordinate("---");
    //		//Enter a non-numerical value in the Y Coordinate text box (e.g., ‘---’).
    //		//Verify an error is displayed associated with the Y Coordinate text box indicating the value entered is not a valid number.
    //		Assert.assertTrue("Invalid Y Coordinate error is not displayed as expected when a non-numeric value is used in the Y Coordinate field.", editForm.isInvalidYCoordinateErrorDisplayed());
    //
    //		//Enter a different, but valid numerical value in the Y Coordinate text box.
    //		String newY = RandomStringGenerator.nextNumStringOfLength(2);
    //		editForm.setYCoordinate(newY);
    //		editForm.setZCoordinate("---");
    //		//Enter a non-numerical value in the Z Coordinate text box (e.g., ‘---’).
    //		//Verify an error is displayed associated with the Z Coordinate text box indicating the value entered is not a valid number.
    //		Assert.assertTrue("Invalid Z Coordinate error is not displayed as expected when a non-numeric value is used in the Z Coordinate field.", editForm.isInvalidZCoordinateErrorDisplayed());
    //
    //		//Enter a different, but valid numerical value in the Z Coordinate text box and click Update
    //		String newZ = RandomStringGenerator.nextNumStringOfLength(2);
    //		editForm.setZCoordinate(newZ);
    //		editForm.clickUpdate(true);
    //
    //		//Verify the updated cell tower is listed in the Cell Towers list.
    //		cellTab.isCellTowerDisplayed(newProvider);
    //
    //		//Click close
    //		cellTab.clickCloseBtn();
    //
    //		//Log out
    //		simPage.logout(testUser);
    //	}
}
