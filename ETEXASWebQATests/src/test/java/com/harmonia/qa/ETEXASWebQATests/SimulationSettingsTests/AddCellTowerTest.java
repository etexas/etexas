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
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CreateCellTowerModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;

/**
 * Test class which executes steps for the Create a Cell Tower test, TC-075
 *
 * @author llaroussini
 */
public class AddCellTowerTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Composite object used throughout the test case
     */
    private CompositeSimulation composite;

    /**
     * Simulation object used throughout the test case
     */
    private TemplateSimulation testSim;

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
        //Get test user, test simulation, and test cell tower
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        testSim = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        composite = testSim.getComposite();
        testCellTower = CellTowerFactory.getCellTower(true); //get a random cell tower
        testCellTowerProvider = testCellTower.getProvider();
        ETexasEntityManager.addEntities(testUser, testSim, testCellTower);

        //Register user and create new simulation from template
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulation(testSim);

        //Create additional sim to be used in ITC
        //TODO - needs to be updated once exeuctions are re-incorporated into latest eTEXAS UI
        //itcTestSim = SimulationFactory.getTemplateSimulation(testUser, true); //gets random simulation
        //ETexasEntityManager.addEntities(itcTestSim);
        //ETexasSimulationUtils.createTemplateSimulation(itcTestSim);

        //Start execution for internal test case
        //TODO - needs to be updated once exeuctions are re-incorporated into latest eTEXAS UI
        //ETexasExecutionUtils.createNewExecution(itcTestSim);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-075
     */
    @Test
    public void createCellTowerExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");
        //Ensure the Simulations page is loaded
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
        ConfigureCellTowersPartialPage cellTowersPage = simSettings.clickCellTowersTab();

        //Verify the following buttons are displayed across the top of the Cell Towers tab: Create, Edit, and Delete.
        cellTowersPage.checkBtns();

        //Verify the Create button is enabled.
        Assert.assertTrue("The Create button is not enabled by default as expected.", cellTowersPage.isCellTowerCreateBtnEnabled());

        //Verify the Edit and Delete buttons are disabled.
        Assert.assertTrue("The Edit button is not disabled by default as expected.", cellTowersPage.isCellTowerEditBtnEnabled());
        Assert.assertTrue("The Delete button is not disabled by default as expected.", cellTowersPage.isCellTowerDeleteBtnEnabled());

        //Verify a table is displayed listing the ID, Provider, X (cm), Y (cm), and Z (cm).
        cellTowersPage.checkCellTowersColumnHeaders();

        //Verify a Close button is displayed at the bottom of the modal
        Assert.assertTrue("Close button not displayed in window as expected.", cellTowersPage.isCloseBtnDisplayed());

        //Click the Add button.
        CreateCellTowerModal addTowerForm = cellTowersPage.clickCreateCellTowerBtn();

        //Verify the Add Cell Tower modal is displayed.
        Assert.assertTrue("Add Cell Tower header not displayed as expected.", addTowerForm.isCreateCellTowerHeaderDisplayed());

        //Verify that a ‘?’ icon and an ‘x’ icon are displayed in the upper right corner of the modal
        addTowerForm.checkCreateCellTowerHeaderIcons();

        //Click the ‘?’ icon.
        addTowerForm.clickHelpIcon();

        //Verify that Add Cell Tower Help modal is displayed with instructions for adding a cell tower.
        addTowerForm.checkHelpModal();

        //Verify an OK button is displayed in the modal
        Assert.assertTrue("OK button not displayed as expected in Create Cell Tower Help modal.", addTowerForm.isHelpOKBtnDisplayed());

        //Click the OK button.
        addTowerForm.clickHelpOKBtn();

        //Verify the Help modal closes.
        Assert.assertFalse("Create Cell Tower Help header still displayed after clicking OK button.", addTowerForm.isCreateCellTowerHelpHeaderDisplayed());

        //Verify the following text boxes are displayed in the Add Cell Tower modal: Cell Tower Provider, X Coordinate, Y Coordinate, and Z Coordinate.
        addTowerForm.checkFieldsDisplayed();

        //Verify the following buttons are displayed at the bottom of the modal: Create, Reset, and Cancel.
        addTowerForm.checkBtns();

        //Enter valid values in all fields
        addTowerForm.setAllFields(testCellTower);

        //Click the Reset button.
        addTowerForm.clickReset();

        //Verify the text boxes are reset to their default values.
        addTowerForm.checkFieldValues("", "", "", "");

        //Enter valid values in all text boxes.
        addTowerForm.setAllFields(testCellTower);

        //Click the Cancel button.
        addTowerForm.clickCancel();

        //Verify the Add Cell Tower window closes.
        Assert.assertFalse("Add Cell Tower header is still displayed after canceling.", addTowerForm.isCreateCellTowerHeaderDisplayed());

        //Verify the cell tower is not displayed in the Cell Towers table.
        Assert.assertFalse("Cell Tower is displayed in list after being canceled.", cellTowersPage.isCellTowerDisplayed(testCellTowerProvider));

        //Click the Create button again.
        addTowerForm = cellTowersPage.clickCreateCellTowerBtn();

        //Enter valid values in all text boxes.
        addTowerForm.setAllFields(testCellTower);

        //Click the Close icon
        addTowerForm.clickCancel();

        //Verify the Add Cell Tower window closes.
        Assert.assertFalse("Add Cell Tower header is still displayed after canceling.", addTowerForm.isCreateCellTowerHeaderDisplayed());

        //Verify the cell tower is not displayed in the Cell Towers table.
        Assert.assertFalse("Cell Tower is displayed in list after being canceled.", cellTowersPage.isCellTowerDisplayed(testCellTowerProvider));

        //Click the Create button again.
        addTowerForm = cellTowersPage.clickCreateCellTowerBtn();

        //Enter valid values in all text boxes.
        addTowerForm.setAllFields(testCellTower);

        //Click the Create button.
        addTowerForm.clickCreate(true);

        //Verify the cell tower is displayed in the Cell Towers table.
        Assert.assertTrue("Cell Tower is not displayed in list after being created.", cellTowersPage.isCellTowerDisplayed(testCellTowerProvider));

        //Verify the provider, x coordinate, y coordinate, and z coordinate values match the values entered previously.
        Assert.assertTrue("Cell Tower provider value is not displayed in list after being created.", cellTowersPage.isCellTowerDisplayed(testCellTowerProvider));
        Assert.assertTrue("Cell Tower X Coordiante value is not displayed in list after being created.", cellTowersPage.isCellTowerDisplayed(testCellTower.getXCoordinate()));
        Assert.assertTrue("Cell Tower Y Coordiate value is not displayed in list after being created.", cellTowersPage.isCellTowerDisplayed(testCellTower.getYCoordinate()));
        Assert.assertTrue("Cell Tower Z Coordiate value is not displayed in list after being created.", cellTowersPage.isCellTowerDisplayed(testCellTower.getZCoordinate()));

        //Click close
        cellTowersPage.clickClose();

        //Log out
        simPage.logout(testUser);
    }

    //TODO - test needs to be updated prior to being re-enabled due to updated eTEXAS UI
    //	/**
    //	 * Test steps for ITC-056
    //	 */
    //	@Test
    //	public void addCellTowerInternalTest() {
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
    //		Assert.assertFalse("The configure button is not disabled as expected when more than one simulation is selected.", simPage.isSimBtnEnabled(SimBtns.EDIT_BTN));
    //
    //		//Select an existing simulation with existing executions.
    //		simPage.selectAllSimulations(false);
    //		simPage.selectSimCheckBox(itcTestSim, true);
    //
    //		//Verify the Configure button is disabled.
    //		Assert.assertFalse("The configure button is not disabled as expected when a simulation with an existing execution is selected.", simPage.isSimBtnEnabled(SimBtns.EDIT_BTN));
    //
    //		//De-select the execution.
    //		simPage.selectSimCheckBox(itcTestSim, false);
    //
    //		//Select an execution with no existing executions and at least one cell tower.
    //		simPage.selectSimCheckBox(testSim, true);
    //
    //		//Hover over the Configure button.
    //		simPage.clickEdit();
    //
    //		//Click the Environment option.
    //		SimulationSettingsModal enviroPage = simPage.clickSimulationSettings();
    //
    //		//Click the Cell Towers tab.
    //		ConfigureCellTowersPartialPage towersTab = enviroPage.clickCellTowersTab();
    //
    //		//Click the Add button.
    //		AddCellTowerForm addTowerForm = towersTab.clickCreateCellTowerBtn();
    //
    //		//With all fields blank, click the Create button.
    //		addTowerForm.clickCreateErrorExpected();
    //
    //		//Verify field required error displays for each field
    //		addTowerForm.checkRequiredFieldErrorAllFields();
    //
    //		//Enter one or more space characters in the Cell Tower Provider text box.
    //		//Enter valid values in the remaining fields.
    //		addTowerForm.setAllFields(testCellTower);
    //		addTowerForm.setCellTowerProvider("  ");
    //		//Verify an error is displayed associated with the Cell Tower Provider text box indicating the field does not support leading/trailing whitespace.
    //		Assert.assertTrue("Whitespace error is not displayed as expected when whitespace only is entered in Cell Tower Provider field.", addTowerForm.isCellTowerProviderWhitespaceErrorDisplayed());
    //
    //		//Enter a value in the Cell Tower Provider text box that contains special characters, not including dashes (e.g., ‘T3$t’).
    //		addTowerForm.setCellTowerProvider("T3$t!");
    //		//Verify an error is displayed associated with the Cell Tower Provider text box indicating the acceptable values for the field.
    //		Assert.assertTrue("Invalid Cell Tower Provider error is not displayed as expected when special characters are used in the Cell Tower Provider field.",
    //				addTowerForm.isInvalidCellTowerProviderErrorDisplayed());
    //
    //		//Enter a value in the Cell Tower Provider text box that contains consecutive spaces within the entered text (e.g., ‘Te    st’).
    //		addTowerForm.setCellTowerProvider(RandomStringGenerator.nextLetterString(5) + "  " + RandomStringGenerator.nextLetterString(5));
    //		//Verify an error is displayed associated with the Cell Tower Provider text box indicating the acceptable values for the field.
    //		Assert.assertTrue("Invalid Cell Tower Provider error is not displayed as expected when consecutive spaces are used in the Cell Tower Provider field.",
    //				addTowerForm.isInvalidCellTowerProviderErrorDisplayed());
    //
    //		//Enter a value in the Cell Tower Provider text box that begins with a space (e.g., ‘ Test’).
    //		addTowerForm.setCellTowerProvider(" " + RandomStringGenerator.nextLetterString(10));
    //		//Verify an error is displayed associated with the Cell Tower Provider text box indicating the acceptable values for the field.
    //		Assert.assertTrue("Whitespace error is not displayed as expected when a leading space is entered in Cell Tower Provider field.", addTowerForm.isCellTowerProviderWhitespaceErrorDisplayed());
    //
    //		//Enter a value in the Cell Tower Provider text box that ends with a space (e.g., ‘Test ’).
    //		addTowerForm.setCellTowerProvider(RandomStringGenerator.nextLetterString(10) + " ");
    //		//Verify an error is displayed associated with the Cell Tower Provider text box indicating the acceptable values for the field.
    //		Assert.assertTrue("Whitespace error is not displayed as expected when a trailing space is entered in Cell Tower Provider field.", addTowerForm.isCellTowerProviderWhitespaceErrorDisplayed());
    //
    //		//Enter a value in the Cell Tower Provider text box that begins with a hyphen/dash (e.g., ‘-Test’).
    //		addTowerForm.setCellTowerProvider("-" + RandomStringGenerator.nextLetterString(10));
    //		//Verify an error is displayed associated with the Cell Tower Provider text box indicating the acceptable values for the field.
    //		Assert.assertTrue("Invalid Cell Tower Provider error is not displayed as expected when a leading '-' is used in the Cell Tower Provider field.",
    //				addTowerForm.isInvalidCellTowerProviderErrorDisplayed());
    //
    //		//Enter a value in the Cell Tower Provider text box that ends with a hyphen/dash (e.g., ‘Test-’).
    //		addTowerForm.setCellTowerProvider(RandomStringGenerator.nextLetterString(10) + "-");
    //		//Verify an error is displayed associated with the Cell Tower Provider text box indicating the acceptable values for the field.
    //		Assert.assertTrue("Invalid Cell Tower Provider error is not displayed as expected when a trailing '-' is used in the Cell Tower Provider field.",
    //				addTowerForm.isInvalidCellTowerProviderErrorDisplayed());
    //
    //		//Enter a valid name in the Cell Tower Provider text box.
    //		//Enter a non-numerical value in the X Coordinate text box (e.g., ‘---’).
    //		addTowerForm.setCellTowerProvider(testCellTower);
    //		addTowerForm.setXCoordinate("---");
    //		//Verify an error is displayed associated with the X Coordinate text box indicating the value entered is not a valid number.
    //		Assert.assertTrue("Invalid X Coordinate error is not displayed as expected when a non-numeric value is used in the X Coordinate field.", addTowerForm.isInvalidXCoordinateErrorDisplayed());
    //
    //		//Enter a valid numerical value in the X Coordinate text box.
    //		//Enter a non-numerical value in the Y Coordinate text box (e.g., ‘---’).
    //		addTowerForm.setXCoordinate(testCellTower);
    //		addTowerForm.setYCoordinate("---");
    //		//Verify an error is displayed associated with the Y Coordinate text box indicating the value entered is not a valid number.
    //		Assert.assertTrue("Invalid Y Coordinate error is not displayed as expected when a non-numeric value is used in the Y Coordinate field.", addTowerForm.isInvalidYCoordinateErrorDisplayed());
    //
    //		//Enter a valid numerical value in the Y Coordinate text box.
    //		//Enter a non-numerical value in the Z Coordinate text box (e.g., ‘---’).
    //		addTowerForm.setYCoordinate(testCellTower);
    //		addTowerForm.setZCoordinate("---");
    //		//Verify an error is displayed associated with the Z Coordinate text box indicating the value entered is not a valid number.
    //		Assert.assertTrue("Invalid Z Coordinate error is not displayed as expected when a non-numeric value is used in the Z Coordinate field.", addTowerForm.isInvalidZCoordinateErrorDisplayed());
    //
    //		//Enter a valid numerical value in the Z Coordinate text box and click the Create button.
    //		addTowerForm.setZCoordinate(testCellTower);
    //		addTowerForm.clickCreate(true);
    //
    //		//Verify the cell tower is listed in the Cell Towers list.
    //		Assert.assertTrue("Cell Tower is not displayed in list after being created.", towersTab.isCellTowerDisplayed(testCellTowerProvider));
    //
    //		//Click the Close button.
    //		towersTab.clickCloseBtn();
    //
    //		//Log out
    //		simPage.logout(testUser);
    //	}
}