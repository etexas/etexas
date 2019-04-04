package com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.OBUDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.OBUDeviceFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeSettingsModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.ConfigureOBUDeviceProfilesPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.ConfigureOBUDeviceProfilesPartialPage.OBUDeviceTableColumn;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.CreateOBUDeviceProfileModal;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomNumberGenerator;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class which executes steps for the Add an On-Board Unit Device Profile
 * test, TC-040 and ITC-007
 *
 * @author llaroussini
 */
public class AddOBUDeviceProfileTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Simulation object used throughout the test case
     */
    private TemplateSimulation simulation;

    /**
     * Composite object used throughout the test case
     */
    private CompositeSimulation composite;

    /**
     * OBU device object used throughout the test case
     */
    private OBUDevice obu;

    /**
     * OBU device object used in the internal test case
     */
    private OBUDevice anotherTestOBU;

    /**
     * Test setup
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get user, simulation, composite, and OBU device used in TC
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        simulation = SimulationFactory.getTemplateSimulation(testUser, true); //gets a random template simulation
        composite = simulation.getComposite();
        obu = OBUDeviceFactory.getOBUDevice(true); //gets a random OBU device
        obu.setOBUPercent("25"); //Known percentage set to ensure total percentage of devices can be consistently set to either less than or greater than 100%
        anotherTestOBU = OBUDeviceFactory.getOBUDevice(true); //get a random OBU device
        anotherTestOBU.setOBUPercent("25"); //Known percentage set to ensure total percentage of devices can be consistently set to either less than or greater than 100%
        ETexasEntityManager.addEntities(testUser, simulation, composite, obu, anotherTestOBU);

        //Register user and create new simulation from template
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulationWithOBUDevice(simulation, anotherTestOBU);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-040
     */
    @Test
    public void addOBUExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify Simulations page is displayed
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select a composite that has no executions.
        simPage.selectComposite(composite, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Composite Settings option.
        CompositeSettingsModal settingsModal = simPage.clickCompositeSettings();

        //Verify the Composite Settings modal is displayed.
        Assert.assertTrue("Composite Settings modal is not displayed as expected.", settingsModal.isCompositeSettingsHeaderDisplayed());

        //Click the OBU Device Profiles tab.
        ConfigureOBUDeviceProfilesPartialPage obuTab = settingsModal.clickOBUTab();

        //Verify the following buttons are displayed across the top of the OBU Device Profiles tab: Create, Edit, Delete, and Applications.
        obuTab.checkOBUBtns();

        //Verify the Create button is enabled.
        Assert.assertTrue("The Create button is not enabled by default in the OBU Device Profiles tab.", obuTab.isOBUCreateBtnEnabled());

        //Verify the Edit, Delete, and Applications buttons are disabled.
        Assert.assertFalse("The Edit button is not diabled by default in the OBU Device Profiles tab.", obuTab.isOBUEditBtnEnabled());
        Assert.assertFalse("The Delete button is not diabled by default in the OBU Device Profiles tab.", obuTab.isOBUDeleteBtnEnabled());
        Assert.assertFalse("The Applications button is not diabled by default in the OBU Device Profiles tab.", obuTab.isOBUApplicationsBtnEnabled());

        //Verify a table is displayed with the following columns: ID, Name, and Percentage.
        obuTab.checkOBUDeviceColumnHeaders();

        //Verify a Close button is displayed at the bottom of the modal.
        Assert.assertTrue("Close button is not displayed at the bottom of the modal as expected.", obuTab.isCloseBtnDisplayed());

        //Click the Create button.
        CreateOBUDeviceProfileModal createModal = obuTab.clickCreate();

        //Verify a Create OBU Device Profile modal is displayed.
        Assert.assertTrue("Create OBU Device Profile modal is not displayed as expected.", createModal.isCreateOBUDeviceProfileHeaderDisplayed());

        //Verify the following icons are displayed in the top right corner of the modal: ‘?’ and ‘x’.
        createModal.checkHeaderIcons();

        //Click the ‘?’ icon.
        createModal.clickHelpIcon();

        //Verify that a Composite Settings Help modal is displayed with instructions for creating OBU device profiles.
        createModal.checkHelpModal();

        //Verify an OK button is displayed in the modal.
        Assert.assertTrue("OK button is not displayed as expected in Create OBU Device Profile Help modal.", createModal.isHelpOKBtnDisplayed());

        //Click the OK button.
        createModal.clickHelpOKBtn();

        //Verify the Help modal closes.
        Assert.assertFalse("Create OBU Device Profile Help modal is still displayed after clicking OK.", createModal.isCreateOBUDeviceProfileHelpHeaderDisplayed());

        //Verify the following text boxes are displayed in the Create OBU Device Profile modal: Name and Percentage.
        createModal.checkFields();

        //Verify the following buttons are displayed at the bottom of the Create OBU Device Profile: Create, Reset, and Cancel.
        createModal.checkBtns();

        //Enter a valid value in the Name text box
        createModal.setName(obu);

        //Enter a valid value in the Percentage text box
        createModal.setPercentage(obu);

        //Click the Reset button.
        createModal.clickReset();

        //Verify all values are returned to their default state.
        createModal.checkFieldValuesDisplayed("", "");

        //Enter valid values in all text boxes.
        createModal.setAllFields(obu);

        //Click the Cancel button.
        createModal.clickCancel();

        //Verify the Create OBU Device Profile modal is no longer displayed.
        Assert.assertFalse("Create OBU Device Profile modal is still displayed after clicking Cancel.", createModal.isCreateOBUDeviceProfileHeaderDisplayed());

        //Verify the new OBU device profile is not listed in the OBU Device Profiles table.
        Assert.assertFalse("New OBU Device Profile is displayed in OBU Device Profiles table despite cancelling the creation.", obuTab.isOBUDisplayed(obu));

        //Click the Create button.
        obuTab.clickCreate();

        //Enter valid values in all text boxes.
        createModal.setAllFields(obu);

        //Click the ‘x’ icon.
        createModal.clickCloseIcon();

        //Verify the Create OBU Device Profile is no longer displayed.
        Assert.assertFalse("Create OBU Device Profile modal is still displayed after clicking Close.", createModal.isCreateOBUDeviceProfileHeaderDisplayed());

        //Verify the new OBU device profile is not listed in the OBU Device Profiles table.
        Assert.assertFalse("New OBU Device Profile is displayed in OBU Device Profiles table despite closing the Create OBU Device Profile modal.", obuTab.isOBUDisplayed(obu));

        //Click the Create button.
        obuTab.clickCreate();

        //Enter valid values in all text boxes.
        createModal.setAllFields(obu);

        //Click the Create button.
        createModal.clickCreate(true);

        //Verify the Create OBU Device Profile modal is no longer displayed.
        Assert.assertFalse("Create OBU Device Profile modal is still displayed after clicking Create.", createModal.isCreateOBUDeviceProfileHeaderDisplayed());

        //Verify the new OBU device profile is listed in the OBU Device Profiles table.
        Assert.assertTrue("New OBU Device Profile is not displayed in OBU Device Profiles table despite successful creation.", obuTab.isOBUDisplayed(obu));

        //Verify the Name and Percentage columns are populated with the values entered previously.
        Assert.assertEquals("OBU Device Profile name value is not displayed in list after being created.", obu.getName(), obuTab.getOBUCellValue(obu, OBUDeviceTableColumn.NAME));
        Assert.assertEquals("OBU Device Profile percentage value is not displayed in list after being created.", obu.getOBUPercent() + "%",
                obuTab.getOBUCellValue(obu, OBUDeviceTableColumn.PERCENTAGE));

        //Verify the newly created OBU device profile has an auto-generated ID displayed in the ID column.
        Assert.assertNotNull("An ID was not generated for the newly created OBU device.", obuTab.getOBUCellValue(obu, OBUDeviceTableColumn.ID));

        //Click the Close button.
        obuTab.clickCloseBtn();

        //Log out
        simPage.logout(testUser);
    }

    /**
     * Test steps for ITC-007
     */
    @Test
    public void addOBUInternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select a composite with at least one device (profile).
        simPage.selectComposite(composite, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Composite Settings option.
        CompositeSettingsModal compositeSettingsModal = simPage.clickCompositeSettings();

        //Click the OBU Device Profiles tab.
        ConfigureOBUDeviceProfilesPartialPage obuTab = compositeSettingsModal.clickOBUTab();

        //Click the Create button.
        CreateOBUDeviceProfileModal createObuModal = obuTab.clickCreate();

        //With all fields blank, click the Create button in the Create OBU Device Profile modal.
        createObuModal.clickCreate(false);

        //Verify an error icon is displayed associated with the Name text box indicating a valid device profile name is required.
        Assert.assertTrue("Field required error not displayed as expected with Name text box when attempting to create OBU with all fields blank.",
                createObuModal.isOBUNameFieldRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the Rule Percentage text box indicating a valid OBU device profile percentage is required.
        Assert.assertTrue("Field required error not displayed as expected with Percentage text box when attempting to create OBU with all fields blank.",
                createObuModal.isOBUPercentageFieldRequiredErrorDisplayed());

        //Enter one or more space characters in the Name text box and enter valid values in the remaining fields.
        createObuModal.setAllFields(obu);
        createObuModal.setName("   ");

        //Click the Create button.
        createObuModal.clickCreate(false);

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error is not displayed when whitepace only is entered in Name text box.", createObuModal.isLeadingTrailingWhitespaceNameErrorDisplayed());

        //Enter a value in the Name text box that contains special characters, not including dashes (e.g., ‘T3$t’).
        createObuModal.setName(RandomStringGenerator.nextLetterString(5) + "$*@(");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid OBU name is not displayed when Name text box contains special characters.", createObuModal.isInvalidOBUNameErrorDisplayed());

        //Enter a value in the Name text box that contains consecutive spaces within the entered text (e.g., ‘Te    st’).
        createObuModal.setName(RandomStringGenerator.nextLetterString(5) + "  " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid OBU name is not displayed when Name text box contains consecutive spaces.", createObuModal.isInvalidOBUNameErrorDisplayed());

        //Enter a value in the Name text box that begins with a space (e.g., ‘ Test’).
        createObuModal.setName(" " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error is not displayed when a leading space is used in Name text box.", createObuModal.isLeadingTrailingWhitespaceNameErrorDisplayed());

        //Enter a value in the Name text box that ends with a space (e.g., ‘Test ’).
        createObuModal.setName(RandomStringGenerator.nextLetterString(5) + " ");

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error is not displayed when a trailing space is used in Name text box.", createObuModal.isLeadingTrailingWhitespaceNameErrorDisplayed());

        //Enter a value in the Name text box that begins with a hyphen/dash (e.g., ‘-Test’).
        createObuModal.setName("-" + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid OBU name is not displayed when Name text box contains a leading dash ('-') character.", createObuModal.isInvalidOBUNameErrorDisplayed());

        //Enter a value in the Name text box that ends with a hyphen/dash (e.g., ‘Test-’).
        createObuModal.setName(RandomStringGenerator.nextLetterString(5) + "-");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid OBU name is not displayed when Name text box contains a trailing dash ('-') character.", createObuModal.isInvalidOBUNameErrorDisplayed());

        //Enter the same name as an existing device (profile) in the Name text box.
        createObuModal.setName(anotherTestOBU);

        //Verify an error is displayed associated with the Name text box indicating device names must be unique.
        Assert.assertTrue("Duplicate device name error is not displayed when Name text box contains the same name as another existing device.",
                createObuModal.isDuplicateOBUNameErrorDisplayed(anotherTestOBU));

        //Enter a valid name in the Name text box.
        createObuModal.setName(obu);

        //Enter a non-numerical value in the Percentage text box (e.g., ‘---’).
        createObuModal.setPercentage("---");

        //Verify an error is displayed associated with the Percentage text box indicating the value entered is not a valid number.
        Assert.assertTrue("Non-numeric error is not displayed when Percentage text box contains a non-numeric value.", createObuModal.isInvalidNonNumericRulePercentageErrorDisplayed());

        //Enter a value less than 1 in the Percentage text box.
        createObuModal.setPercentage(Integer.toString(RandomNumberGenerator.nextInteger(1)));

        //Verify an error is displayed associated with the Percentage text box indicating the minimum acceptable value is 1.
        Assert.assertTrue("Invalid range error is not displayed when Percentage text box contains a value less than 1.", createObuModal.isInvalidPercentageRangeErrorDisplayed());

        //Enter a value greater than 100 in the Percentage text box.
        createObuModal.setPercentage(Integer.toString(RandomNumberGenerator.nextInteger(1000) + 100));

        //Verify an error is displayed associated with the Percentage text box indicating the maximum acceptable value is 100.
        Assert.assertTrue("Invalid range error is not displayed when Percentage text box contains a value greater than 100.", createObuModal.isInvalidPercentageRangeErrorDisplayed());

        //Enter a valid numerical value between 1 and 100 in the Percentage text box.
        createObuModal.setPercentage(Integer.toString(RandomNumberGenerator.nextInteger(20)));

        //Click the Create button.
        createObuModal.clickCreate(true);

        //Verify the OBU device is listed in the OBU Device Profiles list.
        Assert.assertTrue("New OBU Device Profile is not displayed in OBU Device Profiles table despite successful creation.", obuTab.isOBUDisplayed(obu));

        //Click the Create button.
        obuTab.clickCreate();

        //Enter a valid name in the Name text box.
        createObuModal.setName(obu);

        //Enter a value between 1 and 100 in the Percentage text box that will result in the combined total percentage of all cellular devices to exceed 100%.
        createObuModal.setPercentage("80");

        //Verify an error is displayed associated with the Percentage text box indicating the percentage total must not exceed 100%.
        Assert.assertTrue("Max total percentage exceeded error not displayed when total percentage for all OBU devices exceeds 100%.", createObuModal.isMaxTotalPercentageExceededErrorDisplayed());

        //Click the Cancel button.
        createObuModal.clickCancel();

        //Click the Close button.
        obuTab.clickCloseBtn();

        //Logout
        simPage.logout(testUser);
    }
}
