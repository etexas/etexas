package com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests;

import java.util.ArrayList;
import java.util.List;

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
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.EditOBUDeviceProfileModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.ConfigureOBUDeviceProfilesPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.ConfigureOBUDeviceProfilesPartialPage.OBUDeviceTableColumn;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomNumberGenerator;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class which executes the steps of the Edit an On Board Unit Device
 * Profile Test, TC-099/ITC-081
 *
 * @author llaroussini
 */
public class EditOBUDeviceProfileTest extends ETexasAfterTestResetTestBase {

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
     * Additional OBU device object used in the internal test case
     */
    private OBUDevice anotherObu;

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
        anotherObu = OBUDeviceFactory.getOBUDevice(true); //gets a random OBU device
        anotherObu.setOBUPercent("25"); //Known percentage set to ensure total percentage of devices can be consistently set to either less than or greater than 100%
        List<OBUDevice> devices = new ArrayList<OBUDevice>(2);
        devices.add(obu);
        devices.add(anotherObu);
        ETexasEntityManager.addEntities(testUser, simulation, composite, obu, anotherObu);

        //Register user and create new simulation from template with multiple OBU devices
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulationWithOBUDevices(simulation, devices);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-099
     */
    @Test
    public void editOBUExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select a composite that has no executions and at least one OBU device profile.
        simPage.selectComposite(composite, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Composite Settings option.
        CompositeSettingsModal compositeSettingsModal = simPage.clickCompositeSettings();

        //Verify the Composite Settings modal is displayed.
        Assert.assertTrue("The Composite Settings Modal is not displayed after clicking Composite Settings in the Edit menu.", compositeSettingsModal.isCompositeSettingsHeaderDisplayed());

        //Click the OBU Device Profiles tab.
        ConfigureOBUDeviceProfilesPartialPage obuTab = compositeSettingsModal.clickOBUTab();

        //Verify the OBU device profile associated with the selected composite is displayed.
        Assert.assertTrue("The OBU named: " + obu.getName() + " associated with the selected composite is not displayed as expected.", obuTab.isOBUDisplayed(obu));

        //Select the OBU device profile.
        obuTab.selectRow(obu.getName(), true);

        //Verify the Edit button is enabled.
        Assert.assertTrue("The Edit button is not enabled as expected after selecting an OBU.", obuTab.isOBUEditBtnEnabled());

        //Click the Edit button.
        EditOBUDeviceProfileModal editOBUModal = obuTab.clickEdit();

        //Verify an Edit OBU Device Profile modal is displayed.
        Assert.assertTrue("The Edit OBU Device Profile modal is not displayed as expected after clicking Edit.", editOBUModal.isEditOBUHeaderDisplayed());

        //Verify the following icons are displayed in the top right corner of the modal: ‘?’ and ‘x’.
        editOBUModal.checkEditOBUHeaderIcons();

        //Click the ‘?’ icon.
        editOBUModal.clickEditOBUHelp();

        //Verify an Edit OBU Device Profile Help modal is displayed with details associated to editing an OBU device profile.
        editOBUModal.checkEditOBUHelpModal();

        //Verify an OK button is displayed.
        Assert.assertTrue("OK button not displayed as expected in the Edit OBU Device Profile Help modal.", editOBUModal.isEditOBUHelpOKBtnDisplayed());

        //Click the OK button.
        editOBUModal.clickEditOBUHelpOKBtn();

        //Verify the Help modal is no longer displayed.
        Assert.assertFalse("The Edit OBU Device Profile Help modal is still displayed after clicking OK.", editOBUModal.isEditOBUHelpHeaderDisplayed());

        //Verify the following text boxes are displayed in the Edit OBU Device Profile modal: Name and Percentage.
        editOBUModal.checkFields();

        //Verify all fields are populated with the selected OBU device profile’s data.
        editOBUModal.checkFieldValuesDisplayed(obu);

        //Verify the following buttons are displayed at the bottom of the Edit OBU Device Profile modal: Update, Reset, and Cancel.
        editOBUModal.checkEditOBUBtns();

        //Updated values to set for OBU device
        OBUDevice updatedOBU = OBUDeviceFactory.getOBUDevice(true);
        String newName = updatedOBU.getName();
        String newPercent = "20"; //set to a known value to ensure max total percentage of 100% is not exceeded

        //Make a change to the Name value.
        editOBUModal.setName(newName);

        //Make a change to the Percentage value.
        editOBUModal.setPercentage(newPercent);

        //Click the Reset button.
        editOBUModal.clickReset();

        //Verify all values are reset to their original values.
        editOBUModal.checkFieldValuesDisplayed(obu);

        //Make a change to the Name value.
        editOBUModal.setName(newName);

        //Make a change to the Percentage value.
        editOBUModal.setPercentage(newPercent);

        //Click the Cancel button.
        editOBUModal.clickCancel();

        //Verify the Edit OBU Device Profile modal is no longer displayed.
        Assert.assertFalse("The Edit OBU Device Profile modal is still displayed after clicking Cancel.", editOBUModal.isEditOBUHeaderDisplayed());

        //Verify the OBU device profile is unchanged in the OBU Device Profiles table.
        Assert.assertTrue("Original OBU Device is not displayed in list after cancelling.", obuTab.isOBUDisplayed(obu));

        //Verify the OBU device profile is still selected, if not, select the OBU device profile.
        if (obuTab.isOBURowSelected(obu) == false) {
            obuTab.selectRow(obu.getName(), true);
        }

        //Click the Edit button.
        obuTab.clickEdit();

        //Verify the Edit OBU Device Profile modal is displayed.
        Assert.assertTrue("The Edit OBU Device Profile modal is not displayed as expected after clicking Edit.", editOBUModal.isEditOBUHeaderDisplayed());

        //Make a change to the Name value.
        editOBUModal.setName(newName);

        //Make a change to the Percentage value.
        editOBUModal.setPercentage(newPercent);

        //Click the ‘x’ icon.
        editOBUModal.clickCloseIcon();

        //Verify the Edit OBU Device Profile is no longer displayed.
        Assert.assertFalse("The Edit OBU Device Profile modal is still displayed after clicking Close.", editOBUModal.isEditOBUHeaderDisplayed());

        //Verify the OBU device profile is unchanged in the OBU Device Profiles table.
        Assert.assertTrue("Original OBU Device is not displayed in list after closing the edit modal before updating.", obuTab.isOBUDisplayed(obu));

        //Verify the OBU device profile is still selected, if not, select the OBU device profile.
        if (obuTab.isOBURowSelected(obu) == false) {
            obuTab.selectRow(obu.getName(), true);
        }

        //Click the Edit button.
        obuTab.clickEdit();

        //Verify the Edit OBU Device Profile modal is displayed.
        Assert.assertTrue("The Edit OBU Device Profile modal is not displayed as expected after clicking Edit.", editOBUModal.isEditOBUHeaderDisplayed());

        //Make a change to the Name value.
        editOBUModal.setName(newName);

        //Make a change to the Percentage value.
        editOBUModal.setPercentage(newPercent);

        //Click the Update button.
        editOBUModal.clickUpdate(true);

        //Verify the Edit OBU Device Profile modal is no longer displayed.
        Assert.assertFalse("The Edit OBU Device Profile modal is still displayed after clicking Update.", editOBUModal.isEditOBUHeaderDisplayed());

        //Verify the OBU device profile is updated in the OBU Device Profiles table and all updated values are displayed in the associated columns.
        Assert.assertFalse("Original OBU Device is still displayed after successfully editing/updating.", obuTab.isOBUDisplayed(obu));
        Assert.assertTrue("Updated OBU Device is not displayed after successfully editing/updating.", obuTab.isOBUDisplayed(newName));

        //Verify the ID of the OBU device profile remains unchanged.
        Assert.assertEquals("The ID of the updated OBU did not remain static.", obu.getID(), obuTab.getOBUCellValue(updatedOBU, OBUDeviceTableColumn.ID));

        //Update entity manager
        OBUDevice device = ETexasEntityManager.getOBUDevice(obu.getName());
        device.setOBUName(newName);
        device.setOBUPercent(newPercent);

        //Log out
        simPage.logout(testUser);
    }

    /**
     * Test steps for ITC-081
     */
    @Test
    public void editOBUInternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select a composite with at least one OBU device profile.
        simPage.selectComposite(composite, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Composite Settings option.
        CompositeSettingsModal compositeSettingsModal = simPage.clickCompositeSettings();

        //Click the OBU Device Profiles tab.
        ConfigureOBUDeviceProfilesPartialPage obuTab = compositeSettingsModal.clickOBUTab();

        //Select a OBU device.
        obuTab.selectRow(obu.getName(), true);

        //Click the Edit button.
        EditOBUDeviceProfileModal editModal = obuTab.clickEdit();

        //Delete the pre-populated values in all fields.
        editModal.setAllFields("", "");

        //With all fields blank, click the Update button in the Create OBU Device Profile modal.
        editModal.clickUpdate(false);

        //Verify an error icon is displayed associated with the Name text box indicating a valid device profile name is required.
        Assert.assertTrue("Field required error not displayed as expected with Name text box when attempting to create OBU with all fields blank.", editModal.isOBUNameFieldRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the Percentage text box indicating a valid OBU device profile percentage is required.
        Assert.assertTrue("Field required error not displayed as expected with Percentage text box when attempting to create OBU with all fields blank.",
                editModal.isOBUPercentageFieldRequiredErrorDisplayed());

        //Enter one or more space characters in the Name text box and enter valid values in the remaining fields.
        editModal.setAllFields(obu);
        editModal.setName("   ");

        //Click the Update button.
        editModal.clickUpdate(false);

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error is not displayed when whitepace only is entered in Name text box.", editModal.isLeadingTrailingWhitespaceNameErrorDisplayed());

        //Enter a value in the Name text box that contains special characters, not including dashes (e.g., ‘T3$t’).
        editModal.setName(RandomStringGenerator.nextLetterString(5) + "$*@(");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid OBU name is not displayed when Name text box contains special characters.", editModal.isInvalidOBUNameErrorDisplayed());

        //Enter a value in the Name text box that contains consecutive spaces within the entered text (e.g., ‘Te    st’).
        editModal.setName(RandomStringGenerator.nextLetterString(5) + "  " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid OBU name is not displayed when Name text box contains consecutive spaces.", editModal.isInvalidOBUNameErrorDisplayed());

        //Enter a value in the Name text box that begins with a space (e.g., ‘ Test’).
        editModal.setName(" " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error is not displayed when a leading space is used in Name text box.", editModal.isLeadingTrailingWhitespaceNameErrorDisplayed());

        //Enter a value in the Name text box that ends with a space (e.g., ‘Test ’).
        editModal.setName(RandomStringGenerator.nextLetterString(5) + " ");

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error is not displayed when a trailing space is used in Name text box.", editModal.isLeadingTrailingWhitespaceNameErrorDisplayed());

        //Enter a value in the Name text box that begins with a hyphen/dash (e.g., ‘-Test’).
        editModal.setName("-" + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid OBU name is not displayed when Name text box contains a leading dash ('-') character.", editModal.isInvalidOBUNameErrorDisplayed());

        //Enter a value in the Name text box that ends with a hyphen/dash (e.g., ‘Test-’).
        editModal.setName(RandomStringGenerator.nextLetterString(5) + "-");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid OBU name is not displayed when Name text box contains a trailing dash ('-') character.", editModal.isInvalidOBUNameErrorDisplayed());

        //Enter the same name as an existing device (profile) in the Name text box.
        editModal.setName(anotherObu);

        //Verify an error is displayed associated with the Name text box indicating device names must be unique.
        Assert.assertTrue("Duplicate device name error is not displayed when Name text box contains the same name as another existing device.", editModal.isDuplicateOBUNameErrorDisplayed(anotherObu));

        //Enter a valid name in the Name text box.
        editModal.setName(obu);

        //Enter a non-numerical value in the Percentage text box (e.g., ‘---’).
        editModal.setPercentage("---");

        //Verify an error is displayed associated with Rule Percentage text box indicating the value entered is not a valid number.
        Assert.assertTrue("Non-numeric error is not displayed when Percentage text box contains a non-numeric value.", editModal.isInvalidNonNumericRulePercentageErrorDisplayed());

        //Enter a value less than 1 in the Percentage text box.
        editModal.setPercentage(Integer.toString(RandomNumberGenerator.nextInteger(1)));

        //Verify an error is displayed associated with the Percentage text box indicating the minimum acceptable value is 1.
        Assert.assertTrue("Invalid range error is not displayed when Percentage text box contains a value less than 1.", editModal.isInvalidPercentageRangeErrorDisplayed());

        //Enter a value greater than 100 in the Percentage text box.
        editModal.setPercentage(Integer.toString(RandomNumberGenerator.nextInteger(1000) + 100));

        //Verify an error is displayed associated with the Percentage text box indicating the maximum acceptable value is 100.
        Assert.assertTrue("Invalid range error is not displayed when Percentage text box contains a value greater than 100.", editModal.isInvalidPercentageRangeErrorDisplayed());

        //Enter a valid numerical value between 1 and 100 in the Percentage text box.
        editModal.setPercentage(Integer.toString(RandomNumberGenerator.nextInteger(15)));

        //Click the Update button.
        editModal.clickUpdate(true);

        //Verify the OBU device has been updated in the OBU Device Profiles list.
        Assert.assertTrue("Updated OBU Device Profile is not displayed in OBU Device Profiles table despite successful update.", obuTab.isOBUDisplayed(obu));

        //Click the Edit button.
        obuTab.clickEdit();

        //Enter a valid name in the Name text box.
        editModal.setName(obu);

        //Enter a value between 1 and 100 in the Percentage text box that will result in the combined total percentage of all cellular devices to exceed 100%.
        editModal.setPercentage(Integer.toString(RandomNumberGenerator.nextInteger(20) + 80));

        //Verify an error is displayed associated with the Percentage text box indicating the percentage total must not exceed 100%.
        Assert.assertTrue("Max total percentage exceeded error not displayed when total percentage for all OBU devices exceeds 100%.", editModal.isMaxTotalPercentageExceededErrorDisplayed());

        //Click the Cancel button.
        editModal.clickCancel();

        //Click the Close button.
        obuTab.clickCloseBtn();

        //Log Out.
        simPage.logout(testUser);

    }
}