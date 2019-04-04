package com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests;

import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.RSEDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.RSEDeviceFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.ConfigureRSEDevicesPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.EditRSEModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class which executes steps for the Edit a Road Side Equipment Device
 * Test TC-096/ITC-075
 *
 * @author llaroussini
 */
public class EditRSEDeviceTest extends ETexasAfterTestResetTestBase {

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
     * RSE Device object used throughout the test case
     */
    private RSEDevice rse;

    /**
     * The name of the RSE device used in the test case
     */
    private String rseName;

    /**
     * Another RSE Device object used in the internal test case
     */
    private RSEDevice anotherRSE;

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get test user, test simulation, and test RSE device
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        simulation = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        simulation.setUser(testUser);
        composite = simulation.getComposite();
        rse = RSEDeviceFactory.getRSEDevice(true); //get a random RSE device
        rseName = rse.getName();
        anotherRSE = RSEDeviceFactory.getRSEDevice(true); //get a random RSE device
        List<RSEDevice> devices = new ArrayList<RSEDevice>(2);
        devices.add(rse);
        devices.add(anotherRSE);
        simulation.setRSEDevices(devices);
        ETexasEntityManager.addEntities(testUser, simulation, composite, rse);

        //Register user and create new simulation from template with RSE device
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulationWithRSEDevices(simulation, devices);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-096
     */
    @Test
    public void editRSEExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select simulation
        simPage.expandComposite(composite, true);
        simPage.selectSim(simulation, true);

        //Click Edit
        simPage.clickEdit();

        //Click the Simulation Settings option and verify the Simulation Settings modal is displayed
        SimulationSettingsModal simSettings = simPage.clickSimulationSettings();

        //Click the RSE Devices tab.
        ConfigureRSEDevicesPartialPage rseTab = simSettings.clickRSEDevicesTab();

        //Verify the RSE device associated with the selected simulation is displayed.
        Assert.assertTrue("The RSE with name: " + rseName + " could not be found.", rseTab.isRSEDisplayed(rse));

        //Select the RSE device.
        rseTab.selectRow(rseName, true);

        //Verify the Edit button is enabled.
        Assert.assertTrue("The Edit button is not enabled in the RSE tab when a device is selected.", rseTab.isRSEEditBtnEnabled());

        //Click the Edit button and verify an Edit RSE Device modal is displayed.
        EditRSEModal editModal = rseTab.clickEdit();

        //Verify the following icons are displayed in the top right corner of the modal: ‘?’ and ‘x’.
        editModal.checkEditRSEHeaderIcons();

        //Click the ‘?’ icon.
        editModal.clickEditRSEHelp();

        //Verify an Edit RSE Device Help modal is displayed with details associated to editing a road side equipment device.
        editModal.checkEditRSEHelpModal();

        //Verify an OK button is displayed.
        Assert.assertTrue("OK button could not be found in Edit RSE Help modal.", editModal.isEditRSEHelpOKBtnDisplayed());

        //Click the OK button.
        editModal.clickEditRSEHelpOKBtn();

        //Verify the Help modal is no longer displayed.
        Assert.assertFalse("Edit RSE Help modal still displayed after clicking OK.", editModal.isEditRSEHelpHeaderDisplayed());

        //Verify the following text boxes are displayed in the Edit RSE Device modal: Name, X (cm), Y (cm), and Z (cm).
        editModal.checkFieldsDisplayed();

        //Verify all fields are populated with the selected device’s data.
        editModal.checkFieldValues(rse);

        //Verify the following buttons are displayed at the bottom of the Edit RSE Device modal: Update, Reset, and Cancel.
        editModal.checkEditRSEBtns();

        //Updated values to set for RSE device
        RSEDevice newRSE = RSEDeviceFactory.getRSEDevice(true);
        String newName = newRSE.getName();
        String newX = newRSE.getXCoordinate();
        String newY = newRSE.getYCoordinate();
        String newZ = newRSE.getZCoordinate();

        //Make a change to the name value.
        editModal.setName(newName);

        //Make a change to the X (cm) value.
        editModal.setXCoordinate(newX);

        //Make a change to the Y (cm) value.
        editModal.setYCoordinate(newY);

        //Make a change to the Z (cm) value.
        editModal.setZCoordinate(newZ);

        //Click the Reset button.
        editModal.clickReset();

        //Verify all values are reset to their original values.
        editModal.checkFieldValues(rse);

        //Make a change to the name value.
        editModal.setName(newName);

        //Make a change to the X (cm) value.
        editModal.setXCoordinate(newX);

        //Make a change to the Y (cm) value.
        editModal.setYCoordinate(newY);

        //Make a change to the Z (cm) value.
        editModal.setZCoordinate(newZ);

        //Click the Cancel button.
        editModal.clickCancel();

        //Verify the Edit RSE Device modal is no longer displayed.
        Assert.assertFalse("Edit RSE Device modal is still displayed after clicking Cancel.", editModal.isEditRSEHeaderDisplayed());

        //Verify the device is unchanged in the RSE Devices table.
        Assert.assertTrue("Original RSE Device is not displayed in list after update is cancelled.", rseTab.isRSEDisplayed(rse));

        //Verify the device is still selected, if not, select the device.
        if (!rseTab.isRSERowSelected(rse)) {
            rseTab.selectRow(rseName, true);
        }

        //Click the Edit button and verify the Edit RSE Device modal is displayed.
        rseTab.clickEdit();

        //Make a change to the name value.
        editModal.setName(newName);

        //Make a change to the X (cm) value.
        editModal.setXCoordinate(newX);

        //Make a change to the Y (cm) value.
        editModal.setYCoordinate(newY);

        //Make a change to the Z (cm) value.
        editModal.setZCoordinate(newZ);

        //Click the ‘x’ icon.
        editModal.clickCloseIcon();

        //Verify the Edit RSE Device modal is no longer displayed.
        Assert.assertFalse("Edit RSE Device modal is still displayed after clicking Close.", editModal.isEditRSEHeaderDisplayed());

        //Verify the device is unchanged in the RSE Devices table.
        Assert.assertTrue("Original RSE Device is not displayed in list after edit modal is closed.", rseTab.isRSEDisplayed(rse));

        //Verify the device is still selected, if not, select the device.
        if (rseTab.isRSERowSelected(rse) == false) {
            rseTab.selectRow(rseName, true);
        }

        //Click the Edit button and verify the Edit RSE Device modal is displayed.
        rseTab.clickEdit();

        //Make a change to the name value.
        editModal.setName(newName);

        //Make a change to the X (cm) value.
        editModal.setXCoordinate(newX);

        //Make a change to the Y (cm) value.
        editModal.setYCoordinate(newY);

        //Make a change to the Z (cm) value.
        editModal.setZCoordinate(newZ);

        //Click the Update button.
        editModal.clickUpdate(true);

        //Update entity manager
        rse = ETexasEntityManager.getRSEDevice(rseName);
        rse.setName(newName);
        rse.setXCoordinate(newX);
        rse.setYCoordinate(newY);
        rse.setZCoordinate(newZ);

        //Verify the Edit RSE Device modal is no longer displayed.
        Assert.assertFalse("Edit RSE Device modal is still displayed after clicking Update.", editModal.isEditRSEHeaderDisplayed());

        //Verify the device is updated in the RSE Devices table and all updated values are displayed in the associated columns.
        Assert.assertTrue("Updated RSE Device is not displayed in list after update.", rseTab.isRSEDisplayed(rse));

        //Verify the ID of the device remains unchanged.
        Assert.assertEquals("The ID of the updated RSE did not remain static.", rse.getID(), rseTab.getRSEID(newRSE));

        //Log out
        simPage.logout(testUser);
    }

    /**
     * Test steps for ITC-075
     */
    @Test
    public void editRSEInternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select simulation
        simPage.expandComposite(composite, true);
        simPage.selectSim(simulation, true);

        //Click Edit
        simPage.clickEdit();

        //Click the Simulation Settings option
        SimulationSettingsModal simSettings = simPage.clickSimulationSettings();

        //Click the RSE Devices tab.
        ConfigureRSEDevicesPartialPage rseTab = simSettings.clickRSEDevicesTab();

        //Select any RSE device.
        rseTab.selectRow(rseName, true);

        //Click the Edit button
        EditRSEModal editModal = rseTab.clickEdit();

        //Delete the pre-populated values in all fields.
        editModal.setAllFields("", "", "", "");

        //Click the Update button.
        editModal.clickUpdate(false);

        //Verify an error icon is displayed associated with the Name text box indicating a valid name is required.
        Assert.assertTrue("Name required error is not displayed as expected when RSE is attempted to be created with all fields blank.", editModal.isNameRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the X Coordinate text box indicating a valid x coordinate is required.
        Assert.assertTrue("X Coordinate required error is not displayed as expected when RSE is attempted to be created with all fields blank.", editModal.isXCoordinateRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the Y Coordinate text box indicating a valid y coordinate is required.
        Assert.assertTrue("Y Coordinate required error is not displayed as expected when RSE is attempted to be created with all fields blank.", editModal.isYCoordinateRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the Z Coordinate text box indicating a valid z coordinate is required.
        Assert.assertTrue("Z Coordinate required error is not displayed as expected when RSE is attempted to be created with all fields blank.", editModal.isZCoordinateRequiredErrorDisplayed());

        //Enter one or more space characters in the Name text box and enter valid values in the remaining fields.
        editModal.setAllFields(rse);
        editModal.setName("   ");

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error is not displayed when whitespace only is entered in Name text box.", editModal.isRSENameWhitespaceErrorDisplayed());

        //Enter a value in the Name text box that contains special characters, not including dashes (e.g., ‘T3$t’).
        editModal.setName(RandomStringGenerator.nextLetterString(5) + "&*$&@");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid name error is not displayed when special characters are entered in Name text box.", editModal.isInvalidRSENameErrorDisplayed());

        //Enter a value in the Name text box that contains consecutive spaces within the entered text (e.g., ‘Te    st’).
        editModal.setName(RandomStringGenerator.nextLetterString(5) + "  " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid name error is not displayed when consecutive spaces are entered in Name text box.", editModal.isInvalidRSENameErrorDisplayed());

        //Enter a value in the Name text box that begins with a space (e.g., ‘ Test’).
        editModal.setName(" " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error is not displayed when leading whitespace is entered in Name text box.", editModal.isRSENameWhitespaceErrorDisplayed());

        //Enter a value in the Name text box that ends with a space (e.g., ‘Test ’).
        editModal.setName(RandomStringGenerator.nextLetterString(5) + " ");

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error is not displayed when trailing whitespace is entered in Name text box.", editModal.isRSENameWhitespaceErrorDisplayed());

        //Enter a value in the Name text box that begins with a hyphen/dash (e.g., ‘-Test’).
        editModal.setName("-" + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid name error is not displayed when a leading dash ('-') character is used in Name text box.", editModal.isInvalidRSENameErrorDisplayed());

        //Enter a value in the Name text box that ends with a hyphen/dash (e.g., ‘Test-’).
        editModal.setName(RandomStringGenerator.nextLetterString(5) + "-");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid name error is not displayed when a trailing dash ('-') character is used in Name text box.", editModal.isInvalidRSENameErrorDisplayed());

        //Enter the same name as an existing RSE device in the Name text box.
        editModal.setName(anotherRSE);

        //Verify an error is displayed associated with the Name text box indicating device names must be unique.
        Assert.assertTrue("Duplicate device name error not displayed when duplicate name: " + anotherRSE.getName() + " is entered in Name text box.",
                editModal.isDuplicateRSENameErrorDisplayed(anotherRSE));

        //Enter a valid name in the Name text box.
        RSEDevice updatedRSE = RSEDeviceFactory.getRSEDevice(true);
        editModal.setName(updatedRSE.getName());

        //Enter a non-numerical value in the X Coordinate text box (e.g., ‘---’).
        editModal.setXCoordinate("---");

        //Verify an error is displayed associated with the X Coordinate text box indicating the value entered is not a valid number.
        Assert.assertTrue("Invalid, non-numerical, error not displayed when a non-numerical value is entered in X Coordinate text box.", editModal.isInvalidXCoordinateErrorDisplayed());

        //Enter a valid value in the X Coordinate text box.
        editModal.setXCoordinate(updatedRSE.getXCoordinate());

        //Enter a non-numerical value in the Y Coordinate text box (e.g., ‘---’).
        editModal.setYCoordinate("---");

        //Verify an error is displayed associated with the Y Coordinate text box indicating the value entered is not a valid number.
        Assert.assertTrue("Invalid, non-numerical, error not displayed when a non-numerical value is entered in Y Coordinate text box.", editModal.isInvalidYCoordinateErrorDisplayed());

        //Enter a valid value in the Y Coordinate text box.
        editModal.setYCoordinate(updatedRSE.getYCoordinate());

        //Enter a non-numerical value in the Z Coordinate text box (e.g., ‘---’).
        editModal.setZCoordinate("---");

        //Verify an error is displayed associated with the Z Coordinate text box indicating the value entered is not a valid number.
        Assert.assertTrue("Invalid, non-numerical, error not displayed when a non-numerical value is entered in Z Coordinate text box.", editModal.isInvalidZCoordinateErrorDisplayed());

        //Enter a valid value in the Z Coordinate text box.
        editModal.setZCoordinate(updatedRSE.getZCoordinate());

        //Click the Update button.
        editModal.clickUpdate(true);

        //Update entity manager
        rse = ETexasEntityManager.getRSEDevice(rseName);
        rse.setName(updatedRSE.getName());
        rse.setXCoordinate(updatedRSE.getXCoordinate());
        rse.setYCoordinate(updatedRSE.getYCoordinate());
        rse.setZCoordinate(updatedRSE.getZCoordinate());

        //Verify the device is updated in the RSE Devices table and all updated values are displayed in the associated columns.
        Assert.assertTrue("Updated RSE Device is not displayed in list after update.", rseTab.isRSEDisplayed(rse));

        //Log out.
        simPage.logout(testUser);
    }
}
