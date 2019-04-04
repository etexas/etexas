package com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests;

import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.FixedCellularDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.FixedCellularDeviceFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.ConfigureFixedCellularDevicePartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.EditFixedCellularDeviceModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class which executes steps for the Edit Fixed Cellular Device test,
 * TC-079/ITC-060
 *
 * @author rsmith
 * @author llaroussini
 */
public class EditFixedCellularDeviceTest extends ETexasAfterTestResetTestBase {

    /**
     * Composite object used throughout the test case
     */
    private CompositeSimulation composite;

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Simulation object used throughout the test case
     */
    private TemplateSimulation testSim;

    /**
     * Fixed Cellular Device object used throughout the test case.
     */
    private FixedCellularDevice fixedCell;

    /**
     * Fixed cellular device name used throughout the test case
     */
    private String fixedCellName;

    /**
     * Another fixed Cellular Device object used in the internal test case.
     */
    private FixedCellularDevice anotherFixedCell;

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get test user, test simulation, and test fixed cellular device
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        testSim = SimulationFactory.getTemplateSimulation(testUser, true);
        testSim.setUser(testUser);
        composite = testSim.getComposite();
        fixedCell = FixedCellularDeviceFactory.getFixedCellularDevice(true);//get a random fixed Cellular device
        fixedCellName = fixedCell.getName();
        anotherFixedCell = FixedCellularDeviceFactory.getFixedCellularDevice(true);//get a random fixed Cellular device
        List<FixedCellularDevice> devices = new ArrayList<FixedCellularDevice>(2);
        devices.add(fixedCell);
        devices.add(anotherFixedCell);
        testSim.addFixedCellularDevice(fixedCell);
        testSim.addFixedCellularDevice(anotherFixedCell);
        ETexasEntityManager.addEntities(testUser, testSim, composite, fixedCell, anotherFixedCell);

        //Register user and create new simulation from template with Fixed Cellular
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulationWithFixedCellularDevices(testSim, devices);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-079
     */
    @Test
    public void editFixedCellularDeviceExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select simulation
        simPage.expandComposite(composite, true);
        simPage.selectSim(testSim, true);

        //Click the Edit button.
        simPage.clickEdit();
        simPage.checkEditOptions();

        //Click the Simulation Settings option and verify the Simulation Settings modal displays.
        SimulationSettingsModal simSettingsModal = simPage.clickSimulationSettings();
        Assert.assertTrue("Simulations Settings header is not displayed as expected.", simSettingsModal.isSimSettingsHeaderDisplayed());

        //Click the Fixed Cellular tab.
        ConfigureFixedCellularDevicePartialPage fixedCellDevicesPage = simSettingsModal.clickFixedCellularDevicesTab();

        //Verify the fixed cellular device associated with the selected simulation is displayed
        Assert.assertTrue("The Fixed Cell with name: " + fixedCellName + " could not be found.", fixedCellDevicesPage.isFixedCellularDeviceDisplayed(fixedCellName));

        //Select an existing fixed cellular device
        fixedCellDevicesPage.selectRow(fixedCellName, true);

        //Verify the Edit Button is enabled.
        Assert.assertTrue("The Edit button is not enabled in the Fixed Cellular Devices tab when a device is selected.", fixedCellDevicesPage.isFixedCellularEditBtnEnabled());

        //Click the Edit button.
        EditFixedCellularDeviceModal editFixedCellForm = fixedCellDevicesPage.clickEdit();

        //Verify the Edit Fixed Cellular Device Modal is displayed
        Assert.assertTrue("Edit Fixed Cellular Device header is not displayed as expected.", editFixedCellForm.isEditFixedCellularDeviceHeaderDisplayed());

        //Verify that a '?' icon and an 'x' icon are displayed in the upper right corner of the modal.
        editFixedCellForm.checkEditFixedCellularDeviceHeaderIcons();

        //Click the '?' icon.
        editFixedCellForm.clickEditFixedCellularDeviceHelp();

        //Verify that the Edit Fixed Cellular Help modal is displayed with details associated to editing a road side equipment device.
        editFixedCellForm.checkEdiFixedCellularDeviceHelpModal();

        //Verify an OK button is displayed in the modal and click the OK button.
        Assert.assertTrue("An OK button is not displayed in the Edit Fixed Cellular Device Help modal.", editFixedCellForm.isEditFixedCellularDeviceHelpOKBtnDisplayed());
        editFixedCellForm.clickEditFixedCellularDeviceHelpOKBtn();

        //Verify the Help modal closes.
        Assert.assertFalse("Edit Fixed Cellular Help header is still displayed after clicking OK to close.", editFixedCellForm.isEditFixedCellularDeviceHelpHeaderDisplayed());

        //Verify the following text boxes are displayed in the Fixed Cellular modal: Name, MAC address, X (cm), Y (cm), Z(cm)
        editFixedCellForm.checkFieldsDisplayed();

        //Verify the fields are populated with the fixed cellular device's associated information.
        editFixedCellForm.checkFieldValues(fixedCell);

        //Verify the following buttons are displayed at the bottom of the window: Update, Reset, and Cancel.
        editFixedCellForm.checkEditCellularDeviceBtns();

        //Change the values in all fields
        FixedCellularDevice newFixedCell = FixedCellularDeviceFactory.getFixedCellularDevice(true);
        editFixedCellForm.setAllFields(newFixedCell);
        String newName = newFixedCell.getName();
        String newMacAddress = newFixedCell.getMacAddress();
        String newX = newFixedCell.getXCoordinate();
        String newY = newFixedCell.getYCoordinate();
        String newZ = newFixedCell.getZCoordinate();

        //Click the Reset Button
        editFixedCellForm.clickReset();

        //Verify all values are reset to their default values
        editFixedCellForm.checkFieldValues(fixedCell);

        //Change the values in all fields
        editFixedCellForm.setAllFields(newFixedCell);

        //Click the Cancel button icon
        editFixedCellForm.clickCancel();

        //Verify the Edit Fixed Cellular Device modal is no longer displayed.
        Assert.assertFalse("Edit Fixed Cellular header is still displayed after clicking cancel.", editFixedCellForm.isEditFixedCellularDeviceHeaderDisplayed());

        //Verify the device is unchanged in the Fixed Cellular Devices table
        Assert.assertTrue("Original Fixed Cellular device is not displayed in list after selecting the cancel button.", fixedCellDevicesPage.isFixedCellularDeviceDisplayed(fixedCellName));

        //Verify the device is still selected, if not, select the device.
        if (fixedCellDevicesPage.isFixedCellularDeviceRowSelected(fixedCell) == false) {
            fixedCellDevicesPage.selectRow(fixedCellName, true);
        }

        //Click the Edit button again. Verify the Edit Fixed Cellular  Modal is displayed
        editFixedCellForm = fixedCellDevicesPage.clickEdit();
        Assert.assertFalse("Edit Fixed Cellular modal not displayed as expected.", editFixedCellForm.isCreateFixedCellularDeviceHeaderDisplayed());

        //Enter valid values in all text boxes.
        editFixedCellForm.setAllFields(newFixedCell);

        //Click the Close icon
        editFixedCellForm.clickCloseIcon();

        //Verify the Edit Fixed Cellular modal closes.
        Assert.assertFalse("Edit Fixed Cellular Modal is still displayed after closing.", editFixedCellForm.isCreateFixedCellularDeviceHeaderDisplayed());

        //Verify the Fixed Cellular modal is still displayed in the fixed cellular list.
        Assert.assertTrue("Original Fixed Cellular device is not displayed in list after edit modal is closed.", fixedCellDevicesPage.isFixedCellularDeviceDisplayed(fixedCellName));

        //Verify the device is still selected, if not, select the device
        if (fixedCellDevicesPage.isFixedCellularDeviceRowSelected(fixedCell) == false) {
            fixedCellDevicesPage.selectRow(fixedCellName, true);
        }
        //Click the Edit button again.
        editFixedCellForm = fixedCellDevicesPage.clickEdit();

        //Enter valid values in all text boxes.
        editFixedCellForm.setAllFields(newFixedCell);

        //Click the Update button.
        editFixedCellForm.clickUpdate(true);

        //Verify the Edit Detector modal is no longer displayed
        Assert.assertFalse("Edit Fixed Cellular Device Modal is still displayed after selecting update.", editFixedCellForm.isCreateFixedCellularDeviceHeaderDisplayed());

        //Verify the updated detector is displayed with the associated updated information in the detectors list.
        Assert.assertTrue("Updated Fixed Cell Device is not displayed in list after update.", fixedCellDevicesPage.isFixedCellularDeviceDisplayed(newName));

        //Verify the Name, MAC Address, X (cm), Y (cm), and Z (cm) values match the values entered previously.
        Assert.assertTrue("Name value is not displayed in list after being created.", fixedCellDevicesPage.isFixedCellularDeviceDisplayed(newName));
        Assert.assertTrue("MAC Address width value is not displayed in list after being created.", fixedCellDevicesPage.isFixedCellularDeviceDisplayed(newMacAddress));
        Assert.assertTrue("X Coordinate value is not displayed in list after being created.", fixedCellDevicesPage.isFixedCellularDeviceDisplayed(newX));
        Assert.assertTrue("Y Coordinate value is not displayed in list after being created.", fixedCellDevicesPage.isFixedCellularDeviceDisplayed(newY));
        Assert.assertTrue("Z Coordinate value is not displayed in list after being created.", fixedCellDevicesPage.isFixedCellularDeviceDisplayed(newZ));

        //Verify the detector ID is unchanged.
        Assert.assertEquals("The ID of the updated Fixed Cellular Device did not remain static.", fixedCell.getID(), fixedCellDevicesPage.getFixedCellularID(newFixedCell));

        //Update entity manager
        FixedCellularDevice originalFixedCellular = ETexasEntityManager.getFixedCellularDevice(fixedCellName);
        originalFixedCellular.setName(newName);
        originalFixedCellular.setMacAddress(newMacAddress);
        originalFixedCellular.setXCoordinate(newX);
        originalFixedCellular.setYCoordinate(newY);
        originalFixedCellular.setZCoordinate(newZ);

        //Log out
        simPage.logout(testUser);
    }


    /**
     * Test steps for ITC-060
     */
    @Test
    public void editFixedCellularDeviceInternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select simulation
        simPage.expandComposite(composite, true);
        simPage.selectSim(testSim, true);

        //Click the Edit button.
        simPage.clickEdit();
        simPage.checkEditOptions();

        //Click the Simulation Settings option.
        SimulationSettingsModal simSettingsModal = simPage.clickSimulationSettings();

        //Click the Fixed Cellular Devices tab.
        ConfigureFixedCellularDevicePartialPage fixedCellTab = simSettingsModal.clickFixedCellularDevicesTab();

        //Select any fixed cellular device.
        fixedCellTab.selectRow(fixedCellName, true);

        //Click the Edit button.
        EditFixedCellularDeviceModal editModal = fixedCellTab.clickEdit();

        //Delete the pre-populated values in all fields.
        editModal.setAllFields("", "", "", "", "");

        //Click the Update button.
        editModal.clickUpdate(false);

        //Verify an error icon is displayed associated with the Name text box indicating the valid name is required.
        Assert.assertTrue("Name required error not displayed when fixed cell is attempted to be created with all fields blank.", editModal.isNameRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the MAC Address text box indicating a valid MAC address is required.
        Assert.assertTrue("MAC Address required error not displayed when fixed cell is attempted to be created with all fields blank.", editModal.isMACAddressRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the X Coordinate text box indicating a valid x coordinate is required.
        Assert.assertTrue("X coordinate required error not displayed when fixed cell is attempted to be created with all fields blank.", editModal.isXCoordinateRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the Y Coordinate text box indicating a valid y coordinate is required.
        Assert.assertTrue("Y coordinate required error not displayed when fixed cell is attempted to be created with all fields blank.", editModal.isYCoordinateRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the Z Coordinate text box indicating a valid z coordinate is required.
        Assert.assertTrue("Z coordinate required error not displayed when fixed cell is attempted to be created with all fields blank.", editModal.isZCoordinateRequiredErrorDisplayed());

        //Enter one or more space characters in the Name text box and enter valid values in the remaining fields.
        editModal.setAllFields(fixedCell);
        editModal.setName("   ");

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error not displayed when whitespace only is entered in Name text box.", editModal.isLeadingTrailingWhitespaceNameErrorDisplayed());

        //Enter a value in the Name text box that contains special characters, not including dashes (e.g., ‘T3$t’).
        editModal.setName(RandomStringGenerator.nextLetterString(5) + "*()*#(@");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid name error is not displayed when special characters are used in Name text box.", editModal.isInvalidFixedCellNameErrorDisplayed());

        //Enter a value in the Name text box that contains consecutive spaces within the entered text (e.g., ‘Te    st’).
        editModal.setName(RandomStringGenerator.nextLetterString(5) + "  " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid name error is not displayed when consecutive spaces are used in Name text box.", editModal.isInvalidFixedCellNameErrorDisplayed());

        //Enter a value in the Name text box that begins with a space (e.g., ‘ Test’).
        editModal.setName(" " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error not displayed when leading whitespace is entered in Name text box.", editModal.isLeadingTrailingWhitespaceNameErrorDisplayed());

        //Enter a value in the Name text box that ends with a space (e.g., ‘Test ’).
        editModal.setName(RandomStringGenerator.nextLetterString(5) + " ");

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error not displayed when trailing whitespace is entered in Name text box.", editModal.isLeadingTrailingWhitespaceNameErrorDisplayed());

        //Enter a value in the Name text box that begins with a hyphen/dash (e.g., ‘-Test’).
        editModal.setName("-" + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid name error is not displayed when a leading dash ('-') character is used in Name text box.", editModal.isInvalidFixedCellNameErrorDisplayed());

        //Enter a value in the Name text box that ends with a hyphen/dash (e.g., ‘Test-’).
        editModal.setName(RandomStringGenerator.nextLetterString(5) + "-");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid name error is not displayed when a trailing dash ('-') character is used in Name text box.", editModal.isInvalidFixedCellNameErrorDisplayed());

        //Enter the same name as an existing fixed cellular device in the Name text box.
        editModal.setName(anotherFixedCell);

        //Verify an error is displayed associated with the Name text box indicating device names must be unique.
        Assert.assertTrue("Duplicate device name error not displayed when duplicate device name: " + anotherFixedCell.getName() + " is entered in Name text box.",
                editModal.isDuplicateDeviceNameErrorDisplayed(anotherFixedCell));

        //Get new values for all fields
        FixedCellularDevice newFixedCell = FixedCellularDeviceFactory.getFixedCellularDevice(true);
        String newName = newFixedCell.getName();
        String newMacAddress = newFixedCell.getMacAddress();
        String newX = newFixedCell.getXCoordinate();
        String newY = newFixedCell.getYCoordinate();
        String newZ = newFixedCell.getZCoordinate();

        //Enter a valid name in the Name text box.
        editModal.setName(newName);

        //Enter a non-numerical value in the MAC Address text box (e.g., ‘---’).
        editModal.setMacAddress("---");

        //Verify an error is displayed associated with the MAC Address text box indicating the value entered is not a valid number.
        Assert.assertTrue("Non-numeric error not displayed with MAC address text box when non-numeric value entered.", editModal.isNonNumericMACAddressErrorDisplayed());

        //Enter a MAC address value that is already in use by another fixed cell device in the MAC Address text box.
        editModal.setMacAddress(anotherFixedCell);

        //Verify an error is displayed associated with the MAC Address text box indicating the mac address value must be unique.
        Assert.assertTrue("Duplicate MAC address error not displayed when duplicate MAC address: " + anotherFixedCell.getMacAddress() + " from existing devce: " + anotherFixedCell.getName()
                + " is entered in MAC Address text box.", editModal.isDuplicateMACAddressErrorDisplayed(anotherFixedCell));

        //Enter a valid value in the MAC Address text box.
        editModal.setMacAddress(newMacAddress);

        //Enter a non-numerical value in the X Coordinate text box (e.g., ‘---’).
        editModal.setXCoordinate("---");

        //Verify an error is displayed associated with the X Coordinate text box indicating the value entered is not a valid number.
        Assert.assertTrue("Non-numeric error not displayed with X Coordinate text box when non-numeric value entered.", editModal.isNonNumericXCoordinateErrorDisplayed());

        //Enter a valid value in the X Coordinate text box.
        editModal.setXCoordinate(newX);

        //Enter a non-numerical value in the Y Coordinate text box (e.g., ‘---’).
        editModal.setYCoordinate("---");

        //Verify an error is displayed associated with the Y Coordinate text box indicating the value entered is not a valid number.
        Assert.assertTrue("Non-numeric error not displayed with Y Coordinate text box when non-numeric value entered.", editModal.isNonNumericYCoordinateErrorDisplayed());

        //Enter a valid value in the Y Coordinate text box.
        editModal.setYCoordinate(newY);

        //Enter a non-numerical value in the Z Coordinate text box (e.g., ‘---’).
        editModal.setZCoordinate("---");

        //Verify an error is displayed associated with the Z Coordinate text box indicating the value entered is not a valid number.
        Assert.assertTrue("Non-numeric error not displayed with Z Coordinate text box when non-numeric value entered.", editModal.isNonNumericZCoordinateErrorDisplayed());

        //Enter a valid value in the Z Coordinate text box.
        editModal.setZCoordinate(newZ);

        //Click the Update button.
        editModal.clickUpdate(true);

        //Verify the device is updated in the Fixed Cellular Devices table and all updated values are displayed in the associated columns.
        Assert.assertTrue("Updated Fixed Cell Device is not displayed in list after update.", fixedCellTab.isFixedCellularDeviceDisplayed(newName));

        //Update entity manager
        fixedCell = ETexasEntityManager.getFixedCellularDevice(fixedCellName);
        fixedCell.setName(newName);
        fixedCell.setMacAddress(newMacAddress);
        fixedCell.setXCoordinate(newX);
        fixedCell.setYCoordinate(newY);
        fixedCell.setZCoordinate(newZ);

        //Click Close
        fixedCellTab.clickClose();

        //Logout
        simPage.logout(testUser);
    }
}
