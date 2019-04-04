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
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.ConfigureFixedCellularDevicePartialPage.FixedCellularDeviceTableColumnHeader;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CreateFixedCellularDeviceModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class which executes steps for the Add Fixed Cell Device test,
 * TC-071/ITC-052
 *
 * @author llaroussini
 * @author rsmith
 */
public class AddFixedCellularDeviceTest extends ETexasAfterTestResetTestBase {

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
     * Fixed cellular device object used throughout the test case
     */
    private FixedCellularDevice testFixedCellDevice;

    /**
     * Another fixed cellular device object used in the internal test case
     */
    private FixedCellularDevice existingFixedCellDevice;

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get test user, test simulation, and test fixed cellular device
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        testSim = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        composite = testSim.getComposite();
        testFixedCellDevice = FixedCellularDeviceFactory.getFixedCellularDevice(true); //get a random fixed Cellular device
        testFixedCellDevice.setSimulation(testSim);
        existingFixedCellDevice = FixedCellularDeviceFactory.getFixedCellularDevice(true); //get a random fixed Cellular device
        existingFixedCellDevice.setSimulation(testSim);
        List<FixedCellularDevice> devices = new ArrayList<FixedCellularDevice>(1);
        devices.add(existingFixedCellDevice);
        testSim.setFixedCellularDevices(devices);
        ETexasEntityManager.addEntities(testUser, testSim, composite, testFixedCellDevice, existingFixedCellDevice);

        //Register user and create new simulation from template
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulationWithFixedCellularDevice(testSim, existingFixedCellDevice);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-071
     */
    @Test
    public void addFixedCellularDeviceExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Select a simulation within a composite that has no executions.
        SimulationsPage simPage = getPage(SimulationsPage.class);
        simPage.expandComposite(composite, true);
        simPage.selectSim(testSim, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Simulation Settings option.
        SimulationSettingsModal simSettingsModal = simPage.clickSimulationSettings();

        //Verify the Simulation Settings modal is displayed
        Assert.assertTrue("Simulations Settings header is not displayed as expected.", simSettingsModal.isSimSettingsHeaderDisplayed());

        //Click the Fixed Cellular tab.
        ConfigureFixedCellularDevicePartialPage fixedCellDevicesPage = simSettingsModal.clickFixedCellularDevicesTab();

        //Verify the following buttons are displayed across the top of the Cellular tab: Create, Edit, Delete, and Applications.
        fixedCellDevicesPage.checkBtns();

        //Verify the Create Button is enabled
        Assert.assertTrue("The Create button is not enabled as expected upon initial load of Add Fixed Cellular tab.", fixedCellDevicesPage.isFixedCellularCreateBtnEnabled());

        //Verify the following buttons are disabled: Edit, Delete, and Applications.
        Assert.assertFalse("The Edit button is not disabled as expected upon initial load of Fixed Cellular Devices tab.", fixedCellDevicesPage.isFixedCellularEditBtnEnabled());
        Assert.assertFalse("The Delete button is not disabled as expected upon initial load of Fixed Cellular Devices tab.", fixedCellDevicesPage.isFixedCellularDeleteBtnEnabled());
        Assert.assertFalse("The Applications button is not disabled as expected upon initial load of Fixed Cellular Devices tab.", fixedCellDevicesPage.isFixedCellularApplicationsBtnEnabled());

        //Verify a table is displayed with the following columns:  ID, Name, MAC Address, X (cm), Y (cm), and Z (cm).
        fixedCellDevicesPage.checkFixedCellularDeviceColumnHeaders();

        //Verify the table displays a list of Fixed Cellular devices configured in the selected simulation, if any exist.
        int fixedCellularListSize = testSim.getFixedCellularDevices().size();
        if (fixedCellularListSize == 0) {
            Assert.assertFalse("List of Fixed Cellular devices displayed despite simualtion not having any associated Fixed Cellular devices.", fixedCellDevicesPage.areFixedCellularDevicesDisplayed());
        }
        else {
            Assert.assertTrue("List of Fixed Cellular devices not displayed despite simulation having associated Fixed Cellular devices.", fixedCellDevicesPage.areFixedCellularDevicesDisplayed());
        }

        //Verify a Close button is displayed at the bottom of the modal.
        Assert.assertTrue("Close button could not be found.", simSettingsModal.isCloseBtnDisplayed());

        //Click the Create button.
        CreateFixedCellularDeviceModal addFixedCellularDevice = fixedCellDevicesPage.clickCreateFixedCellularDeviceBtn();

        //Verify a Create Fixed Cellular Device modal is displayed.
        Assert.assertTrue("Create Fixed Cellular Device header is not displayed as expected.", addFixedCellularDevice.isCreateFixedCellularDeviceHeaderDisplayed());

        //Verify the following icons are displayed in the top right corner of the modal: ‘?’ and ‘x’.
        addFixedCellularDevice.checkCreateFixedCellularHeaderIcons();

        //Click the ‘?’ icon.
        addFixedCellularDevice.clickHelpIcon();

        //Verify that a Create fixed cellular Device Help modal is displayed with instructions for creating fixed cell devices.
        addFixedCellularDevice.checkHelpModal();

        //Verify an OK button is displayed in the modal and click the OK button.
        Assert.assertTrue("OK button is not displayed.", addFixedCellularDevice.isHelpOKBtnDisplayed());
        addFixedCellularDevice.clickHelpOKBtn();

        //Verify the Help modal closes.
        Assert.assertFalse("Create Fixed Cellular Device Help header still displayed after clicking OK button.", addFixedCellularDevice.isCreateFixedCellularDeviceHelpHeaderDisplayed());

        //Verify the following text boxes are displayed in the Create Fixed Cellular Device modal: Name, X (cm), Y (cm), and Z (cm).
        addFixedCellularDevice.checkFieldsDisplayed();

        //Verify the following buttons are displayed at the bottom of the Create Fixed Cellular Device Device modal: Create, Reset, and Cancel.
        addFixedCellularDevice.checkBtns();

        //Enter a valid name in the Name text box (see Section 4 for valid values).
        addFixedCellularDevice.setName(testFixedCellDevice);

        //Enter a valid mac address in the MAC address text box (see section 4 for valid values).
        addFixedCellularDevice.setMacAddress(testFixedCellDevice);

        //Enter a valid value in the X (cm) text box (see Section 4 for valid values).
        addFixedCellularDevice.setXCoordinate(testFixedCellDevice);

        //Enter a valid value in the Y (cm) text box (see Section 4 for valid values).
        addFixedCellularDevice.setYCoordinate(testFixedCellDevice);

        //Enter a valid value in the Z (cm) text box (see Section 4 for valid values).
        addFixedCellularDevice.setZCoordinate(testFixedCellDevice);

        //Click the Reset button.
        addFixedCellularDevice.clickReset();

        //Verify all values are returned to their default state.
        addFixedCellularDevice.checkFieldValues("", "", "", "", "");

        //Enter valid values in all text boxes.
        addFixedCellularDevice.setAllFields(testFixedCellDevice);

        //Click the Cancel button.
        addFixedCellularDevice.clickCancel();

        //Verify the Create Fixed Cellular Device Device modal is no longer displayed.
        Assert.assertFalse("Add Fixed Cellular Device device form header is still displayed after clicking Cancel.", addFixedCellularDevice.isCreateFixedCellularDeviceHeaderDisplayed());

        //Verify the new Fixed Cellular Device device is not listed in the Fixed Cellular Device Devices table.
        String fixedCellName = testFixedCellDevice.getName();
        Assert.assertFalse("Fixed Cellular Device Device is displayed in list after being cancelled.", fixedCellDevicesPage.isFixedCellularDeviceDisplayed(fixedCellName));

        //Click the Create button.
        fixedCellDevicesPage.clickCreateFixedCellularDeviceBtn();

        //Enter valid values in all text boxes.
        addFixedCellularDevice.setAllFields(testFixedCellDevice);

        //Click the ‘x’ icon.
        addFixedCellularDevice.clickCloseIcon();

        //Verify the Create Fixed Cellular Device Device modal is no longer displayed.
        Assert.assertFalse("Add Fixed Cellular Device device form header is still displayed after clicking Close.", addFixedCellularDevice.isCreateFixedCellularDeviceHeaderDisplayed());

        //Verify the new Fixed Cellular Device device is not listed in the Fixed Cellular Device Devices table.
        Assert.assertFalse("Fixed Cellular Device Device is displayed in list after modal was closed.", fixedCellDevicesPage.isFixedCellularDeviceDisplayed(fixedCellName));

        //Click the Create button.
        fixedCellDevicesPage.clickCreateFixedCellularDeviceBtn();

        //Enter valid values in all text boxes.
        addFixedCellularDevice.setAllFields(testFixedCellDevice);

        //Click the Create button.
        addFixedCellularDevice.clickCreate(true);

        //Verify the Create Fixed Cellular Device Device modal is no longer displayed.
        Assert.assertFalse("Add Fixed Cellular Device device form header is still displayed after clicking Create.", addFixedCellularDevice.isCreateFixedCellularDeviceHeaderDisplayed());

        //Verify the new Fixed Cellular Device device is listed in the Fixed Cellular Device Devices table.
        Assert.assertTrue("Fixed Cellular Device is not displayed in list after being created.", fixedCellDevicesPage.isFixedCellularDeviceDisplayed(fixedCellName));

        //Verify the Name, X (cm), Y (cm), and Z (cm) columns are populated with the values entered previously.
        Assert.assertEquals("Fixed Cellular Device name value is not displayed in list after being created.", testFixedCellDevice.getName(),
                fixedCellDevicesPage.getFixedCellValue(testFixedCellDevice, FixedCellularDeviceTableColumnHeader.NAME));
        Assert.assertEquals("Fixed Cellular Device MAC Address is not displayed in list after being created.", testFixedCellDevice.getMacAddress(),
                fixedCellDevicesPage.getFixedCellValue(testFixedCellDevice, FixedCellularDeviceTableColumnHeader.MAC_ADDRESS));
        Assert.assertEquals("Fixed Cellular Device X Coordiante value is not displayed in list after being created.", String.valueOf(testFixedCellDevice.getXCoordinate()),
                fixedCellDevicesPage.getFixedCellValue(testFixedCellDevice, FixedCellularDeviceTableColumnHeader.X_COORDINATE));
        Assert.assertEquals("Fixed Cellular Device Y Coordiate value is not displayed in list after being created.", String.valueOf(testFixedCellDevice.getYCoordinate()),
                fixedCellDevicesPage.getFixedCellValue(testFixedCellDevice, FixedCellularDeviceTableColumnHeader.Y_COORDINATE));
        Assert.assertEquals("Fixed Cellular Device Z Coordiate value is not displayed in list after being created.", String.valueOf(testFixedCellDevice.getZCoordinate()),
                fixedCellDevicesPage.getFixedCellValue(testFixedCellDevice, FixedCellularDeviceTableColumnHeader.Z_COORDINATE));

        //Verify the newly created Fixed Cellular Device device has an auto-generated ID displayed in the ID column.
        Assert.assertNotNull("An ID was not generated for the newly created Fixed Cellular Device device.", fixedCellDevicesPage.getFixedCellularID(testFixedCellDevice));

        //Click the Close button.
        fixedCellDevicesPage.clickClose();

        //Logout
        simPage.logout(testUser);
    }

    /**
     * Test steps for ITC-052
     */
    @Test
    public void addFixedCellularDeviceInternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");

        //Select a simulation within a composite that has no executions.
        SimulationsPage simPage = getPage(SimulationsPage.class);
        simPage.expandComposite(composite, true);
        simPage.selectSim(testSim, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Simulation Settings option.
        SimulationSettingsModal simSettingsModal = simPage.clickSimulationSettings();

        //Click the Fixed Cellular tab.
        ConfigureFixedCellularDevicePartialPage fixedCellDevicesPage = simSettingsModal.clickFixedCellularDevicesTab();

        //Click the Create button.
        CreateFixedCellularDeviceModal createModal = fixedCellDevicesPage.clickCreateFixedCellularDeviceBtn();

        //With all fields blank, click the Create button in the Create Fixed Cellular Device modal.
        createModal.clickCreate(false);

        //Verify an error icon is displayed associated with the Name text box indicating the valid name is required.
        Assert.assertTrue("Name required error not displayed when fixed cell is attempted to be created with all fields blank.", createModal.isNameRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the MAC Address text box indicating a valid MAC address is required.
        Assert.assertTrue("MAC Address required error not displayed when fixed cell is attempted to be created with all fields blank.", createModal.isMACAddressRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the X Coordinate text box indicating a valid x coordinate is required.
        Assert.assertTrue("X coordinate required error not displayed when fixed cell is attempted to be created with all fields blank.", createModal.isXCoordinateRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the Y Coordinate text box indicating a valid y coordinate is required.
        Assert.assertTrue("Y coordinate required error not displayed when fixed cell is attempted to be created with all fields blank.", createModal.isYCoordinateRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the Z Coordinate text box indicating a valid z coordinate is required.
        Assert.assertTrue("Z coordinate required error not displayed when fixed cell is attempted to be created with all fields blank.", createModal.isZCoordinateRequiredErrorDisplayed());

        //Enter one or more space characters in the Name text box and enter valid values in the remaining fields.
        createModal.setAllFields(testFixedCellDevice);
        createModal.setName("   ");

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error not displayed when whitespace only is entered in Name text box.", createModal.isLeadingTrailingWhitespaceNameErrorDisplayed());

        //Enter a value in the Name text box that contains special characters, not including dashes (e.g., ‘T3$t’).
        createModal.setName(RandomStringGenerator.nextLetterString(5) + "*()*#(@");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid name error is not displayed when special characters are used in Name text box.", createModal.isInvalidFixedCellNameErrorDisplayed());

        //Enter a value in the Name text box that contains consecutive spaces within the entered text (e.g., ‘Te    st’).
        createModal.setName(RandomStringGenerator.nextLetterString(5) + "  " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid name error is not displayed when consecutive spaces are used in Name text box.", createModal.isInvalidFixedCellNameErrorDisplayed());

        //Enter a value in the Name text box that begins with a space (e.g., ‘ Test’).
        createModal.setName(" " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error not displayed when leading whitespace is entered in Name text box.", createModal.isLeadingTrailingWhitespaceNameErrorDisplayed());

        //Enter a value in the Name text box that ends with a space (e.g., ‘Test ’).
        createModal.setName(RandomStringGenerator.nextLetterString(5) + " ");

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error not displayed when trailing whitespace is entered in Name text box.", createModal.isLeadingTrailingWhitespaceNameErrorDisplayed());

        //Enter a value in the Name text box that begins with a hyphen/dash (e.g., ‘-Test’).
        createModal.setName("-" + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid name error is not displayed when a leading dash ('-') character is used in Name text box.", createModal.isInvalidFixedCellNameErrorDisplayed());

        //Enter a value in the Name text box that ends with a hyphen/dash (e.g., ‘Test-’).
        createModal.setName(RandomStringGenerator.nextLetterString(5) + "-");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid name error is not displayed when a trailing dash ('-') character is used in Name text box.", createModal.isInvalidFixedCellNameErrorDisplayed());

        //Enter the same name as an existing fixed cellular device in the Name text box.
        createModal.setName(existingFixedCellDevice);

        //Verify an error is displayed associated with the Name text box indicating device names must be unique.
        Assert.assertTrue("Duplicate device name error not displayed when duplicate device name: " + existingFixedCellDevice.getName() + " is entered in Name text box.",
                createModal.isDuplicateDeviceNameErrorDisplayed(existingFixedCellDevice));

        //Enter a valid name in the Name text box.
        createModal.setName(testFixedCellDevice);

        //Enter a non-numerical value in the MAC Address text box (e.g., ‘---’).
        createModal.setMacAddress("---");

        //Verify an error is displayed associated with the MAC Address text box indicating the value entered is not a valid number.
        Assert.assertTrue("Non-numeric error not displayed with MAC address text box when non-numeric value entered.", createModal.isNonNumericMACAddressErrorDisplayed());

        //Enter a MAC address value that is already in use by another fixed cell device in the MAC Address text box.
        createModal.setMacAddress(existingFixedCellDevice);

        //Verify an error is displayed associated with the MAC Address text box indicating the mac address value must be unique.
        Assert.assertTrue(
                "Duplicate MAC address error not displayed when duplicate MAC address: " + existingFixedCellDevice.getMacAddress() + " from existing devce: " + existingFixedCellDevice.getName()
                + " is entered in MAC Address text box.",
                createModal.isDuplicateMACAddressErrorDisplayed(existingFixedCellDevice));

        //Enter a valid value in the MAC Address text box.
        createModal.setMacAddress(testFixedCellDevice);

        //Enter a non-numerical value in the X Coordinate text box (e.g., ‘---’).
        createModal.setXCoordinate("---");

        //Verify an error is displayed associated with the X Coordinate text box indicating the value entered is not a valid number.
        Assert.assertTrue("Non-numeric error not displayed with X Coordinate text box when non-numeric value entered.", createModal.isNonNumericXCoordinateErrorDisplayed());

        //Enter a valid value in the X Coordinate text box.
        createModal.setXCoordinate(testFixedCellDevice);

        //Enter a non-numerical value in the Y Coordinate text box (e.g., ‘---’).
        createModal.setYCoordinate("---");

        //Verify an error is displayed associated with the Y Coordinate text box indicating the value entered is not a valid number.
        Assert.assertTrue("Non-numeric error not displayed with Y Coordinate text box when non-numeric value entered.", createModal.isNonNumericYCoordinateErrorDisplayed());

        //Enter a valid value in the Y Coordinate text box.
        createModal.setYCoordinate(testFixedCellDevice);

        //Enter a non-numerical value in the Z Coordinate text box (e.g., ‘---’).
        createModal.setZCoordinate("---");

        //Verify an error is displayed associated with the Z Coordinate text box indicating the value entered is not a valid number.
        Assert.assertTrue("Non-numeric error not displayed with Z Coordinate text box when non-numeric value entered.", createModal.isNonNumericZCoordinateErrorDisplayed());

        //Enter a valid value in the Z Coordinate text box.
        createModal.setZCoordinate(testFixedCellDevice);

        //Click the Create button.
        createModal.clickCreate(true);

        //Verify the new fixed cellular device is listed in the Fixed Cellular Devices table.
        Assert.assertTrue("Fixed Cellular Device is not displayed in list after being created.", fixedCellDevicesPage.isFixedCellularDeviceDisplayed(testFixedCellDevice.getName()));

        //Click the Close button.
        fixedCellDevicesPage.clickClose();

        //Logout
        simPage.logout(testUser);
    }
}
