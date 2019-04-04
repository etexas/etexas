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
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CreateRSEDeviceModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class which executes steps for the Add a Road Side Equipment Device
 * test, TC-039/ITC-006
 *
 * @author llaroussini
 */
public class AddRSEDeviceTest extends ETexasAfterTestResetTestBase {

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
     * RSE device object used throughout the test case
     */
    private RSEDevice testRSE;

    /**
     * RSE device object used in the internal test case
     */
    private RSEDevice existingTestRSE;

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get test user, test simulation, and test RSE device
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        testSim = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        composite = testSim.getComposite();
        testRSE = RSEDeviceFactory.getRSEDevice(true); //get a random rse device
        existingTestRSE = RSEDeviceFactory.getRSEDevice(true); //get a random rse device
        List<RSEDevice> devices = new ArrayList<RSEDevice>(1);
        devices.add(existingTestRSE);
        testSim.setRSEDevices(devices);
        ETexasEntityManager.addEntities(testUser, testSim, composite, testRSE, existingTestRSE);

        //Register user and create new simulation from template
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulationWithRSEDevice(testSim, existingTestRSE);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-039
     */
    @Test
    public void addRSEDeviceExternalTest() {
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

        //Click the RSE Devices tab.
        ConfigureRSEDevicesPartialPage rseDevicesPage = simSettingsModal.clickRSEDevicesTab();

        //Verify the following buttons are displayed across the top of the RSE Devices tab: Create, Edit, Delete, and Applications.
        rseDevicesPage.checkBtns();

        //Verify the Create button is enabled.
        Assert.assertTrue("The Create button is not enabled as expected upon initial load of RSE tab.", rseDevicesPage.isRSECreateBtnEnabled());

        //Verify the following buttons are disabled: Edit, Delete, and Applications.
        Assert.assertFalse("The Edit button is not disabled as expected upon initial load of RSE tab.", rseDevicesPage.isRSEEditBtnEnabled());
        Assert.assertFalse("The Delete button is not disabled as expected upon initial load of RSE tab.", rseDevicesPage.isRSEDeleteBtnEnabled());
        Assert.assertFalse("The Applications button is not disabled as expected upon initial load of RSE tab.", rseDevicesPage.isRSEApplicationsBtnEnabled());

        //Verify a table is displayed with the following columns: ID, Name, X (cm), Y (cm), and Z (cm).
        rseDevicesPage.checkRSEColumnHeaders();

        //Verify the table displays a list of RSE devices configured in the selected simulation, if any exist.
        int rseListSize = testSim.getRSEDevices().size();
        if (rseListSize == 0) {
            Assert.assertFalse("List of RSE devices displayed despite simualtion not having any associated RSE devices.", rseDevicesPage.areRSEDevicesDisplayed());
        }
        else {
            Assert.assertTrue("List of RSE devices not displayed despite simulation having associated RSE devices.", rseDevicesPage.areRSEDevicesDisplayed());
        }

        //Verify a Close button is displayed at the bottom of the modal.
        Assert.assertTrue("Close button could not be found.", simSettingsModal.isCloseBtnDisplayed());

        //Click the Create button.
        CreateRSEDeviceModal addRSE = rseDevicesPage.clickCreate();

        //Verify a Create RSE Device modal is displayed.
        Assert.assertTrue("Create RSE Device header is not displayed as expected.", addRSE.isCreateRSEHeaderDisplayed());

        //Verify the following icons are displayed in the top right corner of the modal: ‘?’ and ‘x’.
        addRSE.checkCreateRSEHeaderIcons();

        //Click the ‘?’ icon.
        addRSE.clickHelpIcon();

        //Verify that a Create RSE Device Help modal is displayed with instructions for creating RSE devices.
        addRSE.checkHelpModal();

        //Verify an OK button is displayed in the modal and click the OK button.
        Assert.assertTrue("OK button is not displayed.", addRSE.isHelpOKBtnDisplayed());
        addRSE.clickHelpOKBtn();

        //Verify the Help modal closes.
        Assert.assertFalse("Create RSE Device Help header still displayed after clicking OK button.", addRSE.isCreateRSEHelpHeaderDisplayed());

        //Verify the following text boxes are displayed in the Create RSE Device modal: Name, X (cm), Y (cm), and Z (cm).
        addRSE.checkFieldsDisplayed();

        //Verify the following buttons are displayed at the bottom of the Create RSE Device modal: Create, Reset, and Cancel.
        addRSE.checkBtns();

        //Enter a valid name in the Name text box (see Section 4 for valid values).
        addRSE.setName(testRSE);

        //Enter a valid value in the X (cm) text box (see Section 4 for valid values).
        addRSE.setXCoordinate(testRSE);

        //Enter a valid value in the Y (cm) text box (see Section 4 for valid values).
        addRSE.setYCoordinate(testRSE);

        //Enter a valid value in the Z (cm) text box (see Section 4 for valid values).
        addRSE.setZCoordinate(testRSE);

        //Click the Reset button.
        addRSE.clickReset();

        //Verify all values are returned to their default state.
        addRSE.checkFieldValues("", "", "", "");

        //Enter valid values in all text boxes.
        addRSE.setAllFields(testRSE);

        //Click the Cancel button.
        addRSE.clickCancel();

        //Verify the Create RSE Device modal is no longer displayed.
        Assert.assertFalse("Add RSE device form header is still displayed after clicking Cancel.", addRSE.isCreateRSEHeaderDisplayed());

        //Verify the new RSE device is not listed in the RSE Devices table.
        Assert.assertFalse("RSE Device is displayed in list after being cancelled.", rseDevicesPage.isRSENameDisplayed(testRSE));

        //Click the Create button.
        rseDevicesPage.clickCreate();

        //Enter valid values in all text boxes.
        addRSE.setAllFields(testRSE);

        //Click the ‘x’ icon.
        addRSE.clickCloseIcon();

        //Verify the Create RSE Device modal is no longer displayed.
        Assert.assertFalse("Add RSE device form header is still displayed after clicking Close.", addRSE.isCreateRSEHeaderDisplayed());

        //Verify the new RSE device is not listed in the RSE Devices table.
        Assert.assertFalse("RSE Device is displayed in list after modal was closed.", rseDevicesPage.isRSENameDisplayed(testRSE));

        //Click the Create button.
        rseDevicesPage.clickCreate();

        //Enter valid values in all text boxes.
        addRSE.setAllFields(testRSE);

        //Click the Create button.
        addRSE.clickCreate(true);

        //Verify the Create RSE Device modal is no longer displayed.
        Assert.assertFalse("Add RSE device form header is still displayed after clicking Create.", addRSE.isCreateRSEHeaderDisplayed());

        //Verify the new RSE device is listed in the RSE Devices table (includes checking all associated values)
        testRSE.setID(rseDevicesPage.getRSEID(testRSE));
        Assert.assertTrue("RSE is not displayed in list after being created.", rseDevicesPage.isRSEDisplayed(testRSE));

        //Verify the newly created RSE device has an auto-generated ID displayed in the ID column.
        Assert.assertNotNull("An ID was not generated for the newly created RSE device.", rseDevicesPage.getRSEID(testRSE));

        //Click the Close button.
        rseDevicesPage.clickClose();

        //Logout
        simPage.logout(testUser);
    }

    /**
     * Test steps for ITC-006
     */
    @Test
    public void addRSEDeviceInternalTest() {
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

        //Click the RSE Devices tab.
        ConfigureRSEDevicesPartialPage rseTab = simSettingsModal.clickRSEDevicesTab();

        //Click the Create button.
        CreateRSEDeviceModal createModal = rseTab.clickCreate();

        //With all fields blank, click the Create button in the Create RSE Device modal.
        createModal.setAllFields("", "", "", "");

        //Verify an error icon is displayed associated with the Name text box indicating a valid name is required.
        Assert.assertTrue("Name required error is not displayed as expected when RSE is attempted to be created with all fields blank.", createModal.isNameRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the X Coordinate text box indicating a valid x coordinate is required.
        Assert.assertTrue("X Coordinate required error is not displayed as expected when RSE is attempted to be created with all fields blank.", createModal.isXCoordinateRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the Y Coordinate text box indicating a valid y coordinate is required.
        Assert.assertTrue("Y Coordinate required error is not displayed as expected when RSE is attempted to be created with all fields blank.", createModal.isYCoordinateRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the Z Coordinate text box indicating a valid z coordinate is required.
        Assert.assertTrue("Z Coordinate required error is not displayed as expected when RSE is attempted to be created with all fields blank.", createModal.isZCoordinateRequiredErrorDisplayed());

        //Enter one or more space characters in the Name text box and enter valid values in the remaining fields.
        createModal.setAllFields(testRSE);
        createModal.setName("   ");

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error is not displayed when whitespace only is entered in Name text box.", createModal.isRSENameWhitespaceErrorDisplayed());

        //Enter a value in the Name text box that contains special characters, not including dashes (e.g., ‘T3$t’).
        createModal.setName(RandomStringGenerator.nextLetterString(5) + "&*$&@");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid name error is not displayed when special characters are entered in Name text box.", createModal.isInvalidRSENameErrorDisplayed());

        //Enter a value in the Name text box that contains consecutive spaces within the entered text (e.g., ‘Te    st’).
        createModal.setName(RandomStringGenerator.nextLetterString(5) + "  " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid name error is not displayed when consecutive spaces are entered in Name text box.", createModal.isInvalidRSENameErrorDisplayed());

        //Enter a value in the Name text box that begins with a space (e.g., ‘ Test’).
        createModal.setName(" " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error is not displayed when leading whitespace is entered in Name text box.", createModal.isRSENameWhitespaceErrorDisplayed());

        //Enter a value in the Name text box that ends with a space (e.g., ‘Test ’).
        createModal.setName(RandomStringGenerator.nextLetterString(5) + " ");

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error is not displayed when trailing whitespace is entered in Name text box.", createModal.isRSENameWhitespaceErrorDisplayed());

        //Enter a value in the Name text box that begins with a hyphen/dash (e.g., ‘-Test’).
        createModal.setName("-" + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid name error is not displayed when a leading dash ('-') character is used in Name text box.", createModal.isInvalidRSENameErrorDisplayed());

        //Enter a value in the Name text box that ends with a hyphen/dash (e.g., ‘Test-’).
        createModal.setName(RandomStringGenerator.nextLetterString(5) + "-");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid name error is not displayed when a trailing dash ('-') character is used in Name text box.", createModal.isInvalidRSENameErrorDisplayed());

        //Enter the same name as an existing RSE device in the Name text box.
        createModal.setName(existingTestRSE);

        //Verify an error is displayed associated with the Name text box indicating device names must be unique.
        Assert.assertTrue("Duplicate device name error not displayed when duplicate name: " + existingTestRSE.getName() + " is entered in Name text box.",
                createModal.isDuplicateRSENameErrorDisplayed(existingTestRSE));

        //Enter a valid name in the Name text box.
        createModal.setName(testRSE);

        //Enter a non-numerical value in the X Coordinate text box (e.g., ‘---’).
        createModal.setXCoordinate("---");

        //Verify an error is displayed associated with the X Coordinate text box indicating the value entered is not a valid number.
        Assert.assertTrue("Invalid, non-numerical, error not displayed when a non-numerical value is entered in X Coordinate text box.", createModal.isInvalidXCoordinateErrorDisplayed());

        //Enter a valid value in the X Coordinate text box.
        createModal.setXCoordinate(testRSE);

        //Enter a non-numerical value in the Y Coordinate text box (e.g., ‘---’).
        createModal.setYCoordinate("---");

        //Verify an error is displayed associated with the Y Coordinate text box indicating the value entered is not a valid number.
        Assert.assertTrue("Invalid, non-numerical, error not displayed when a non-numerical value is entered in Y Coordinate text box.", createModal.isInvalidYCoordinateErrorDisplayed());

        //Enter a valid value in the Y Coordinate text box.
        createModal.setYCoordinate(testRSE);

        //Enter a non-numerical value in the Z Coordinate text box (e.g., ‘---’).
        createModal.setZCoordinate("---");

        //Verify an error is displayed associated with the Z Coordinate text box indicating the value entered is not a valid number.
        Assert.assertTrue("Invalid, non-numerical, error not displayed when a non-numerical value is entered in Z Coordinate text box.", createModal.isInvalidZCoordinateErrorDisplayed());

        //Enter a valid value in the Z Coordinate text box.
        createModal.setZCoordinate(testRSE);

        //Click the Create button.
        createModal.clickCreate(true);

        //Verify the new RSE device is listed in the RSE Devices table.
        testRSE.setID(rseTab.getRSEID(testRSE));
        Assert.assertTrue("RSE is not displayed in list after being created.", rseTab.isRSEDisplayed(testRSE));

        //Click the Close button.
        rseTab.clickClose();

        //Logout
        simPage.logout(testUser);

    }
}