package com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CellularDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.CellularDeviceFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeSettingsModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.ConfigureCellularDevicePartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.CreateCellularDeviceModal;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomNumberGenerator;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class which executes steps for the Add a Cellular Device test,
 * TC-068/ITC-049
 *
 * @author llaroussini
 * @author saistrop
 */
public class AddCellularDeviceProfileTest extends ETexasAfterTestResetTestBase {

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
     * Cellular device object used throughout the test case
     */
    private CellularDevice testCellDevice;

    /**
     * Cellular device object used in the internal test case
     */
    private CellularDevice existingCellDevice;

    /**
     * The name of the cell device object used throughout the test case
     */
    private String testCellDeviceName;

    /**
     * The Min Quantity value of the cell device object
     */
    private String testCellDeviceMin;

    /**
     * The Max Quantity value of the cell device object
     */
    private String testCellDeviceMax;

    /**
     * The Percentage value of the cell device object
     */
    private String testCellDevicePercentage;

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");
        testUser = ETexasUserFactory.getUser(true); //Get a random user.

        //Get test user, test simulation, and test cellular device
        testSim = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        composite = testSim.getComposite(); //gets the Composite from the simulation
        testCellDevice = CellularDeviceFactory.getCellularDevice(true); //get a random cellular device
        testCellDevice.setCellularPercent("20"); //setting a known percentage to ensure percentage total can consistently be set to either less than or greater than 100% in ITC
        testCellDeviceName = testCellDevice.getName(); //gets name of cellular device
        testCellDeviceMin = testCellDevice.getMinCellsPerVehicle(); //gets min quantity for cellular device
        testCellDeviceMax = testCellDevice.getMaxCellsPerVehicle(); //gets max quantity for cellular device
        testCellDevicePercentage = testCellDevice.getCellularPercent(); //gets percentage for cellular device
        existingCellDevice = CellularDeviceFactory.getCellularDevice(true); //get a random cellular device
        existingCellDevice.setCellularPercent("20"); //setting a known percentage to ensure percentage total can consistently be set to either less than or greater than 100% in ITC

        //Add test simulation to Entity Manager
        ETexasEntityManager.addEntities(testUser, testSim, testCellDevice, existingCellDevice);

        //Gets the landing page and creates test simulation with cell device that is used in ITC
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulationWithCellularDevice(testSim, existingCellDevice);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-068
     */
    @Test
    public void addCellularDeviceExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Ensure the Simulations page is loaded
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select composite
        simPage.selectComposite(composite, true);

        //Click Edit button
        simPage.clickEdit();

        //Click Composite Settings from Edit dropdown and verify Composite Settings window displays
        CompositeSettingsModal settingsPage = simPage.clickCompositeSettings();
        Assert.assertTrue("Composite Settings modal is not displayed as expected.", settingsPage.isCompositeSettingsHeaderDisplayed());

        //Click Cellular Device Profiles tab
        ConfigureCellularDevicePartialPage cellDevicesPage = settingsPage.clickCellularTab();

        //Check Cellular buttons
        cellDevicesPage.checkCellularBtns();

        //Verify Create button is enabled
        Assert.assertTrue("Create button is not enabled as expected.", cellDevicesPage.isCellularCreateBtnEnabled());

        //Verify Edit, Delete, and Applications buttons are disabled
        Assert.assertFalse("Edit button is enabled when expected disabled.", cellDevicesPage.isCellularEditBtnEnabled());
        Assert.assertFalse("Delete button enabled when expected disabled.", cellDevicesPage.isCellularDeleteBtnEnabled());
        Assert.assertFalse("Applications button enabled when expected disabled.", cellDevicesPage.isCellularApplicationsBtnEnabled());

        //Verify headers for ID, Device Name, and Cellular Percentage are displayed
        cellDevicesPage.checkCellularDeviceColumnHeaders();

        //Verify a table displays for any existing cellular devices
        Assert.assertTrue("No table was displayed for existing cellular devices.", cellDevicesPage.isDeviceTableDisplayed());

        //Verify a Close button displays at bottom of window
        Assert.assertTrue("Close button could not be found.", settingsPage.isCloseBtnDisplayed());

        //Click Create Cellular Device button
        CreateCellularDeviceModal addCellForm = cellDevicesPage.clickCreateCellularDeviceBtn();

        //Verify Create Cellular Device Form window displays
        Assert.assertTrue("Create cellular device form header is not displayed as expected.", addCellForm.isCreateCellularDeviceHeaderDisplayed());

        //Verify help and close icons display
        addCellForm.checkCreateCellularDeviceHeaderIcons();

        //Click Help icon and verify the help modal displays
        addCellForm.clickCreateCellularDeviceHelp();
        Assert.assertTrue("Create cellular device help form header is not displayed as expected.", addCellForm.isCreateCellularDeviceHelpHeaderDisplayed());
        Assert.assertTrue("Create cellular device help form content is not displayed as expected.", addCellForm.isHelpContentDisplayed());

        //Verify OK button displays, click OK
        Assert.assertTrue("OK button is not displayed.", addCellForm.isHelpOKBtnDisplayed());
        addCellForm.clickHelpOKBtn();

        //Verify help modal closes
        Assert.assertFalse("Help modal is still displayed after clicking the OK button.", addCellForm.isCreateCellularDeviceHelpHeaderDisplayed());

        //Verify text box fields display
        addCellForm.checkFieldsDisplayed();

        //Verify Create, Reset, and Cancel buttons display at bottom of window
        addCellForm.checkBtns();

        //Enter values in text box fields, click reset, verify fields clear
        addCellForm.setAllFields(testCellDevice);
        addCellForm.clickReset();
        addCellForm.checkFieldValues("", "", "", "");

        //Enter values in text box fields, click cancel, verify Create Cellular Device Profile modal is not displayed
        addCellForm.setAllFields(testCellDevice);
        addCellForm.clickCancel();
        Assert.assertFalse("Create Cellular Device Profile modal still displayed after clicking cancel.", addCellForm.isCreateCellularDeviceHeaderDisplayed());

        //Verify new cellular device profile isn't present in list after clicking cancel
        Assert.assertFalse("Cellular Devicce Profile is present after clicking cancel.", cellDevicesPage.isCellularDeviceDisplayed(testCellDeviceName));

        //Clicks Create button, enters valid values, clicks the 'x' icon
        addCellForm = cellDevicesPage.clickCreateCellularDeviceBtn();
        addCellForm.setAllFields(testCellDevice);
        addCellForm.clickCloseIcon();

        //Verify Create Cellular Device Profile modal is no longer displayed and cellular device doesn't display in table
        Assert.assertFalse("Create Cellular Device Profile modal still displayed after clicking 'x' icon.", addCellForm.isCreateCellularDeviceHeaderDisplayed());
        Assert.assertFalse("Cellular Devicce Profile is present after clicking 'x' icon.", cellDevicesPage.isCellularDeviceDisplayed(testCellDeviceName));

        //Creates new Cellular Device Profile
        addCellForm = cellDevicesPage.clickCreateCellularDeviceBtn();
        addCellForm.setAllFields(testCellDevice);
        settingsPage = addCellForm.clickCreate(true);

        //Assert Create Cellular Device Profile modal is no longer displayed
        Assert.assertFalse("Create Cellular Device Profile modal still displayed after clicking create.", addCellForm.isCreateCellularDeviceHeaderDisplayed());
        Assert.assertTrue("Cellular Device Profile is not present after clicking create.", cellDevicesPage.isCellularDeviceDisplayed(testCellDeviceName));

        //Assert values match those entered previously
        Assert.assertTrue("Name does not match the value entered previously.", cellDevicesPage.isCellularDeviceAttributeDisplayed(testCellDeviceName));
        Assert.assertTrue("Min Quantity does not match the value entered previously.", cellDevicesPage.isCellularDeviceAttributeDisplayed(String.valueOf(testCellDeviceMin)));
        Assert.assertTrue("Max Quantity does not match the value entered previously.", cellDevicesPage.isCellularDeviceAttributeDisplayed(String.valueOf(testCellDeviceMax)));
        Assert.assertTrue("Percentage does not match the value entered previously.", cellDevicesPage.isCellularDeviceAttributeDisplayed(String.valueOf(testCellDevicePercentage) + "%"));

        //Assert an ID has been assigned to the row
        Assert.assertTrue("No ID was assigned to the Cellular Device.", cellDevicesPage.isCellularDeviceIdDisplayed(testCellDeviceName));

        //Assert the new Cellular Device is selected
        Assert.assertTrue("The Cellular Device is not selected.", cellDevicesPage.isCellularRowSelected(testCellDeviceName));

        //Asserts the Create, Edit, Delete, and Parameter buttons are enabled
        Assert.assertTrue("Create button is not enabled as expected.", cellDevicesPage.isCellularCreateBtnDisplayed());
        Assert.assertTrue("Edit button is disabled when expected enabled.", cellDevicesPage.isCellularEditBtnEnabled());
        Assert.assertTrue("Delete button disabled when expected enabled.", cellDevicesPage.isCellularDeleteBtnEnabled());
        Assert.assertTrue("Applications button disabled when expected enabled.", cellDevicesPage.isCellularApplicationsBtnEnabled());

        //Log out
        simPage.logout(testUser);
    }

    /**
     * Test steps for ITC-049
     */
    @Test
    public void addCellularDeviceInternalTest() {
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

        //Click the Cellular Device Profiles tab.
        ConfigureCellularDevicePartialPage cellTab = compositeSettingsModal.clickCellularTab();

        //Click the Create button.
        CreateCellularDeviceModal createCellModal = cellTab.clickCreateCellularDeviceBtn();

        //With all fields blank, click the Create button in the Create Cellular Device Profile modal.
        createCellModal.clickCreate(false);

        //Verify an error icon is displayed associated with the Name text box indicating a valid device profile name is required.
        Assert.assertTrue("Field required error is not displayed as expected with Name text box when cellular device is attempted to be created with all fields blank.",
                createCellModal.isNameFieldRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the Min Quantity text box indicating a valid minimum number of cellular devices per vehicle is required.
        Assert.assertTrue("Field required error is not displayed as expected with Min Quantity text box when cellular device is attempted to be created with all fields blank.",
                createCellModal.isNameFieldRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the Max Quantity text box indicating a valid maximum number of cellular devices per vehicle is required.
        Assert.assertTrue("Field required error is not displayed as expected with Max Quantity text box when cellular device is attempted to be created with all fields blank.",
                createCellModal.isNameFieldRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the Percentage text box indicating a valid cellular device profile percentage is required.
        Assert.assertTrue("Field required error is not displayed as expected with Percentage text box when cellular device is attempted to be created with all fields blank.",
                createCellModal.isNameFieldRequiredErrorDisplayed());

        //Enter one or more space characters in the Name text box and enter valid values in the remaining fields.
        createCellModal.setAllFields(testCellDevice);
        createCellModal.setCellularDeviceName("   ");

        //Click the Create button.
        createCellModal.clickCreate(false);

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error not displayed with Name text box when Name text box contains whitespace only.",
                createCellModal.isNameLeadingTrailingWhitespaceErrorDisplayed());

        //Enter a value in the Name text box that contains special characters, not including dashes (e.g., ‘T3$t’).
        createCellModal.setCellularDeviceName(RandomStringGenerator.nextLetterString(5) + "*(*)(");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid device name error not displayed when Name text box contains special characters.", createCellModal.isInvalidDeviceNameErrorDisplayed());

        //Enter a value in the Name text box that contains consecutive spaces within the entered text (e.g., ‘Te    st’).
        createCellModal.setCellularDeviceName(RandomStringGenerator.nextLetterString(5) + "  " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid device name error not displayed when Name text box contains consecutive spaces.", createCellModal.isInvalidDeviceNameErrorDisplayed());

        //Enter a value in the Name text box that begins with a space (e.g., ‘ Test’).
        createCellModal.setCellularDeviceName(" " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error not displayed with Name text box when Name text box contains leading whitespace.",
                createCellModal.isNameLeadingTrailingWhitespaceErrorDisplayed());

        //Enter a value in the Name text box that ends with a space (e.g., ‘Test ’).
        createCellModal.setCellularDeviceName(RandomStringGenerator.nextLetterString(5) + " ");

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error not displayed with Name text box when Name text box contains trailing whitespace.",
                createCellModal.isNameLeadingTrailingWhitespaceErrorDisplayed());

        //Enter a value in the Name text box that begins with a hyphen/dash (e.g., ‘-Test’).
        createCellModal.setCellularDeviceName("-" + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid device name error not displayed when Name text box contains a leading dash ('-') character.", createCellModal.isInvalidDeviceNameErrorDisplayed());

        //Enter a value in the Name text box that ends with a hyphen/dash (e.g., ‘Test-’).
        createCellModal.setCellularDeviceName(RandomStringGenerator.nextLetterString(5) + "-");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid device name error not displayed when Name text box contains a trailing dash ('-') character.", createCellModal.isInvalidDeviceNameErrorDisplayed());

        //Enter the same name as an existing device (profile) in the Name text box.
        createCellModal.setCellularDeviceName(existingCellDevice);

        //Verify an error is displayed associated with the Name text box indicating device names must be unique.
        Assert.assertTrue("Duplicate device error not displayed when duplicate name used in Name text box.", createCellModal.isDuplicateDeviceErrorDisplayed(existingCellDevice));

        //Enter a valid name in the Name text box.
        createCellModal.setCellularDeviceName(testCellDevice);

        //Enter a non-numerical value in the Min Quantity text box (e.g., ‘---’).
        createCellModal.setMinCellsPerVehicle("---");

        //Verify an error is displayed associated with the Min Quantity text box indicating the value entered is not a valid number.
        Assert.assertTrue("Non-numeric error not displayed when non-numeric value entered in Min Quantity text box.", createCellModal.isNonNumericMinQuantityErrorDisplayed());

        //Enter a value greater than 8 in the Min Quantity text box.
        createCellModal.setMinCellsPerVehicle(Integer.toString(RandomNumberGenerator.nextInteger(100) + 8));

        //Verify an error is displayed associated with the Min Quantity text box indicating the maximum acceptable value is 8.
        Assert.assertTrue("Invalid Range error not displayed with Min Quantity text box when value greater than 8 is used.", createCellModal.isMinQuantityInvalidRangeErrorDisplayed());

        //Enter a decimal numerical value between 0 and 8 (i.e., ‘2.5’) in the Min Quantity text box.
        createCellModal.setMinCellsPerVehicle("2.5");

        //Click anywhere outside the Min Quantity text box.
        createCellModal.setMaxCellsPerVehicle(testCellDevice);

        //Verify the value is automatically rounded to the closest whole integer.
        Assert.assertEquals("Min Quantity value is not rounded as expected.", "3", createCellModal.getDisplayedMinCellsPerVehicle());

        //Enter a non-numerical value in the Max Quantity text box (e.g., ‘---’).
        createCellModal.setMaxCellsPerVehicle("---");

        //Verify an error is displayed associated with the Max Quantity text box indicating the value entered is not a valid number.
        Assert.assertTrue("Non-numeric error not displayed when non-numeric value entered in Max Quantity text box.", createCellModal.isNonNumericMaxQuantityErrorDisplayed());

        //Enter a value greater than 8 in the Max Quantity text box.
        createCellModal.setMaxCellsPerVehicle(Integer.toString(RandomNumberGenerator.nextInteger(100) + 8));

        //Verify an error is displayed associated with the Max Quantity text box indicating the maximum acceptable value is 8.
        Assert.assertTrue("Invalid Range error not displayed with Max Quantity text box when value greater than 8 is used.", createCellModal.isMaxQuantityInvalidRangeErrorDisplayed());

        //Enter a value of 0 or greater, but less than the value of the Min Quantity in the Max Quantity text box.
        createCellModal.setMinCellsPerVehicle("5");
        createCellModal.setMaxCellsPerVehicle("2");
        createCellModal.clickCreate(false);

        //Verify an error is displayed associated with the Max Quantity text box indicating the maximum cells per vehicle must be greater than or equal to the minimum cells per vehicle.
        Assert.assertTrue("Overall Invalid Range error not displayed with Max Quantity text box when value in Max Quanity is less than value in Min Quantity.",
                createCellModal.isInvalidMinMaxRangeErrorDisplayed());

        //Enter a decimal numerical value greater than the Min Quantity between 0 and 8 (i.e., ‘3.5’) in the Max Quantity text box.
        createCellModal.setMaxCellsPerVehicle("1.5");

        //Click anywhere outside the Max Quantity text box.
        createCellModal.setMinCellsPerVehicle(testCellDevice);

        //Verify the value is automatically rounded to the closest whole integer.
        Assert.assertEquals("Max Quantity value is not rounded as expected.", "2", createCellModal.getDisplayedMaxCellsPerVehicle());

        //Enter a non-numerical value in the Percentage text box (e.g., ‘---’).
        createCellModal.setMaxCellsPerVehicle(testCellDevice);
        createCellModal.setCellularPercentage("---");

        //Verify an error is displayed associated with the Percentage text box indicating the value entered is not a valid number.
        Assert.assertTrue("Non-numeric error not displayed when non-numeric value entered in Percentage text box.", createCellModal.isNonNumericPercentageErrorDisplayed());

        //Enter a value less than 1 in the Percentage text box.
        createCellModal.setCellularPercentage(Integer.toString(RandomNumberGenerator.nextInteger(1)));

        //Verify an error is displayed associated with the Percentage text box indicating the minimum acceptable value is 1.
        Assert.assertTrue("Invalid Percentage Range error not displayed with Percentage text box when value entered is less than 1.", createCellModal.isInvalidPercentageRangeErrorDisplayed());

        //Enter a value greater than 100 in the Percentage text box.
        createCellModal.setCellularPercentage(Integer.toString(RandomNumberGenerator.nextInteger() + 101));

        //Verify an error is displayed associated with the Percentage text box indicating the maximum acceptable value is 100.
        Assert.assertTrue("Invalid Percentage Range error not displayed with Percentage text box when value entered is greater than 100.", createCellModal.isInvalidPercentageRangeErrorDisplayed());

        //Enter a valid numerical value between 1 and 100 in the Percentage text box.
        createCellModal.setCellularPercentage(Integer.toString(RandomNumberGenerator.nextInteger(60))); //upper bound set to 60 to ensure total percentage of all devices does not exceed 100%

        //Click the Create button.
        createCellModal.clickCreate(true);

        //Verify the cellular device is listed in the Cellular Device Profiles list.
        Assert.assertTrue("Cellular Device Profile is not present after clicking create.", cellTab.isCellularDeviceDisplayed(testCellDeviceName));

        //Click the Create button.
        cellTab.clickCreateCellularDeviceBtn();

        //Enter a valid name in the Name text box.
        createCellModal.setCellularDeviceName(RandomStringGenerator.nextLetterString(5));

        //Enter a value between 1 and 100 in the Percentage text box that will result in the combined total percentage of all cellular devices to exceed 100%.
        createCellModal.setCellularPercentage("90");

        //Verify an error is displayed associated with the Percentage text box indicating the percentage total must not exceed 100%.
        Assert.assertTrue("Error message indicating combined percentage total exceeds maximum percentage is not displayed when total percentages for all cellular devices exceeds 100%",
                createCellModal.isCombinedPercentageExceedsMaxErrorText());

        //Click the Cancel button.
        createCellModal.clickCancel();

        //Click the Close button.
        cellTab.clickCloseBtn();

        //Logout
        simPage.logout(testUser);
    }
}