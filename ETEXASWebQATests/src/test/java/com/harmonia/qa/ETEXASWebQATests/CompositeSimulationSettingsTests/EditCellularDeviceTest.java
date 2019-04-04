package com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests;

import java.util.ArrayList;
import java.util.List;

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
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.EditCellularDeviceModal;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomNumberGenerator;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class which executes steps for the Edit Cellular Device test,
 * TC-069/ITC-050
 *
 * @author llaroussini
 */
public class EditCellularDeviceTest extends ETexasAfterTestResetTestBase {

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
     * The Percentage value of the cell device object, set to a known value to
     * ensure percentage total can consistently be set to either less than or
     * greater than 100%
     */
    private String testCellDevicePercentage = "20";

    /**
     * Test setup
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get test user, test simulation, and test cellular device
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        testSim = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        composite = testSim.getComposite(); //gets the Composite from the simulation
        testCellDevice = CellularDeviceFactory.getCellularDevice(true); //get a random cellular device
        testCellDevice.setCellularPercent(testCellDevicePercentage);
        existingCellDevice = CellularDeviceFactory.getCellularDevice(true); //get a random cellular device
        existingCellDevice.setCellularPercent(testCellDevicePercentage);
        testCellDeviceName = testCellDevice.getName(); //gets name of cellular device
        testCellDeviceMin = testCellDevice.getMinCellsPerVehicle(); //gets min quantity for cellular device
        testCellDeviceMax = testCellDevice.getMaxCellsPerVehicle(); //gets max quantity for cellular device
        testCellDevicePercentage = testCellDevice.getCellularPercent(); //gets percentage for cellular device
        List<CellularDevice> cells = new ArrayList<CellularDevice>(2);
        cells.add(testCellDevice);
        cells.add(existingCellDevice);

        //Add test simulation to Entity Manager
        ETexasEntityManager.addEntities(testUser, testSim, testCellDevice);

        //Gets the landing page and creates test simulation
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulationWithCellularDevices(testSim, cells);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-069
     */
    @Test
    public void editCellularDeviceExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Ensure Simulations page is loaded
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select an existing simulation with no executions and at least one cellular device.
        simPage.selectComposite(composite, true);

        //Click Edit and then select Composite Settings
        simPage.clickEdit();
        CompositeSettingsModal settingsPage = simPage.clickCompositeSettings();

        //Verify Composite Settings Modal appears
        Assert.assertTrue("Composite Settings modal is not displayed as expected.", settingsPage.isCompositeSettingsHeaderDisplayed());

        //Select Cellular Device Profiles Tab
        ConfigureCellularDevicePartialPage cellDevicesPage = settingsPage.clickCellularTab();

        //Verify Cellular Device Profile associated with composite is displayed
        Assert.assertTrue("There is no cellular device associated with the composite displayed where one was expected.", cellDevicesPage.isCellularDeviceDisplayed(testCellDeviceName));

        //Select cellular device profile and verify Edit button is enabled
        cellDevicesPage.selectCellularDevice(testCellDevice);
        Assert.assertTrue("Edit button was not enabled when expected.", cellDevicesPage.isCellularEditBtnEnabled());

        //Click the Edit button
        EditCellularDeviceModal editForm = cellDevicesPage.clickEdit();

        //Verify Edit Cellular Device Modal and icons display
        Assert.assertTrue("Edit Cellular Device modal was not displayed when expected.", editForm.isEditCellularDeviceHeaderDisplayed());
        editForm.checkEditCellularDeviceHeaderIcons();

        //Click the help icon and verify the Help modal appears
        editForm.clickEditCellularDeviceHelp();
        Assert.assertTrue("Edit Cellular Device Profile Help modal did not appear when expected.", editForm.isEditCellularDeviceHelpHeaderDisplayed());

        //Verify OK button is displayed in the Help modal
        Assert.assertTrue("OK Button not displayed in the Help modal where expected.", editForm.isEditCellularDeviceHelpOKBtnDisplayed());

        //Click OK button and verify header no longer displayed
        editForm.clickEditCellularDeviceHelpOKBtn();
        Assert.assertFalse("Help modal still displayed after clicking OK button.", editForm.isEditCellularDeviceHelpHeaderDisplayed());

        //Verify text boxes appear for Name, Min Quantity, Max Quantity, and Percentage
        editForm.checkFieldsDisplayed();

        //Verify fields are populated with the device's data
        editForm.checkFieldValues(testCellDevice);

        //Verify Update, Reset, and Cancel buttons are displayed at the bottom of the Edit form
        editForm.checkEditCellularDeviceBtns();

        //Create new Cellular Device Profile to test with
        CellularDevice randomDevice = CellularDeviceFactory.getCellularDevice(true);

        //Update the values and click the Reset button
        editForm.setAllFields(randomDevice);
        editForm.clickReset();

        //Verify values return to previous entries
        editForm.checkFieldValues(testCellDevice);

        //Update values and click the Cancel button
        editForm.setAllFields(randomDevice);
        editForm.clickCancel();

        //Verify Edit modal is no longer displayed
        Assert.assertFalse("Edit Cellular Device Profile modal is still displayed after clicking cancel.", editForm.isEditCellularDeviceHeaderDisplayed());

        //Verify values haven't changed
        Assert.assertTrue("Original Device Name is not present after update was canceled.", cellDevicesPage.isCellularDeviceAttributeDisplayed(testCellDeviceName));
        Assert.assertTrue("Original Percentage is not present after update was canceled.", cellDevicesPage.isCellularDeviceAttributeDisplayed(testCellDevicePercentage + "%"));
        Assert.assertTrue("Original Min Quantity is not present after update was canceled.", cellDevicesPage.isCellularDeviceAttributeDisplayed(testCellDeviceMin));
        Assert.assertTrue("Original Max Quantity is not present after update was canceled.", cellDevicesPage.isCellularDeviceAttributeDisplayed(testCellDeviceMax));

        //Select cellular device profile if it had not been selected
        cellDevicesPage.selectCellularDevice(testCellDevice);

        //Click the Edit button and verify modal appears
        editForm = cellDevicesPage.clickEdit();
        Assert.assertTrue("Edit Celluluar Device modal is not displayed after clicking Edit.", editForm.isEditCellularDeviceHeaderDisplayed());

        //Update values and click the 'x' icon
        editForm.setAllFields(randomDevice);
        editForm.clickCloseIcon();

        //Verify values haven't changed
        Assert.assertTrue("Original Device Name is not present after edit modal was closed.", cellDevicesPage.isCellularDeviceAttributeDisplayed(testCellDeviceName));
        Assert.assertTrue("Original Percentage is not present after edit modal was closed.", cellDevicesPage.isCellularDeviceAttributeDisplayed(testCellDevicePercentage + "%"));
        Assert.assertTrue("Original Min Quantity is not present after edit modal was closed.", cellDevicesPage.isCellularDeviceAttributeDisplayed(testCellDeviceMin));
        Assert.assertTrue("Original Max Quantity is not present after edit modal was closed.", cellDevicesPage.isCellularDeviceAttributeDisplayed(testCellDeviceMax));

        //Select cellular device profile if it had not been selected
        cellDevicesPage.selectCellularDevice(testCellDevice);

        //Click the Edit button and verify modal appears
        editForm = cellDevicesPage.clickEdit();
        Assert.assertTrue("Edit Celluluar Device modal is not displayed after clicking Edit.", editForm.isEditCellularDeviceHeaderDisplayed());

        //Update values and click the Update button
        editForm.setAllFields(randomDevice);
        editForm.clickUpdate(true);

        //Verify Modal is no longer displayed
        Assert.assertFalse("Edit Cellular Device modal still displayed after clicking Update.", editForm.isEditCellularDeviceHeaderDisplayed());

        //Verify Cellular Device matches new values entered previously
        Assert.assertTrue("New Device Name is not present after updating.", cellDevicesPage.isCellularDeviceAttributeDisplayed(randomDevice.getName()));
        Assert.assertTrue("New Percentage is not present after updating.", cellDevicesPage.isCellularDeviceAttributeDisplayed(randomDevice.getCellularPercent() + "%"));
        Assert.assertTrue("New Min Quantity is not present after updating.", cellDevicesPage.isCellularDeviceAttributeDisplayed(randomDevice.getMinCellsPerVehicle()));
        Assert.assertTrue("New Max Quantity is not present after updating.", cellDevicesPage.isCellularDeviceAttributeDisplayed(randomDevice.getMaxCellsPerVehicle()));

        //Verify ID has not changed
        Assert.assertEquals("The ID of the Cellular Device did not remain static after updating.", testCellDevice.getDeviceId(), cellDevicesPage.getCellularDeviceRowID(randomDevice).getText());

        //Update entity manager
        CellularDevice device = ETexasEntityManager.getCellularDevice(testCellDeviceName);
        device.setName(randomDevice.getName());
        device.setMaxCellsPerVehicle(randomDevice.getMaxCellsPerVehicle());
        device.setMinCellsPerVehicle(randomDevice.getMinCellsPerVehicle());
        device.setCellularPercent(randomDevice.getCellularPercent());
        device.setDeviceId(randomDevice.getDeviceId());

        //Log out
        simPage.logout(testUser);
    }

    /**
     * Test steps for ITC-050
     */
    @Test
    public void editCellularDeviceInternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select a composite with at least one cellular device profile.
        simPage.selectComposite(composite, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Composite Settings option.
        CompositeSettingsModal compositeSettingsModal = simPage.clickCompositeSettings();

        //Click the Cellular Device Profiles tab.
        ConfigureCellularDevicePartialPage cellTab = compositeSettingsModal.clickCellularTab();

        //Select a Cellular Device Profile.
        cellTab.selectCellularDevice(testCellDevice);

        //Click the Edit button.
        EditCellularDeviceModal editModal = cellTab.clickEdit();

        //Delete the pre-populated values in all fields.
        editModal.setAllFields("", "", "", "");

        //With all fields blank, click the Update button in the Create Cellular Device Profile modal.
        editModal.clickUpdate(false);

        //Verify an error icon is displayed associated with the Name text box indicating a valid device profile name is required.
        Assert.assertTrue("Field required error is not displayed as expected with Name text box when cellular device is attempted to be created with all fields blank.",
                editModal.isNameFieldRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the Min Quantity text box indicating a valid minimum number of cellular devices per vehicle is required.
        Assert.assertTrue("Field required error is not displayed as expected with Min Quantity text box when cellular device is attempted to be created with all fields blank.",
                editModal.isNameFieldRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the Max Quantity text box indicating a valid maximum number of cellular devices per vehicle is required.
        Assert.assertTrue("Field required error is not displayed as expected with Max Quantity text box when cellular device is attempted to be created with all fields blank.",
                editModal.isNameFieldRequiredErrorDisplayed());

        //Verify an error icon is displayed associated with the Percentage text box indicating a valid cellular device profile percentage is required.
        Assert.assertTrue("Field required error is not displayed as expected with Percentage text box when cellular device is attempted to be created with all fields blank.",
                editModal.isNameFieldRequiredErrorDisplayed());

        //Enter one or more space characters in the Name text box and enter valid values in the remaining fields.
        editModal.setAllFields(testCellDevice);
        editModal.setCellularDeviceName("   ");

        //Click the Update button.
        editModal.clickUpdate(false);

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error not displayed with Name text box when Name text box contains whitespace only.", editModal.isNameLeadingTrailingWhitespaceErrorDisplayed());

        //Enter a value in the Name text box that contains special characters, not including dashes (e.g., ‘T3$t’).
        editModal.setCellularDeviceName(RandomStringGenerator.nextLetterString(5) + "*(*)(");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid device name error not displayed when Name text box contains special characters.", editModal.isInvalidDeviceNameErrorDisplayed());

        //Enter a value in the Name text box that contains consecutive spaces within the entered text (e.g., ‘Te    st’).
        editModal.setCellularDeviceName(RandomStringGenerator.nextLetterString(5) + "  " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid device name error not displayed when Name text box contains consecutive spaces.", editModal.isInvalidDeviceNameErrorDisplayed());

        //Enter a value in the Name text box that begins with a space (e.g., ‘ Test’).
        editModal.setCellularDeviceName(" " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error not displayed with Name text box when Name text box contains leading whitespace.",
                editModal.isNameLeadingTrailingWhitespaceErrorDisplayed());

        //Enter a value in the Name text box that ends with a space (e.g., ‘Test ’).
        editModal.setCellularDeviceName(RandomStringGenerator.nextLetterString(5) + " ");

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error not displayed with Name text box when Name text box contains trailing whitespace.",
                editModal.isNameLeadingTrailingWhitespaceErrorDisplayed());

        //Enter a value in the Name text box that begins with a hyphen/dash (e.g., ‘-Test’).
        editModal.setCellularDeviceName("-" + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid device name error not displayed when Name text box contains a leading dash ('-') character.", editModal.isInvalidDeviceNameErrorDisplayed());

        //Enter a value in the Name text box that ends with a hyphen/dash (e.g., ‘Test-’).
        editModal.setCellularDeviceName(RandomStringGenerator.nextLetterString(5) + "-");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid device name error not displayed when Name text box contains a trailing dash ('-') character.", editModal.isInvalidDeviceNameErrorDisplayed());

        //Enter the same name as an existing device (profile) in the Name text box.
        editModal.setCellularDeviceName(existingCellDevice);

        //Verify an error is displayed associated with the Name text box indicating device names must be unique.
        Assert.assertTrue("Duplicate device error not displayed when duplicate name used in Name text box.", editModal.isDuplicateDeviceErrorDisplayed(existingCellDevice));

        //Enter a valid name in the Name text box.
        editModal.setCellularDeviceName(testCellDevice);

        //Enter a non-numerical value in the Min Quantity text box (e.g., ‘---’).
        editModal.setMinCellsPerVehicle("---");

        //Verify an error is displayed associated with the Min Quantity text box indicating the value entered is not a valid number.
        Assert.assertTrue("Non-numeric error not displayed when non-numeric value entered in Min Quantity text box.", editModal.isNonNumericMinQuantityErrorDisplayed());

        //Enter a value greater than 8 in the Min Quantity text box.
        editModal.setMinCellsPerVehicle(Integer.toString(RandomNumberGenerator.nextInteger(100) + 8));

        //Verify an error is displayed associated with the Min Quantity text box indicating the maximum acceptable value is 8.
        Assert.assertTrue("Invalid Range error not displayed with Min Quantity text box when value greater than 8 is used.", editModal.isMinQuantityInvalidRangeErrorDisplayed());

        //Enter a decimal numerical value between 0 and 8 (i.e., ‘2.5’) in the Min Quantity text box.
        editModal.setMinCellsPerVehicle("2.5");

        //Click anywhere outside the Min Quantity text box.
        editModal.setMaxCellsPerVehicle(testCellDevice);

        //Verify the value is automatically rounded to the closest whole integer.
        Assert.assertEquals("Min Quantity value is not rounded as expected.", "3", editModal.getDisplayedMinCellsPerVehicle());

        //Enter a non-numerical value in the Max Quantity text box (e.g., ‘---’).
        editModal.setMaxCellsPerVehicle("---");

        //Verify an error is displayed associated with the Max Quantity text box indicating the value entered is not a valid number.
        Assert.assertTrue("Non-numeric error not displayed when non-numeric value entered in Max Quantity text box.", editModal.isNonNumericMaxQuantityErrorDisplayed());

        //Enter a value greater than 8 in the Maximum Number of Cells Per Vehicle text box.
        editModal.setMaxCellsPerVehicle(Integer.toString(RandomNumberGenerator.nextInteger(100) + 8));

        //Verify an error is displayed associated with the Max Quantity text box indicating the maximum acceptable value is 8.
        Assert.assertTrue("Invalid Range error not displayed with Max Quantity text box when value greater than 8 is used.", editModal.isMaxQuantityInvalidRangeErrorDisplayed());

        //Enter a value of 0 or greater, but less than the value of the Min Quantity in the Max Quantity text box.
        editModal.setMinCellsPerVehicle("5");
        editModal.setMaxCellsPerVehicle("2");
        editModal.clickUpdate(false);

        //Verify an error is displayed associated with the Max Quantity text box indicating the maximum cells per vehicle must be greater than or equal to the minimum cells per vehicle.
        Assert.assertTrue("Overall Invalid Range error not displayed with Max Quantity text box when value in Max Quanity is less than value in Min Quantity.",
                editModal.isInvalidMinMaxRangeErrorDisplayed());

        //Enter a decimal numerical value greater than the Min Quantity between 0 and 8 (i.e., ‘3.5’) in the Max Quantity text box.
        editModal.setMaxCellsPerVehicle("2.5");

        //Click anywhere outside the Max Quantity text box.
        editModal.setMinCellsPerVehicle(testCellDevice);

        //Verify the value is automatically rounded to the closest whole integer.
        Assert.assertEquals("Max Quantity value is not rounded as expected.", "3", editModal.getDisplayedMaxCellsPerVehicle());

        //Enter a non-numerical value in the Percentage text box (e.g., ‘---’).
        editModal.setCellularPercentage("---");

        //Verify an error is displayed associated with the Percentage text box indicating the value entered is not a valid number.
        Assert.assertTrue("Non-numeric error not displayed when non-numeric value entered in Percentage text box.", editModal.isNonNumericPercentageErrorDisplayed());

        //Enter a value less than 1 in the Percentage text box.
        editModal.setCellularPercentage(Integer.toString(RandomNumberGenerator.nextInteger(1)));

        //Verify an error is displayed associated with the Percentage text box indicating the minimum acceptable value is 1.
        Assert.assertTrue("Invalid Percentage Range error not displayed with Percentage text box when value entered is less than 1.", editModal.isInvalidPercentageRangeErrorDisplayed());

        //Enter a value greater than 100 in the Percentage text box.
        editModal.setCellularPercentage(Integer.toString(RandomNumberGenerator.nextInteger() + 101));

        //Verify an error is displayed associated with the Percentage text box indicating the maximum acceptable value is 100.
        Assert.assertTrue("Invalid Percentage Range error not displayed with Percentage text box when value entered is greater than 100.", editModal.isInvalidPercentageRangeErrorDisplayed());

        //Enter valid Min and Max Quantity values
        editModal.setMinCellsPerVehicle(testCellDevice);
        editModal.setMaxCellsPerVehicle(testCellDevice);

        //Enter a valid numerical value between 1 and 100 in the Percentage text box.
        editModal.setCellularPercentage(Integer.toString(RandomNumberGenerator.nextInteger(60))); //upper bound set to 60 to ensure total percentage of all devices does not exceed 100%

        //Click the Update button.
        editModal.clickUpdate(true);

        //Verify the cellular device has been updated in the Cellular Devices list.
        Assert.assertTrue("Cellular Device Profile is not present after clicking create.", cellTab.isCellularDeviceDisplayed(testCellDeviceName));

        //Click the Edit button.
        cellTab.clickEdit();

        //Enter a valid name in the Name text box.
        editModal.setCellularDeviceName(RandomStringGenerator.nextLetterString(5));

        //Enter a value between 1 and 100 in the Percentage text box that will result in the combined total percentage of all cellular devices to exceed 100%.
        editModal.setCellularPercentage("90");

        //Verify an error is displayed associated with the Percentage text box indicating the percentage total must not exceed 100%.
        Assert.assertTrue("Error message indicating combined percentage total exceeds maximum percentage is not displayed when total percentages for all cellular devices exceeds 100%",
                editModal.isCombinedPercentageExceedsMaxErrorText());

        //Click the Cancel button.
        editModal.clickCancel();

        //Click the Close button.
        cellTab.clickCloseBtn();

        //Logout
        simPage.logout(testUser);
    }
}
