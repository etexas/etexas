package com.harmonia.qa.ETEXASWebQATests.SimulationManagementTests;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.Simulation.SimulationType;
import com.harmonia.qa.ETEXASWebQATests.entities.UploadedSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage.SimBtns;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulations.CreateSimulationFromUploadModal;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class which executes steps for the Create A New Composite with Uploaded
 * Simulation Test, TC-100, ITC-076
 *
 * @author llaroussini
 */
public class CreateANewCompositeWithUploadedSimulation extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Simulation object used for testing
     */
    private UploadedSimulation simulation;

    /**
     * Composite object used for testing
     */
    private CompositeSimulation composite;

    /**
     * The actual file used in testing
     */
    private File testFile;

    /**
     * File name of the jar to use in testing
     */
    private String testFileName = "testtempclass1.zip";

    /**
     * The invalid (non-zip) file which will be uploaded
     */
    private File nonZipFile;

    /**
     * The name of the invalid (non-zip) file which will be uploaded
     */
    private String nonZipFileName = "msg-rx-app-2.0-SNAPSHOT.jar";

    /**
     * The invalid (non-TEXAS zip) file which will be uploaded
     */
    private File nonTEXASZipFile;

    /**
     * The name of the invalid (non-TEXAS zip) file which will be uploaded
     */
    private String nonTEXASZipFileName = "eTEXASTestResults.zip";

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get user and simulation/composite for testing
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        simulation = SimulationFactory.getUploadedSimulation(testUser, true); //get a random template simulation
        simulation.setSimType(SimulationType.ETEXAS);
        composite = simulation.getComposite();
        ETexasEntityManager.addEntities(testUser, simulation, composite);

        //TEXAS Zip File Exists
        testFile = FileUtils.getFile("src", "test", "resources", testFileName);

        //Invalid file (non-zip) exists -- for ITC
        nonZipFile = FileUtils.getFile("src", "target", "resources", nonZipFileName);

        //Invalid file (non-TEXAS zip) exists -- for ITC
        nonTEXASZipFile = FileUtils.getFile("src", "target", "resources", nonTEXASZipFileName);

        //Register user
        LandingPage landingPage = ETexasUserUtils.userRegistration(testUser);

        //Login
        landingPage.loginAs(testUser);
    }

    /**
     * Test steps for TC-100
     *
     * @throws IOException if errors are encountered when reading information
     *         about the test file (likely a result of permissions issues)
     */
    @Test
    public void newCompositeFromUploadExternal() throws IOException {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Verify the Create button is enabled.
        Assert.assertTrue("The Create button is not enabled as expected upon intial load of Simulations page.", simPage.isBtnEnabled(SimBtns.CREATE_BTN));

        //Click the Create button.
        simPage.clickCreate();

        //Verify Template and Upload options are displayed.
        simPage.checkCreateOptions();

        //Click the Upload option.
        CreateSimulationFromUploadModal uploadModal = simPage.clickUpload();

        //Verify a Create Simulation from Upload modal is displayed.
        uploadModal.checkUploadSimHeader();

        //Verify the following icons are displayed in the top right corner of the modal: ‘?’ and ‘x’.
        uploadModal.checkUploadSimHeaderIcons();

        //Click the ‘?’ icon.
        uploadModal.clickUploadSimHelp();

        //Verify a Create Simulation from Upload Help modal is displayed with instructions for creating a simulation from upload.
        uploadModal.checkHelpModal();

        //Verify an OK button is displayed.
        Assert.assertTrue("OK button could not be found in Create Simulation from Upload Help modal.", uploadModal.isHelpOKBtnDisplayed());

        //Click the OK button.
        uploadModal.clickHelpOKBtn();

        //Verify the Help modal closes.
        Assert.assertFalse("Create Simulation from Upload Help modal is still displayed after clicking OK.", uploadModal.isCreateSimUploadHelpHeaderDisplayed());

        //Verify the following fields are displayed in the Create Simulation from Upload modal: Composite text box, Select button, Name text box, Upload text box, Browse button, TEXAS Type radio button, and Playback Type radio button.
        uploadModal.checkFields();

        //Verify the following buttons are displayed at the bottom of the Create Simulation from Upload modal: Create, Reset, and Cancel.
        uploadModal.checkBtns();

        //Enter a name for the composite in the Composite text box.
        uploadModal.setCompositeName(composite);

        //Enter a name for the simulation into the Name text box.
        uploadModal.setSimName(simulation);

        //Click the Browse button. (omitted due to the nature of Selenium interaction - we set the upload file directly rather than using the 'upload'/file browser dialog)
        //Verify a file dialog is displayed. (omitted, this cannot be tested in automation)
        //Select an TEXAS project zip file.
        uploadModal.setUploadFile(testFile);

        /*
         * TODO Verify the zip file path is populated in the Upload field
         * (Omitted - there is no good way to check this as neither the 'value'
         * or text of the field actually update as far as Selenium is concerned)
         * - attempting to access the value directly with javascript might be an
         * option, however I was unsuccessful in my attempts to obtain the value
         * in that way.
         */

        //Select the TEXAS radio button.
        uploadModal.clickTexasRadio();

        //Click the Reset button.
        uploadModal.clickReset();

        //Verify all values are returned to their default state.
        uploadModal.checkDisplayedValues("", "");
        //TODO Omitting the check of the file name since this value is not updated in a way which can be checked

        //Enter valid values in all fields.
        uploadModal.setAllFields(simulation, testFile);

        //Click the Cancel button.
        uploadModal.clickCancel();

        //Verify the Create Simulation from Upload modal is no longer displayed.
        Assert.assertFalse("Create Simulation from Upload modal is still displayed after clicking Cancel.", uploadModal.isCreateSimUploadHeaderDisplayed());

        //Verify neither the composite nor individual simulation are displayed in the list of simulations.
        Assert.assertFalse("New composite is displayed despite cancelling creation.", simPage.isCompositeDisplayed(composite));
        Assert.assertFalse("New simulation is displayed despite cancelling creation.", simPage.isSimDisplayed(simulation));

        //Click the Create button.
        simPage.clickCreate();

        //Click the Upload option.
        simPage.clickUpload();

        //Verify default values are displayed in all fields.
        uploadModal.checkDisplayedValues("", "");
        //TODO Omitting the check of the file name since this value is not updated in a way which can be checked

        //Enter valid values in all fields.
        uploadModal.setAllFields(simulation, testFile);

        //Click the ‘x’ icon.
        uploadModal.clickCloseIcon();

        //Verify the Create Simulation from Upload modal is no longer displayed.
        Assert.assertFalse("Create Simulation from Upload modal is still displayed after clicking Close.", uploadModal.isCreateSimUploadHeaderDisplayed());

        //Verify neither the composite nor individual simulation are displayed in the list of simulations.
        Assert.assertFalse("New composite is displayed despite closing create modal.", simPage.isCompositeDisplayed(composite));
        Assert.assertFalse("New simulation is displayed despite closing create modal.", simPage.isSimDisplayed(simulation));

        //Click the Create button.
        simPage.clickCreate();

        //Click the Upload option.
        simPage.clickUpload();

        //Verify default values are displayed in all fields.
        uploadModal.checkDisplayedValues("", "");
        //TODO Omitting the check of the file name since this value is not updated in a way which can be checked

        //Enter valid values in all fields.
        uploadModal.setAllFields(simulation, testFile);

        //Click the Create button.
        uploadModal.clickCreate(true);

        //Verify the Create Simulation from Upload modal disappears.
        Assert.assertFalse("Create Simulation from Upload modal is still displayed after clicking Create.", uploadModal.isCreateSimUploadHeaderDisplayed());

        //Verify the newly created composite and individual simulation are displayed in the list of simulations.
        Assert.assertTrue("New composite is not displayed despite successful creation.", simPage.isCompositeDisplayed(composite));
        Assert.assertTrue("New simulation is not displayed despite successful creation.", simPage.isSimDisplayed(simulation));

        //Verify the entered composite name and individual simulation name are displayed in the Name column.
        Assert.assertTrue("New composite name could not be found.", simPage.isCompositeNameDisplayed(composite.getName()));
        Assert.assertTrue("New simulation name could not be found.", simPage.isSimDisplayed(simulation.getName()));

        //Verify unique IDs are auto-generated and displayed for the composite and individual simulation in the ID column.
        Assert.assertNotNull("An ID was not assigned to the new composite as expected.", simPage.getCompositeID(composite));
        Assert.assertNotNull("An ID was not assigned to the new simulation as expected.", simPage.getSimID(simulation));

        //Verify the individual simulation type of TEXAS is displayed in the Type column.
        Assert.assertEquals("The value in the Type column for the newly created simulation is not displayed as expected.", simulation.getSimType().getLabel(), simPage.getSimType(simulation));

        //Verify the text “Uploaded File” is associated with the individual simulation and displayed in the Source column.
        Assert.assertEquals("The value in the Source column for the newly created simulation is not displayed as expected.", "Uploaded File", simPage.getSimSource(simulation));

        //Log out
        simPage.logout(testUser);
    }

    /**
     * Test steps for ITC-076
     *
     * @throws IOException if errors are encountered when reading information
     *         about the test file (likely a result of permissions issues)
     */
    @Test
    public void newCompositeFromUploadInternal() throws IOException {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Click the Create button.
        simPage.clickCreate();

        //Click the Upload link.
        CreateSimulationFromUploadModal uploadModal = simPage.clickUpload();

        //With all fields blank, click the Create button in the Create Simulation from Upload modal.
        uploadModal.clickCreate(false);

        //Verify an error is displayed associated with the Composite text box indicating a valid composite is required.
        Assert.assertTrue("Composite Name required error is not displayed as expected when attempting to create a simulation with all fields blank.",
                uploadModal.isCompositeNameRequiredErrorDisplayed());

        //Verify an error is displayed associated with the Name text box indicating a valid simulation name is required.
        Assert.assertTrue("Simulation Name required error is not displayed as expected when attempting to create a simulation with all fields blank.",
                uploadModal.isSimulationNameRequiredErrorDisplayed());

        //Verify an error is displayed associated with the Upload text box indicating a valid zip file is required.
        Assert.assertTrue("Upload required error is not displayed as expected when attempting to create a simulation with all fields blank.", uploadModal.isUploadRequiredErrorDisplayed());

        //Enter a one more space characters in the Composite text box and enter valid values in the remaining fields
        uploadModal.setAllFields(simulation, testFile);
        uploadModal.setCompositeName("   ");

        //Click the Create button.
        uploadModal.clickCreate(false);

        //Verify an error is displayed associated with the Composite text box indicating a valid composite is required.
        Assert.assertTrue("Composite Name required error is not displayed as expected when whitespace only is used in Composite Name text box.", uploadModal.isCompositeNameRequiredErrorDisplayed());

        //Enter a value in the Composite text box that contains special characters, not including dashes (e.g., ‘T3$t’).
        uploadModal.setCompositeName(RandomStringGenerator.nextLetterString(5) + "&$*#)");

        //Verify an error is displayed associated with the Composite text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Composite Name error is not displayed as expected when Composite name contains special characters.", uploadModal.isCompositeNameInvalidErrorDisplayed());

        //Enter a value in the Composite text box that contains consecutive spaces within the entered text (e.g., ‘Te    st’).
        uploadModal.setCompositeName(RandomStringGenerator.nextLetterString(5) + "  " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Composite text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Composite Name error is not displayed as expected when Composite name contains consecutive spaces.", uploadModal.isCompositeNameInvalidErrorDisplayed());

        //Enter a value in the Composite text box that begins with a space (e.g., ‘ Test’).
        uploadModal.setCompositeName(" " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Composite text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/Trailing Whitespace error is not displayed as expected when Composite name contains leading whitespace.",
                uploadModal.isCompositeNameLeadingTrailingSpacesErrorDisplayed());

        //Enter a value in the Composite text box that ends with a space (e.g., ‘Test ’).
        uploadModal.setCompositeName(RandomStringGenerator.nextLetterString(5) + " ");

        //Verify an error is displayed associated with the Composite text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/Trailing Whitespace error is not displayed as expected when Composite name contains trailing whitespace.",
                uploadModal.isCompositeNameLeadingTrailingSpacesErrorDisplayed());

        //Enter a value in the Name text box that begins with a hyphen/dash (e.g., ‘-Test’).
        uploadModal.setCompositeName("-" + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Composite Name error is not displayed as expected when Composite name contains a leading dash ('-') character.", uploadModal.isCompositeNameInvalidErrorDisplayed());

        //Enter a value in the Name text box that ends with a hyphen/dash (e.g., ‘Test-’).
        uploadModal.setCompositeName(RandomStringGenerator.nextLetterString(5) + "-");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Composite Name error is not displayed as expected when Composite name contains a trailing dash ('-') character.", uploadModal.isCompositeNameInvalidErrorDisplayed());

        //Enter a valid name in the Composite text box.
        uploadModal.setCompositeName(composite);

        //Enter one or more space characters in the Name text box.
        uploadModal.setSimName("   ");

        //Verify an error is displayed associated with the Name text box indicating a valid simulation name is required.
        Assert.assertTrue("Simulation Name required error is not displayed as expected when whitespace only is used in Simulation Name text box.", uploadModal.isSimulationNameRequiredErrorDisplayed());

        //Enter a value in the Name text box that contains special characters, not including dashes (e.g., ‘T3$t’).
        uploadModal.setSimName(RandomStringGenerator.nextLetterString(5) + "&$*#)");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Simulation Name error is not displayed as expected when Simulation name contains special characters.", uploadModal.isSimulationNameInvalidErrorDisplayed());

        //Enter a value in the Name text box that contains consecutive spaces within the entered text (e.g., ‘Te    st’).
        uploadModal.setSimName(RandomStringGenerator.nextLetterString(5) + "  " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Simulation Name error is not displayed as expected when Simulation name contains consecutive spaces.", uploadModal.isSimulationNameInvalidErrorDisplayed());

        //Enter a value in the Name text box that begins with a space (e.g., ‘ Test’).
        uploadModal.setSimName(" " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/Trailing Whitespace error is not displayed as expected when Simulation name contains leading whitespace.",
                uploadModal.isSimulationNameLeadingTrailingSpacesErrorDisplayed());

        //Enter a value in the Name text box that ends with a space (e.g., ‘Test ’).
        uploadModal.setSimName(RandomStringGenerator.nextLetterString(5) + " ");

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/Trailing Whitespace error is not displayed as expected when Simulation name contains trailing whitespace.",
                uploadModal.isSimulationNameLeadingTrailingSpacesErrorDisplayed());

        //Enter a value in the Name text box that begins with a hyphen/dash (e.g., ‘-Test’).
        uploadModal.setSimName("-" + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Simulation Name error is not displayed as expected when Simulation name contains a leading dash ('-') character.",
                uploadModal.isSimulationNameInvalidErrorDisplayed());

        //Enter a value in the Name text box that ends with a hyphen/dash (e.g., ‘Test-’).
        uploadModal.setSimName(RandomStringGenerator.nextLetterString(5) + "-");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Simulation Name error is not displayed as expected when Simulation name contains a trailing dash ('-') character.",
                uploadModal.isSimulationNameInvalidErrorDisplayed());

        //Enter a valid name in the Name text box.
        uploadModal.setSimName(simulation);

        //Click the Browse button and select a non-zip file.
        uploadModal.setUploadFile(nonZipFile);

        //Click the Create button.
        uploadModal.clickCreate(false);

        //Verify an error is displayed associated with the Upload text box indicating the field does not support non-zip files.
        Assert.assertTrue("Invalid .zip file error is not displayed as expected when a non-zip file is attempted to be uploaded.", uploadModal.isInvalidFileUploadErrorDisplayed());

        //Click the Browse button and select a zip file that is not associated with TEXAS or Playback.
        uploadModal.setUploadFile(nonTEXASZipFile);

        //Disabled due to BUG 12029
        //Click the Create button.
        //uploadModal.clickCreate(false);
        //Verify an error is displayed associated with the Upload text box indicating the field does not support the selected file.
        //Assert.assertTrue("Invalid .zip file error is not displayed as expected when a non-TEXAS zip file is attempted to be uploaded.", uploadModal.isInvalidFileUploadErrorDisplayed());

        //Click the Browse button and select a TEXAS zip file.
        uploadModal.setUploadFile(testFile);

        //Click the Create button.
        uploadModal.clickCreate(true);

        //Verify the newly created composite is displayed in the simulation list.
        Assert.assertTrue("New composite is not displayed despite successful creation.", simPage.isCompositeDisplayed(composite));

        //Verify the newly created composite is selected, if not, select the newly created composite.
        if (!simPage.isCompositeSelected(composite)) {
            simPage.selectComposite(composite, true);
        }

        //Click the Create button.
        simPage.clickCreate();

        //Click the Upload link.
        simPage.clickUpload();

        //Enter the same name used in previous steps in the Name text box.
        uploadModal.setSimName(simulation);

        //Click the Browse button and select a TEXAS project zip file.
        uploadModal.setUploadFile(testFile);

        //Click the Create button.
        uploadModal.clickCreate(false);

        //Verify an error is displayed associated with the Name text box indicating the simulation name must be unique.
        Assert.assertTrue("Duplicate Simulation Name error not displayed as expected when name is used that matches the name of a simulation that already exists in the selected composite.",
                uploadModal.isDuplicateSimulationNameErrorDisplayed(simulation));

        //Click the Cancel button.
        uploadModal.clickCancel();

        //Logout
        simPage.logout(testUser);
    }
}
