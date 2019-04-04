package com.harmonia.qa.ETEXASWebQATests.SimulationManagementTests;

import java.io.File;
import java.io.IOException;

import junit.framework.Assert;

import org.apache.commons.io.FileUtils;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.Simulation.SimulationType;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.UploadedSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulations.CreateSimulationFromUploadModal;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class for TC-006 and ITC-005- Create a new uploaded simulation from an
 * uploaded TEXAS zip file.
 *
 * @author cbulloss
 * @author llaroussini
 */
public class CreateANewUploadedSimulationInACompositeTest extends ETexasAfterTestResetTestBase {

    /**
     * The user used in the test case.
     */
    private ETexasUser testuser;

    /**
     * Existing composite used in test case
     */
    private CompositeSimulation composite;

    /**
     * Additional existing composite used in test case
     */
    private CompositeSimulation composite2;

    /**
     * Simulation which will be created during the test
     */
    private UploadedSimulation simulation;

    /**
     * Additional simulation which will be created during the test
     */
    private UploadedSimulation simulation2;

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
     * Test setup, prerequisites
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //User registered
        testuser = ETexasUserFactory.getUser(true); //Get a random user.
        ETexasUserUtils.userRegistration(testuser);
        ETexasEntityManager.addEntity(testuser);

        //Create multiple composite simulations
        TemplateSimulation exisitingSim = SimulationFactory.getTemplateSimulation(testuser, true);
        ETexasSimulationUtils.createTemplateSimulation(exisitingSim);
        composite = exisitingSim.getComposite();
        TemplateSimulation exisitingSim2 = SimulationFactory.getTemplateSimulation(testuser, true);
        ETexasSimulationUtils.createTemplateSimulation(exisitingSim2);
        composite2 = exisitingSim2.getComposite();

        //User logged in, on simulations page
        ETexasCommonUtils.goToLandingPage().loginAs(testuser);

        //Instantiate the test simulation
        simulation = SimulationFactory.getUploadedSimulation(testuser, true);
        simulation.setSimType(SimulationType.ETEXAS);
        simulation2 = SimulationFactory.getUploadedSimulation(testuser, true);
        simulation2.setSimType(SimulationType.ETEXAS);
        String sim2Name = simulation2.getName();
        ETexasEntityManager.addEntities(simulation, simulation2);

        //TEXAS Zip File Exists
        testFile = FileUtils.getFile("src", "test", "resources", testFileName);

        //Invalid file (non-zip) exists -- for ITC
        nonZipFile = FileUtils.getFile("src", "target", "resources", nonZipFileName);
        //Invalid file (non-TEXAS zip) exists -- for ITC
        nonTEXASZipFile = FileUtils.getFile("src", "target", "resources", nonTEXASZipFileName);
    }

    /**
     * Test steps for TC-006
     *
     * @throws IOException if errors are encountered when reading information
     *         about the test file (likely a result of permissions issues)
     */
    @Test
    public void newSimUploadExternal() throws IOException {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select a composite name from the Simulations list.
        simPage.selectComposite(composite, true);

        //Verify that the Create, Edit, and Delete buttons are enabled.
        simPage.checkEnabledSimulationBtns();

        //Click the Create button.
        simPage.clickCreate();

        //Verify Template and Upload options are displayed.
        simPage.checkCreateOptions();

        //Click the Upload option.
        CreateSimulationFromUploadModal uploadForm = simPage.clickUpload();

        //Verify a Create Simulation from Upload modal is displayed.
        uploadForm.checkUploadSimHeader();

        //Verify the following icons are displayed in the top right corner of the modal: ‘?’ and ‘x’.
        uploadForm.checkUploadSimHeaderIcons();

        //Click the ‘?’ icon.
        uploadForm.clickUploadSimHelp();

        //Verify a Create Simulation from Upload Help modal is displayed with instructions for creating a simulation from upload.
        uploadForm.checkHelpModal();

        //Verify an OK button is displayed in the modal.
        Assert.assertTrue("The Ok button is not displayed on the help window.", uploadForm.isHelpOKBtnDisplayed());

        //Click the OK button.
        uploadForm.clickHelpOKBtn();

        //Verify that the Create Simulation from Upload Help modal closes.
        Assert.assertFalse("Help window still displayed after clicking ok.", uploadForm.isCreateSimUploadHelpHeaderDisplayed());

        //Verify the following fields are displayed in the Create Simulation from Upload modal: Composite text box, Select button, Name text box, Upload text box, Browse button, TEXAS Type radio button, and Playback Type radio button and verify the following buttons are displayed at the bottom of the Create Simulation from Upload modal: Create, Reset, and Cancel.
        uploadForm.checkFields();

        //Verify the Composite text box is pre-filled with the selected composite name.
        String compositeName = composite.getName();
        Assert.assertEquals("Displayed composite name does not match expected name.", compositeName, uploadForm.getCompositeName());

        //Enter a name for the simulation in the Name text box.
        uploadForm.setSimName(simulation);

        //click browse (omitted due to the nature of Selenium interaction - we set the upload file directly rather than using the 'upload'/file browser dialog)
        //verify file dialog displays (omitted, this cannot be tested in automation)
        //select a Texas project zip file
        uploadForm.setUploadFile(testFile);

        //TODO Verify the zip file path is populated in the Upload field (Omitted - there is no good way to check this as neither the 'value' or text of the field actually update as far as Selenium is concerned)

        //Select the TEXAS radio button.
        uploadForm.clickTexasRadio();

        //Click the Reset button.
        uploadForm.clickReset();

        //Verify all values are returned to their default state.
        uploadForm.checkDisplayedValues(compositeName, "");
        //TODO Omitting the check of the file name since this value is not updated in a way which can be checked

        //Enter a name for the simulation in the Name text box.
        uploadForm.setSimName(simulation);

        //Click browse and select a Texas project Zip file (Clicking browse omitted in automated testing)
        uploadForm.setUploadFile(testFile);

        //Select the TEXAS radio button.
        uploadForm.clickTexasRadio();

        //Click the Cancel button.
        simPage = uploadForm.clickCancel();

        //Verify the Create Simulation from Upload modal is no longer displayed.
        Assert.assertFalse("Upload simulation form still displayed after clicking cancel.", uploadForm.isHeaderDisplayed(CreateSimulationFromUploadModal.HEADER_TEXT));

        //Verify the simulation is not listed under the composite name.
        simPage.expandComposite(composite, true);
        Assert.assertFalse("Simulation named '" + simulation.getName() + "'is displayed despite clicking cancel.", simPage.isSimDisplayed(simulation));

        //Click the Create button.
        simPage.clickCreate();

        //Click the Upload option.
        uploadForm = simPage.clickUpload();

        //Verify the Create Simulation from Upload modal is displayed.
        uploadForm.checkUploadSimHeader();

        //Verify the Composite text box is pre-filled with the selected composite name.
        Assert.assertEquals("Displayed composite name does not match expected name.", compositeName, uploadForm.getCompositeName());

        //Enter a name for the simulation in the Name text box.
        uploadForm.setSimName(simulation.getName());

        //Click browse and select a Texas project Zip file (Clicking browse omitted in automated testing)
        uploadForm.setUploadFile(testFile);

        //Select the TEXAS radio button.
        uploadForm.clickTexasRadio();

        //Click the ‘x’ icon.
        simPage = uploadForm.clickCloseIcon();

        //Verify the Create Simulation from Upload modal is no longer displayed.
        Assert.assertFalse("Upload simulation form still displayed after clicking close.", uploadForm.isCreateSimUploadHeaderDisplayed());

        //Verify the simulation is not listed under the composite name.
        Assert.assertFalse("Simulation named '" + simulation.getName() + "'is displayed despite clicking the close icon.", simPage.isSimDisplayed(simulation)); //note: composite only needs to be expanded once, therefore step not repeated here

        //Click the Create button.
        simPage.clickCreate();

        //Click the Upload option.
        uploadForm = simPage.clickUpload();

        //Verify the Create Simulation from Upload modal is displayed.
        Assert.assertTrue("Upload Simulation form not displayed.", uploadForm.isCreateSimUploadHeaderDisplayed());

        //Verify the Composite text box is pre-filled with the selected composite name.
        Assert.assertEquals("Displayed composite name does not match expected name.", compositeName, uploadForm.getCompositeName());

        //Enter a name for the simulation in the Name text box.
        uploadForm.setSimName(simulation.getName());

        //Click browse and select a Texas project Zip file (Clicking browse omitted in automated testing)
        uploadForm.setUploadFile(testFile);

        //Select the TEXAS radio button.
        uploadForm.clickTexasRadio();

        //Click the Create button.
        simPage = uploadForm.clickCreate(true);

        //Verify the Create Simulation from Upload modal is no longer displayed.
        Assert.assertFalse("Upload simulation form still displayed after clicking create.", uploadForm.isCreateSimUploadHeaderDisplayed());

        //Verify that the simulation is displayed under the selected composite.
        Assert.assertTrue("Simulation named '" + simulation.getName() + "' is not displayed after successful creation.", simPage.isSimDisplayed(simulation)); //note: composite only needs to be expanded once, therefore step not repeated here

        //Select any other composite.
        simPage.selectComposite(composite2, true);

        //Click the Create button.
        simPage.clickCreate();

        //Click the Upload option.
        uploadForm = simPage.clickUpload();

        //Verify the Create Simulation from Upload modal is displayed.
        Assert.assertTrue("Upload Simulation form not displayed.", uploadForm.isCreateSimUploadHeaderDisplayed());

        //Click the Select button and verify a list of all the user’s composites is displayed.
        uploadForm.checkCompositeList();

        //Select a different composite name.
        uploadForm.selectComposite(composite);

        //Enter a name for the simulation into the Name text box.
        uploadForm.setSimName(simulation2.getName());

        //Click browse and select a Texas project Zip file (Clicking browse omitted in automated testing)
        uploadForm.setUploadFile(testFile);

        //Select the TEXAS radio button.
        uploadForm.clickTexasRadio();

        //Click the Reset button.
        uploadForm.clickReset();

        //Verify all values are returned to their default state.
        Assert.assertEquals("Name field did not reset correctly after clicking the reset button.", CreateSimulationFromUploadModal.DEFAULT_NAME_TEXT, uploadForm.getSimName());
        //TODO Omitting the check of the file name since this value is not updated in a way which can be checked

        //Verify the Composite text box is pre-filled with the selected composite name.
        String compositeName2 = composite2.getName();
        Assert.assertEquals("Displayed composite name does not match expected name.", compositeName2, uploadForm.getCompositeName());

        //Click the Select button and select a different composite name.
        uploadForm.selectComposite(composite);

        //Enter a name for the simulation into the Name text box.
        String sim2Name = simulation2.getName();
        uploadForm.setSimName(sim2Name);

        //Click browse and select a Texas project Zip file (Clicking browse omitted in automated testing)
        uploadForm.setUploadFile(testFile);

        //Select the TEXAS radio button.
        uploadForm.clickTexasRadio();

        //Click the Cancel button.
        uploadForm.clickCancel();

        //Verify the Create Simulation from Upload modal is no longer displayed.
        Assert.assertFalse("Upload simulation form still displayed after clicking cancel.", uploadForm.isCreateSimUploadHeaderDisplayed());

        //Verify the simulation is not listed under the composite name.
        Assert.assertFalse("Simulation named '" + sim2Name + "' is displayed despite clicking cancel.", simPage.isSimDisplayed(simulation2)); //note: composite only needs to be expanded once, therefore step not repeated here

        //Click the Create button.
        simPage.clickCreate();

        //Click the Upload option.
        uploadForm = simPage.clickUpload();

        //Verify the Composite text box is pre-filled with the selected composite name.
        Assert.assertEquals("Displayed composite name does not match expected name.", compositeName2, uploadForm.getCompositeName());

        //Click the Select button and select a different composite name.
        uploadForm.selectComposite(composite);

        //Enter a name for the simulation into the Name text box.
        uploadForm.setSimName(sim2Name);

        //Click browse and select a Texas project Zip file (Clicking browse omitted in automated testing)
        uploadForm.setUploadFile(testFile);

        //Select the TEXAS radio button.
        uploadForm.clickTexasRadio();

        //Click the ‘x’ icon.
        simPage = uploadForm.clickCloseIcon();

        //Verify the Create Simulation from Upload modal is no longer displayed.
        Assert.assertFalse("Upload simulation form still displayed after clicking the close icon.", uploadForm.isCreateSimUploadHeaderDisplayed());

        //Verify the simulation is not listed under the composite name.
        Assert.assertFalse("Simulation named '" + sim2Name + "'is displayed despite clicking the close icon.", simPage.isSimDisplayed(simulation2)); //note: composite only needs to be expanded once, therefore step not repeated here

        //Click the Create button.
        simPage.clickCreate();

        //Click the Upload option.
        uploadForm = simPage.clickUpload();

        //Verify the Composite text box is pre-filled with the selected composite name.
        Assert.assertEquals("Displayed composite name does not match expected name.", compositeName2, uploadForm.getCompositeName());

        //Click the Select button and select a different composite name.
        uploadForm.selectComposite(composite);

        //Enter a name for the simulation into the Name text box.
        uploadForm.setSimName(sim2Name);

        //Click browse and select a Texas project Zip file (Clicking browse omitted in automated testing)
        uploadForm.setUploadFile(testFile);

        //Select the TEXAS radio button.
        uploadForm.clickTexasRadio();

        //Click the Create button.
        uploadForm.clickCreate(true);

        //Verify the Create Simulation from Upload modal is no longer displayed.
        Assert.assertFalse("Upload simulation form still displayed after clicking create.", uploadForm.isCreateSimUploadHeaderDisplayed());

        //Verify the new simulation is listed under the selected composite.
        Assert.assertTrue("Simulation named '" + simulation2.getName() + "' is not displayed after successful creation.", simPage.isSimDisplayed(simulation2)); //note: composite only needs to be expanded once, therefore step not repeated here

        //Logout
        simPage.logout(testuser);

    }

    /**
     * Test steps for ITC-005
     *
     * @throws IOException if errors are encountered when reading information
     *         about the test file (likely a result of permissions issues)
     */
    //TODO re-implement once updated against eTEXAS version 3.0
    //@Test
    public void newSimUploadInternal() throws IOException {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");
        //Verify Simulation page is displayed
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Click the new button
        simPage.clickCreate();

        //Click upload
        CreateSimulationFromUploadModal uploadForm = simPage.clickUpload();

        //With name field blank, click Create
        uploadForm.clickCreate(false);
        //Verify field required error is displayed associated with Name text box
        uploadForm.checkSimNameFieldRequiredErrorDisplayed();
        uploadForm.checkUploadFieldRequiredErrorDisplayed();

        //Enter whitespace only in Name text box, upload a valid file, and click Create
        uploadForm.setSimName("    ");
        uploadForm.setUploadFile(testFile);
        uploadForm.clickCreate(false);
        //Verify field required error is displayed associated with Name text box
        uploadForm.checkSimNameWhitespaceErrorDisplayed();

        //Enter invalid name (contains special characters), click Create
        uploadForm.setSimName("Te$t!");
        uploadForm.clickCreate(false);
        //Verify error message displayed associated with Name text box
        Assert.assertTrue("Invalid Sim Name error tooltip could not be found after entering a simulation name with special characters.", uploadForm.isSimulationNameInvalidErrorDisplayed());

        //Enter invalid name (contains consecutive spaces), click Create
        uploadForm.setSimName("Te  st");
        uploadForm.clickCreate(false);
        //Verify error message displayed associated with Name text box
        Assert.assertTrue("Invalid Sim Name error tooltip could not be found after entering a simulation name with consecutive spaces between characters.",
                uploadForm.isSimulationNameInvalidErrorDisplayed());

        //Enter invalid name (contains leading space), click Create
        uploadForm.setSimName(" Test");
        uploadForm.clickCreate(false);
        //Verify leading/trailing space error message displayed associated with Name text box
        uploadForm.checkSimNameWhitespaceErrorDisplayed();

        //Enter invalid name (contains trailing space), click Create
        uploadForm.setSimName("Test ");
        uploadForm.clickCreate(false);
        //Verify leading/trailing space error message displayed associated with Name text box
        uploadForm.checkSimNameWhitespaceErrorDisplayed();

        //Enter invalid name (contains leading dash), click Create
        uploadForm.setSimName("-Test");
        uploadForm.clickCreate(false);
        //Verify leading/trailing space error message displayed associated with Name text box
        Assert.assertTrue("Invalid Sim Name error tooltip could not be found after entering a simulation name with a leading dash.", uploadForm.isSimulationNameInvalidErrorDisplayed());

        //Enter invalid name (contains trailing dash), click Create
        uploadForm.setSimName("Test-");
        uploadForm.clickCreate(false);
        //Verify leading/trailing space error message displayed associated with Name text box
        Assert.assertTrue("Invalid Sim Name error tooltip could not be found after entering a simulation name with a trailing dash.", uploadForm.isSimulationNameInvalidErrorDisplayed());

        //Enter valid simulation name, upload an invalid file (non-zip), and click create
        String newSim = RandomStringGenerator.nextLetterString(10);
        uploadForm.setSimName(newSim);
        //uploadForm.setUploadFile(nonZipFile);  - - BUG 12463
        //uploadForm.clickCreateNoRtrn();
        //Assert.assertTrue("Invalid File Upload error tooltip could not be found after uploading a non-zip file.", uploadForm.isInvalidFileUploadErrorDisplayed());
        //Upload an invalid file (non-TEXAS zip), and click create
        //uploadForm.setUploadFile(nonTEXASZipFile);
        //uploadForm.clickCreateNoRtrn();
        //Verify error message is displayed associated with Upload text box
        //Assert.assertTrue("Invalid zip file error could not be found after uploading a non-TEXAS zip file.", uploadForm.isInvalidFileUploadErrorDisplayed());

        //Upload valid file, click create, and verify new simulation is displayed
        uploadForm.setUploadFile(testFile);
        uploadForm.clickCreate(true);
        //Assert.assertTrue("New uploaded simulation is not displayed in list.", simPage.isSimDisplayed(newSim)); - BUG 12788

        //Log out
        simPage.logout(testuser);
    }
}
