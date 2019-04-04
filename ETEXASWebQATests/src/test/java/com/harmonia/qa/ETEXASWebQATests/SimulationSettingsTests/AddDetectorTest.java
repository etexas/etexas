package com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.Detector;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.Lane;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.DetectorFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.enums.LaneID;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.parital.environment.ConfigureDetectorsPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.parital.environment.CreateDetectorModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.SelectDetectorLaneModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomNumberGenerator;

/**
 * Test class which executes steps for the Add a Detector test, TC-054
 *
 * @author llaroussini
 * @author rsmith
 */
public class AddDetectorTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Simulation object used throughout the test case
     */
    private TemplateSimulation testSim;

    /**
     * Simulation object used throughout the internal test case
     */
    private TemplateSimulation itcTestSim;

    /**
     * Detector object used throughout the test case
     */
    private Detector detector;

    /**
     * Composite object used throughout the test case
     */
    private CompositeSimulation testComposite;

    /**
     * Test setup
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get test user, test simulation, test composite, and detector
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        testSim = ETexasEntityManager.getTemplateSimulation(defaultEx05Sim);
        testSim.setUser(testUser);
        testComposite = testSim.getComposite();
        detector = DetectorFactory.getDetector(true); //gets a random detector
        Lane lane = ETexasEntityManager.getLane(LaneID.FIFTEEN);
        detector.setLane(lane);
        ETexasEntityManager.addEntities(testUser, testSim, detector);

        //Create additional sim to be used in ITC
        itcTestSim = SimulationFactory.getTemplateSimulation(testUser, true); //gets random simulation
        ETexasEntityManager.addEntities(itcTestSim);

        //Register user and create new simulation from template
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulation(testSim);
        ETexasSimulationUtils.createTemplateSimulation(itcTestSim);

        //Start execution for internal test case
        //ETexasExecutionUtils.createNewExecution(itcTestSim); //TODO update once executions completed in version 3.0

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-054
     */
    @Test
    public void addDetectorExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Ensure Simulations page is loaded
        SimulationsPage simPage = getPage(SimulationsPage.class);
        simPage.waitUntilLoaded();

        //Expand Composite
        simPage.expandComposite(testComposite, true);

        //Select simulation
        simPage.selectSim(testSim, true);

        //Click the Edit button
        simPage.clickEdit();

        //Click the Simulation Settings Option
        simPage.clickSimulationSettings();
        SimulationSettingsModal simulationSettingsModal = getPage(SimulationSettingsModal.class);

        //Verify the Simulation Settings Modal is displayed
        Assert.assertTrue("Simulation Settings Modal not displayed after clicking the Simulation Settings Option.", simulationSettingsModal.isSimSettingsHeaderDisplayed());

        //Click the Detectors tab
        ConfigureDetectorsPartialPage detectorForm = simulationSettingsModal.clickDetectorsTab();

        //Verify the following buttons are displayed across the top of the Detectors tab: Create, edit, delete.
        detectorForm.checkBtns();

        //Verify the Create, Edit and Delete buttons are disabled.
        Assert.assertTrue("The Create button is not enabled by default as expected.", detectorForm.isDetectorCreateBtnEnabled());
        Assert.assertTrue("The Edit button is not disabled by default as expected.", detectorForm.isDetectorEditBtnEnabled());
        Assert.assertTrue("The Delete button is not disabled by default as expected.", detectorForm.isDetectorDeleteBtnEnabled());

        //Verify a table is displayed listing the ID, Lane, Width (cm), Height (cm), and Distance from Stop Line (cm).
        detectorForm.checkDetectorColumnHeaders();

        //Verify the table displays a list of detectors configured in the selected simulation, if any exist.
        int listSize = testSim.getDetectors().size();
        if (listSize == 0) {
            Assert.assertFalse("List of detectors was found despite no detectors being associated with the simulation", detectorForm.isDetectorTableDisplayed());
        }
        else {
            Assert.assertTrue("List of detectors was not found despite dectectors being associated with the simulation", detectorForm.isDetectorTableDisplayed());
        }

        //Verify a Close button is displayed at the bottom of the modal
        Assert.assertTrue("Close button not displayed in window as expected.", detectorForm.isCloseBtnDisplayed());

        //Click the Create Button
        CreateDetectorModal createDetectorModal = detectorForm.clickCreateDetectorBtn();

        //Verify the Create Detector modal is displayed.
        Assert.assertTrue("Add Detector header not displayed as expected.", createDetectorModal.isCreateDetectorHeaderDisplayed());

        //Verify that a ‘?’ icon and an ‘x’ icon are displayed in the upper right corner of the modal
        createDetectorModal.checkAddDetectorHeaderIcons();

        //Click the ‘?’ icon.
        createDetectorModal.clickHelpIcon();

        //Verify that Create Detector Help modal is displayed with instructions for adding a detector
        createDetectorModal.checkHelpModal();

        //Verify an OK button is displayed in the modal
        Assert.assertTrue("OK button not displayed as expected in Create Detector Help modal.", createDetectorModal.isHelpOKBtnDisplayed());

        //Click the OK button.
        createDetectorModal.clickHelpOKBtn();

        //Verify the Help modal closes.
        Assert.assertFalse("Create Detector header still displayed after clicking OK button.", createDetectorModal.isCreateDetectorHelpHeaderDisplayed());

        //Verify the following fields are displayed in the Add Detector modal: Lane dropdown, Width, Height, and Distance.
        createDetectorModal.checkFieldsDisplayed();

        //Verify a Show Lanes button is displayed next to the lane dropdown
        Assert.assertTrue("Show Lanes button could not be found.", createDetectorModal.isShowLanesBtnDisplayed());

        //Verify the following buttons are displayed at the bottom of the modal: Create, Reset, and Cancel.
        createDetectorModal.checkBtns();

        //Click the Lane Dropdown and verify options are displayed for the values 1-16
        createDetectorModal.checkLaneOptions();

        //Select any Value
        createDetectorModal.selectLane(detector);

        //Verify the Selected value is displayed in the lane dropdown
        String laneID = detector.getLane().getLaneID().getLabel();
        Assert.assertEquals("The Lane ID," + laneID + "is not displayed as expected.", laneID, createDetectorModal.getDisplayedLane());

        //Click the Show Lanes button.
        SelectDetectorLaneModal selectDetectorLane = createDetectorModal.clickShowAllLanesBtn();

        //Verify a Select Detector lane Modal is displayed.
        Assert.assertTrue("Select Detector Lane header is not displayed as expected", selectDetectorLane.isSelectDetectorLaneHeaderDisplayed());

        //Verify a Close button is displayed at the bottom of the Modal.
        Assert.assertTrue("Close button could not be found.", selectDetectorLane.isCloseBtnDisplayed());

        //Verify the following columns are displayed: ID, Approach, Type, Movements,Speed Limit (m/s, and Nodes (x,y,z,w) (cm).
        selectDetectorLane.checkSelectDetectorLaneModalColumnHeaders();

        //Verify all lane IDs 1-16 are displayed and columns are populated for each lane ID.
        selectDetectorLane.checkAllLanes(testSim);

        //Select any lane
        selectDetectorLane.selectDetectorLane(detector);

        //Click Close button
        selectDetectorLane.clickCloseIcon();

        //Verify the selected value is displayed in the lane dropdown
        createDetectorModal.checkSelectedLane(detector.getLane());

        //Enter valid values in remaining fields
        createDetectorModal.setDistance(detector);
        createDetectorModal.setHeight(detector);
        createDetectorModal.setWidth(detector);

        //Click the Reset button.
        createDetectorModal.clickReset();

        //Verify the text boxes are reset to their default values.
        createDetectorModal.checkFieldValues("", "", "", "");

        //Enter valid values in all text boxes.
        createDetectorModal.setAllFields(detector);

        //Click the Cancel button.
        createDetectorModal.clickCancel();

        //Verify the Create Detector modal closes.
        Assert.assertFalse("Create Detector Header is still displayed after canceling.", createDetectorModal.isCreateDetectorHeaderDisplayed());

        //Verify the new detector is not listed in the Detectors table.
        Assert.assertFalse("Detector is displayed in list after being canceled.", detectorForm.isDetectorDisplayed(detector.getDistance())); //TODO If a unique value is assigned to detectors in the UI in the future, make sure it is being used.

        //Click the Create button again.
        createDetectorModal = detectorForm.clickCreateDetectorBtn();
        Assert.assertTrue("Create Detector modal not displayed as expected.", createDetectorModal.isCreateDetectorHeaderDisplayed());

        //Enter valid values in all text boxes.
        createDetectorModal.setAllFields(detector);

        //Click the Close icon
        createDetectorModal.clickCloseIcon();

        //Verify the Create Detector modal closes.
        Assert.assertFalse("Create Detector Modal is still displayed after canceling.", createDetectorModal.isCreateDetectorHeaderDisplayed());

        //Click the Create button again.
        createDetectorModal = detectorForm.clickCreateDetectorBtn();

        //Enter valid values in all text boxes.
        createDetectorModal.setAllFields(detector);

        //Click the Create button.
        createDetectorModal.clickCreate(true);

        //Verify the lane, width, height, and distance values match the values entered previously.
        Assert.assertTrue("Detector Lane value is not displayed in list after being created.", detectorForm.isDetectorDisplayed(detector.getLane().getLaneID().getLabel()));
        Assert.assertTrue("Detector width value is not displayed in list after being created.", detectorForm.isDetectorDisplayed(detector.getWidth()));
        Assert.assertTrue("Detector height value is not displayed in list after being created.", detectorForm.isDetectorDisplayed(detector.getHeight()));
        Assert.assertTrue("Detector distance value is not displayed in list after being created.", detectorForm.isDetectorDisplayed(detector.getDistance()));

        //Verify the newly created detector has an auto-generated ID displayed in the ID column
        Assert.assertNotNull("An ID was not generated for the newly created Detector device.", detectorForm.getDetectorID(detector));

        //Click close
        detectorForm.clickClose();

        //Log out
        simPage.logout(testUser);
    }

    /**
     * Test steps for ITC-035
     */
    @Test
    public void addDetectorInternalTest() {

        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");

        //Ensure Simulations page is loaded
        SimulationsPage simPage = getPage(SimulationsPage.class);
        simPage.waitUntilLoaded();

        //Expand Composite
        simPage.expandComposite(testComposite, true);

        //Select simulation
        simPage.selectSim(testSim, true);

        //Click the Edit button
        simPage.clickEdit();

        //Click the Simulation Settings Option
        SimulationSettingsModal simulationSettingsModal = simPage.clickSimulationSettings();

        //Click the Detectors tab
        ConfigureDetectorsPartialPage detectorForm = simulationSettingsModal.clickDetectorsTab();

        //Click the Add button.
        CreateDetectorModal addForm = detectorForm.clickCreateDetectorBtn();

        //With all fields blank, click the Create button.
        addForm.clickCreate(false);

        //Verify field required errors display with all text boxes.
        addForm.checkRequiredFieldErrorAllFields();

        //Enter a valid value in the Height,Distance, and Select a lane from the lane drop down.
        addForm.setHeight(detector);
        addForm.setDistance(detector);
        addForm.selectLane(detector);

        //Enter a value greater than 400 in the Width text box.
        addForm.setWidth(Integer.toString(RandomNumberGenerator.nextInteger() + 401));
        addForm.clickCreate(false);

        //Verify an error is displayed associated with the Width text box indicating the maximum value accepted is 400.
        Assert.assertTrue("Width error not displayed as expected when value over 400 is entered in Width text box.", addForm.isDetectorWidthNumberErrorDisplayed());

        //Enter a value less than 1 in the Width text box.
        addForm.setWidth("0");
        addForm.clickCreate(false);

        //Verify an error is displayed associated with the Width text box indicating the minimum acceptable value is 1.
        Assert.assertTrue("Width error not displayed as expected when value less than 1 is entered in Width text box.", addForm.isDetectorWidthNumberErrorDisplayed());

        //Enter a non-alphanumeric value in the Width text box (e.g., ‘---‘).
        addForm.setWidth("---");

        //Verify an error is displayed associated with the Width text box indicating the value must be numeric.
        Assert.assertTrue("Width error is not displayed as expected when Width text box contains a non-numeric value.", addForm.isInvalidWidthErrorDisplayed());

        //Enter a valid value in the Width text box.
        addForm.setWidth(detector);

        //Enter a value greater than 800 in the Height text box.
        addForm.setHeight(Integer.toString(RandomNumberGenerator.nextInteger() + 801));
        addForm.clickCreate(false);

        //Verify an error is displayed associated with the Height text box indicating the maximum value accepted is 800.
        Assert.assertTrue("Maximum Height error not displayed as expected when value over 800 is entered in Height text box.", addForm.isDetectorHeightNumberErrorDisplayed());

        //Enter a value less than 1 in the Height text box.
        addForm.setHeight("0");
        addForm.clickCreate(false);

        //Verify an error is displayed associated with the Height text box indicating the minimum acceptable value is 1.
        Assert.assertTrue("Minimum Height error not displayed as expected when value less than 1 is entered in Height text box.", addForm.isDetectorHeightNumberErrorDisplayed());

        //Enter a non-alphanumeric value in the Height text box (e.g., ‘---‘).
        addForm.setHeight("---");

        //Verify an error is displayed associated with the Height text box indicating the value must be numeric.
        Assert.assertTrue("Height error is not displayed as expected when Height text box contains a non-numeric value.", addForm.isInvalidNonNumericHeightErrorDisplayed());

        //Enter a valid value in the Height text box.
        addForm.setHeight(detector);

        //Enter a value greater than 25000 in the Distance from Stop Line text box.
        addForm.setDistance(Integer.toString(RandomNumberGenerator.nextInteger() + 25001));
        addForm.clickCreate(false);

        //Verify an error is displayed associated with the Distance text box indicating the maximum value accepted is 25000.
        Assert.assertTrue("Maximum Distance error not displayed as expected when value over 25000 is entered in Distance from Stop Line text box.", addForm.isDetectorDistanceNumberErrorDisplayed());

        //Enter a negative value in the Distance text box.
        addForm.setDistance("-" + Integer.toString(RandomNumberGenerator.nextInteger()));

        //Verify an error is displayed associated with the Distance text box indicating the value cannot be negative.
        Assert.assertTrue("Minimum Distance from Stop Line error not displayed as expected when value less than 0 is entered in Distance from Stop Line text box.",
                addForm.isDetectorDistanceNumberErrorDisplayed());

        //Enter a non-alphanumeric value in the Distance text box (e.g., ‘---‘).
        addForm.setDistance("---");

        //Verify an error is displayed associated with the Distance text box indicating the value must be numeric.
        Assert.assertTrue("Distance error is not displayed as expected when Distance text box contains a non-numeric value.", addForm.isInvalidNonNumericDistanceErrorDisplayed());

        //Enter a valid value in the Distance text box and click the Create button.
        addForm.setDistance(detector);
        addForm.clickCreate(true);

        // Verify the detector is added to the detectors list.
        String id = detectorForm.getDetectorID(detector);
        detector.setID(id);
        Assert.assertTrue("Detector row was not found after clicking 'Add' in Add Detector window.", detectorForm.isDetectorRowDisplayed(detector));

        //Click the Close button.
        detectorForm.clickClose();

        //Logout
        simPage.logout(testUser);

    }
}
