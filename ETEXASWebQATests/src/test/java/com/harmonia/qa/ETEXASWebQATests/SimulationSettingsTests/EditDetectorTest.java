package com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.Detector;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.DetectorFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.parital.environment.ConfigureDetectorsPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.SelectDetectorLaneModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.EditDetectorModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomNumberGenerator;

/**
 * Test class which executes steps for the Edit a Detector test, TC-097/ITC-074
 *
 * @author rsmith
 */
public class EditDetectorTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Simulation object used throughout the test case
     */
    private TemplateSimulation testSim;

    /**
     * Detector object used throughout the test case
     */
    private Detector detector;

    /**
     * Composite object used throughout the test case
     */
    private CompositeSimulation testComposite;

    /**
     * The ID associated with the detector object
     */
    private String detectorID;

    /**
     * Test setup
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");
        //Get test user, test simulation, and test detector device
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        testSim = ETexasEntityManager.getTemplateSimulation(defaultEx05Sim);
        testSim.setUser(testUser);
        testComposite = testSim.getComposite();
        detector = DetectorFactory.getDetector(true); //get a random detector
        ETexasEntityManager.addEntities(testUser, testSim, testComposite, detector);

        //Register user and create new simulation from template with detector
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulationWithDetector(testSim, detector);
        detectorID = detector.getID();

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-097
     */
    @Test
    public void editDetectorExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select simulation
        simPage.expandComposite(testComposite, true);
        simPage.selectSim(testSim, true);

        //Click Edit
        simPage.clickEdit();
        simPage.checkEditOptions();

        //Click Simulation Settings option and verify Simulation Settings modal displays
        SimulationSettingsModal simSettings = simPage.clickSimulationSettings();

        //Click the Detectors tab.
        ConfigureDetectorsPartialPage detectorTab = simSettings.clickDetectorsTab();

        //Select an existing detector.
        detectorTab.selectRow(detectorID, true);
        Assert.assertTrue("The detector with an ID of " + detectorID + " is not selected as expected.", detectorTab.isDetectorRowSelected(detector));

        //Verify the Edit Button is enabled.
        detectorTab.isDetectorEditBtnEnabled();

        //Click the Edit button.
        EditDetectorModal editDetectorForm = detectorTab.clickEdit();

        //Verify the Edit Detector modal is displayed.
        Assert.assertTrue("Edit Detector header not displayed as expected.", editDetectorForm.isEditDetectorHeaderDisplayed());

        //Verify that a ‘?’ icon and an ‘x’ icon are displayed in the upper right corner of the modal
        editDetectorForm.checkEditDetectorHeaderIcons();

        //Click the ‘?’ icon.
        editDetectorForm.clickEditDetectorHelp();

        //Verify that Edit Detector Help modal is displayed with instructions for adding a detector
        editDetectorForm.checkHelpModal();

        //Verify an OK button is displayed in the modal
        Assert.assertTrue("OK button not displayed as expected in Edit Detector Help modal.", editDetectorForm.isEditDetectorHelpOKBtnDisplayed());

        //Click the OK button.
        editDetectorForm.clickEditDetectorHelpOKBtn();

        //Verify the Help modal closes.
        Assert.assertFalse("Edit Detector header still displayed after clicking OK button.", editDetectorForm.isEditDetectorHelpHeaderDisplayed());

        //Verify the following fields are displayed in the Edit Detector modal: Lane dropdown, Width, Height, and Distance.
        editDetectorForm.checkFieldsDisplayed();

        //Verify a Show Lanes button is displayed next to the lane dropdown
        Assert.assertTrue("Show Lanes button could not be found.", editDetectorForm.isShowLanesBtnDisplayed());

        //Verify the fields are populated with the detector's associated information.
        editDetectorForm.checkFieldValues(detector);

        //Verify the following buttons are displayed at the bottom of the modal: Create, Reset, and Cancel.
        editDetectorForm.checkEditDetectorBtns();

        //Click the Lane Dropdown and verify options are displayed for the values 1-16
        editDetectorForm.checkLaneOptions();

        //Updated values to set for Detector and static values
        Detector newDetector = DetectorFactory.getDetector(true);
        String newLaneID = newDetector.getLane().getLaneID().getLabel();
        String newHeight = newDetector.getHeight();
        String newWidth = newDetector.getWidth();
        String newDistance = newDetector.getDistance();
        String originalHeight = detector.getHeight();

        //Select a new lane
        editDetectorForm.selectLane(newDetector);

        //Verify the Selected value is displayed in the lane dropdown
        Assert.assertEquals("The Lane ID," + newLaneID + " is not displayed as expected.", newLaneID, editDetectorForm.getDisplayedLane());

        //Click the Show Lanes button.
        SelectDetectorLaneModal selectDetectorLane = editDetectorForm.clickShowAllLanesBtn();

        //Verify a Select Detector lane Modal is displayed.
        Assert.assertTrue("Select Detector Lane header is not displayed as expected", selectDetectorLane.isSelectDetectorLaneHeaderDisplayed());

        //Verify a Close button is displayed at the bottom of the Modal.
        Assert.assertTrue("Close button could not be found.", selectDetectorLane.isSelectDetectorLaneCloseBtnDisplayed());

        //Verify the following columns are displayed: ID, Approach, Type, Movements,Speed Limit (m/s, and Nodes (x,y,z,w) (cm).
        selectDetectorLane.checkSelectDetectorLaneModalColumnHeaders();

        //Verify all lane IDs 1-16 are displayed and columns are populated for each lane ID. //TODO implement this step. Xpath's wont currently work with this modal.
        // selectDetectorLane.checkAllLanes(testSim);

        //Select any lane //TODO implement this step. Currently not working, stating that the element is unclickable.
        //selectDetectorLane.selectDetectorLane(detector);

        //Click Close button
        selectDetectorLane.clickCloseIcon();

        //Verify the selected value is displayed in the lane dropdown
        editDetectorForm.checkSelectedLane(newDetector.getLane());

        //Enter valid values in remaining fields
        editDetectorForm.setDistance(newDetector);
        editDetectorForm.setHeight(newDetector);
        editDetectorForm.setWidth(newDetector);

        //Click the Reset button.
        editDetectorForm.clickReset();

        //Verify the text boxes are reset to their default values.
        editDetectorForm.checkFieldValues(detector);

        //Enter valid values in all text boxes.
        editDetectorForm.setAllFields(newDetector);

        //Click the Cancel button.
        editDetectorForm.clickCancel();

        //Verify the Edit Detector modal closes.
        Assert.assertFalse("Create Detector Header is still displayed after canceling.", editDetectorForm.isCreateDetectorHeaderDisplayed());

        //Verify the detector is still displayed in the detector list.
        Assert.assertTrue("Original Detector is not displayed in list after update is canceled.", detectorTab.isDetectorDisplayed(originalHeight));

        //Verify the detector is selected, if not, select the detector
        if (detectorTab.isDetectorRowSelected(detector) == false) {
            detectorTab.selectRow(originalHeight, true);
        }

        //Click the Edit button again. Verify the Edit Detector Modal is displayed
        editDetectorForm = detectorTab.clickEdit();
        Assert.assertFalse("Edit Detector modal not displayed as expected.", editDetectorForm.isCreateDetectorHeaderDisplayed());

        //Enter valid values in all text boxes.
        editDetectorForm.setAllFields(newDetector);

        //Click the Close icon
        editDetectorForm.clickCloseIcon();

        //Verify the Edit Detector modal closes.
        Assert.assertFalse("Edit Detector Modal is still displayed after closing.", editDetectorForm.isCreateDetectorHeaderDisplayed());

        //Verify the detector is still displayed in the detector list.
        Assert.assertTrue("Original Detector is not displayed in list after update is canceled.", detectorTab.isDetectorDisplayed(originalHeight));

        //Verify the detector is selected, if not, select the detector
        if (detectorTab.isDetectorRowSelected(detector) == false) {
            detectorTab.selectRow(originalHeight, true);
        }

        //Click the Edit button again.
        editDetectorForm = detectorTab.clickEdit();

        //Enter valid values in all text boxes.
        editDetectorForm.setAllFields(newDetector);

        //Click the Update button.
        editDetectorForm.clickUpdate(true);

        //Verify the Edit Detector modal is no longer displayed
        Assert.assertFalse("Edit Detector Modal is still displayed after selecting update.", editDetectorForm.isCreateDetectorHeaderDisplayed());

        //Verify the original detector is no longer displayed in the detector list.
        Assert.assertFalse("Original Detector is still displayed in list after update.", detectorTab.isDetectorDisplayed(originalHeight));

        //Verify the updated detector is displayed with the associated updated information in the detectors list.
        Assert.assertTrue("Updated Detector is not displayed in list after update.", detectorTab.isDetectorDisplayed(newHeight));

        //Verify the lane, width, height, and distance values match the values entered previously.
        Assert.assertTrue("Detector Lane value is not displayed in list after being created.", detectorTab.isDetectorDisplayed(newLaneID));
        Assert.assertTrue("Detector width value is not displayed in list after being created.", detectorTab.isDetectorDisplayed(newWidth));
        Assert.assertTrue("Detector height value is not displayed in list after being created.", detectorTab.isDetectorDisplayed(newHeight));
        Assert.assertTrue("Detector distance value is not displayed in list after being created.", detectorTab.isDetectorDisplayed(newDistance));

        //Verify the detector ID is unchanged.
        Assert.assertEquals("The ID of the updated Detector did not remain static.", detectorID, detectorTab.getDetectorID(newDetector));

        //Update entity manager
        Detector originalDetector = ETexasEntityManager.getDetector(originalHeight);
        originalDetector.setDistance(newDistance);
        originalDetector.setWidth(newWidth);
        originalDetector.setLane(newDetector.getLane());
        originalDetector.setHeight(newHeight);

        //Click Close
        detectorTab.clickClose();

        //Log out
        simPage.logout(testUser);
    }

    /**
     * Test steps for ITC-074
     */
    @Test
    public void editDetectorInternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select simulation
        simPage.expandComposite(testComposite, true);
        simPage.selectSim(testSim, true);

        //Click Edit
        simPage.clickEdit();

        //Click Simulation Settings option and verify Simulation Settings modal displays
        SimulationSettingsModal simSettings = simPage.clickSimulationSettings();

        //Click the Detectors tab.
        ConfigureDetectorsPartialPage detectorTab = simSettings.clickDetectorsTab();

        //Select an existing detector.
        detectorTab.selectRow(detectorID, true);

        //Click the Edit button
        EditDetectorModal editModal = detectorTab.clickEdit();

        //Delete the pre-populated values
        editModal.setAllFields("", "", "", "");

        //Click the Update Button
        editModal.clickUpdate(false);

        //Verify field required errors display with all text boxes.
        editModal.checkRequiredFieldErrorAllFields();

        //Enter a valid value in the Height,Distance, and Select a lane from the lane drop down.
        Detector newDetector = DetectorFactory.getDetector(true);
        editModal.setHeight(newDetector);
        editModal.setDistance(newDetector);
        editModal.selectLane(newDetector);

        //Enter a value greater than 400 in the Width text box.
        editModal.setWidth(Integer.toString(RandomNumberGenerator.nextInteger() + 401));
        editModal.clickUpdate(false);

        //Verify an error is displayed associated with the Width text box indicating the maximum value accepted is 400.
        Assert.assertTrue("Width error not displayed as expected when value over 400 is entered in Width text box.", editModal.isDetectorWidthNumberErrorDisplayed());

        //Enter a value less than 1 in the Width text box.
        editModal.setWidth("0");
        editModal.clickUpdate(false);

        //Verify an error is displayed associated with the Width text box indicating the minimum acceptable value is 1.
        Assert.assertTrue("Width error not displayed as expected when value less than 1 is entered in Width text box.", editModal.isDetectorWidthNumberErrorDisplayed());

        //Enter a non-alphanumeric value in the Width text box (e.g., ‘---‘).
        editModal.setWidth("---");

        //Verify an error is displayed associated with the Width text box indicating the value must be numeric.
        Assert.assertTrue("Width error is not displayed as expected when Width text box contains a non-numeric value.", editModal.isInvalidWidthErrorDisplayed());

        //Enter a valid value in the Width text box.
        editModal.setWidth(newDetector.getWidth());

        //Enter a value greater than 800 in the Height text box.
        editModal.setHeight(Integer.toString(RandomNumberGenerator.nextInteger() + 801));
        editModal.clickUpdate(false);

        //Verify an error is displayed associated with the Height text box indicating the maximum value accepted is 800.
        Assert.assertTrue("Maximum Height error not displayed as expected when value over 800 is entered in Height text box.", editModal.isDetectorHeightNumberErrorDisplayed());

        //Enter a value less than 1 in the Height text box.
        editModal.setHeight("0");
        editModal.clickUpdate(false);

        //Verify an error is displayed associated with the Height text box indicating the minimum acceptable value is 1.
        Assert.assertTrue("Minimum Height error not displayed as expected when value less than 1 is entered in Height text box.", editModal.isDetectorHeightNumberErrorDisplayed());

        //Enter a non-alphanumeric value in the Height text box (e.g., ‘---‘).
        editModal.setHeight("---");

        //Verify an error is displayed associated with the Height text box indicating the value must be numeric.
        Assert.assertTrue("Height error is not displayed as expected when Height text box contains a non-numeric value.", editModal.isInvalidNonNumericHeightErrorDisplayed());

        //Enter a valid value in the Height text box.
        editModal.setHeight(newDetector.getHeight());

        //Enter a value greater than 25000 in the Distance from Stop Line text box.
        editModal.setDistance(Integer.toString(RandomNumberGenerator.nextInteger() + 25001));
        editModal.clickUpdate(false);

        //Verify an error is displayed associated with the Distance text box indicating the maximum value accepted is 25000.
        Assert.assertTrue("Maximum Distance error not displayed as expected when value over 25000 is entered in Distance from Stop Line text box.", editModal.isDetectorDistanceNumberErrorDisplayed());

        //Enter a negative value in the Distance text box.
        editModal.setDistance("-" + Integer.toString(RandomNumberGenerator.nextInteger()));

        //Verify an error is displayed associated with the Distance text box indicating the value cannot be negative.
        Assert.assertTrue("Minimum Distance from Stop Line error not displayed as expected when value less than 0 is entered in Distance from Stop Line text box.",
                editModal.isDetectorDistanceNumberErrorDisplayed());

        //Enter a non-alphanumeric value in the Distance text box (e.g., ‘---‘).
        editModal.setDistance("---");

        //Verify an error is displayed associated with the Distance text box indicating the value must be numeric.
        Assert.assertTrue("Distance error is not displayed as expected when Distance text box contains a non-numeric value.", editModal.isInvalidNonNumericDistanceErrorDisplayed());

        //Enter a valid value in the Distance text box and click the Create button.
        editModal.setDistance(newDetector.getDistance());
        editModal.clickUpdate(true);

        //Verify the detector is updated in the Detectors table and all updated values are displayed in the associated columns.
        newDetector.setID(detector.getID());
        Assert.assertTrue("Detector was not updated in the detectors table with the associated data.", detectorTab.isDetectorRowDisplayed(newDetector));

        //Update Entity Manager
        detector = ETexasEntityManager.getDetector(detector.getHeight());
        detector.setLane(newDetector.getLane());
        detector.setWidth(newDetector.getWidth());
        detector.setHeight(newDetector.getHeight());
        detector.setDistance(newDetector.getDistance());

        //Click the Close button.
        detectorTab.clickClose();

        //Logout
        simPage.logout(testUser);

    }
}
