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
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal.Btn;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;

/**
 * Test class which executes steps for the Delete a Detector test,
 * TC-055/ITC-036
 *
 * @author rsmith
 */
public class DeleteDetectorTest extends ETexasAfterTestResetTestBase {

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
     * The ID associated with the detector
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
     * Test steps for TC-055
     */
    @Test
    public void deleteDetectorExternalTest() {
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

        //Verify the Delete button is enabled.
        Assert.assertTrue("The delete button is not enabled as expected when a detector is selected.", detectorTab.isDetectorDeleteBtnEnabled());

        //Click the Delete button.
        ConfirmDeleteModal deleteWarning = detectorTab.clickDeleteDetectorBtn();

        //Verify a Confirm Delete modal is displayed with text confirming deletion of the selected detector.
        deleteWarning.checkConfirmDeleteHeader();
        deleteWarning.checkDeleteWarningDetectorContent(detector);

        //Verify an 'x' icon is displayed in the Confirm Delete modal
        Assert.assertTrue("The 'x' icon is not displayed in Confirm Delete modal as expected.", deleteWarning.isCloseIconDisplayed());

        //Verify that Yes and No buttons are displayed at the bottom of the modal.
        deleteWarning.checkConfirmDeleteBtns();

        //Click the No button.
        deleteWarning.clickBtn(Btn.NO);

        //Verify modal is no longer displayed.
        Assert.assertFalse("Delete Warning window is still displayed after clicking the No button.", deleteWarning.isDetectorDeletionContentDisplayed(detector));

        //Verify the Detector list is unchanged.
        Assert.assertTrue("Detector is not displayed in list after deletion is cancelled.", detectorTab.isDetectorDisplayed(detectorID));

        //Verify detector is selected, if not, select detector
        if (detectorTab.isDetectorRowSelected(detector) == false) {
            detectorTab.selectRow(detectorID, true);
        }

        //Click the Delete button.
        deleteWarning = detectorTab.clickDeleteDetectorBtn();

        //Verify the Delete Warning modal is displayed
        deleteWarning.checkConfirmDeleteHeader();

        //Click the 'x' icon
        deleteWarning.clickCloseIcon();

        //Verify modal is no longer displayed.
        Assert.assertFalse("Delete Warning window is still displayed after clicking 'x' icon.", deleteWarning.isDetectorDeletionContentDisplayed(detector));

        //Verify the detector list is unchanged.
        Assert.assertTrue("Detector is not displayed in list after deletion is cancelled.", detectorTab.isDetectorDisplayed(detectorID));

        //Verify detector is selected, if not, select detector
        if (!detectorTab.isDetectorRowSelected(detector)) {
            detectorTab.selectRow(detectorID, true);
        }

        //Click the Delete button.
        deleteWarning = detectorTab.clickDeleteDetectorBtn();

        //Verify a Confirm Delete modal is displayed
        deleteWarning.checkConfirmDeleteHeader();

        //Click the Yes button.
        deleteWarning.clickBtn(Btn.YES);
        detectorTab.waitUntilLoaded();

        //Verify modal is no longer displayed.
        Assert.assertFalse("Delete Warning window is still displayed after clicking the Yes button.", deleteWarning.isDetectorDeletionContentDisplayed(detector));

        //Verify the detector is no longer displayed in the Detectors list.
        Assert.assertFalse("Detector is still displayed in list after deletion.", detectorTab.isDetectorDisplayed(detectorID));

        //Click Close button
        detectorTab.clickClose();

        //Log out
        simPage.logout(testUser);
    }

    //TODO - needs to be updated based on UI changes

    //	/**
    //	 * Test steps for TC-055
    //	 */
    //	@Test
    //	public void deleteDetectorInternalTest() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");
    //		//Ensure Simulations page is loaded
    //		SimulationsPage simPage = getPage(SimulationsPage.class);
    //		simPage.waitUntilLoaded();
    //
    //		//Select multiple existing simulations.
    //		simPage.selectAllSimulations(true);
    //
    //		//Verify the Configure button is disabled.
    //		Assert.assertFalse("The configure button is not disabled as expected when more than one simulation is selected.", simPage.isSimBtnEnabled(SimBtns.CONFIGURE_BTN));
    //
    //		//Select an existing simulation with existing executions.
    //		simPage.selectAllSimulations(false);
    //		simPage.selectSimCheckBox(itcTestSim, true);
    //
    //		//Verify the Configure button is disabled.
    //		Assert.assertFalse("The configure button is not disabled as expected when a simulation with an existing execution is selected.", simPage.isSimBtnEnabled(SimBtns.CONFIGURE_BTN));
    //
    //		//De-select the execution.
    //		simPage.selectSimCheckBox(itcTestSim, false);
    //
    //		//Select an execution with no existing executions and at least one detector.
    //		simPage.selectSimCheckBox(testSim, true);
    //
    //		//Hover over the Configure button and click the Environment option.
    //		simPage.clickEdit();
    //		SimulationSettingsModal environmentPage = simPage.clickCompositeSettings();
    //
    //		//Verify the Detectors tab is selected.
    //		ConfigureDetectorsPartialPage detectorForm = environmentPage.clickDetectorsTab();
    //
    //		//Select a listed detector.
    //		detectorForm.selectDetectorCheckBox(detector, true);
    //
    //		//Click the Delete button.
    //		DeleteWarningForm deleteForm = detectorForm.clickDelete();
    //
    //		//Click the Yes button to confirm deletion.
    //		deleteForm.clickBtn(Btn.YES);
    //
    //		//Verify the detector is no longer displayed.
    //		getPage(SimulationsPage.class);
    //		Assert.assertFalse("Delete Warning window still displayed after clicking 'Yes'.", deleteForm.isDetectorDeletionContentDisplayed());
    //		Assert.assertFalse("Detector is still displayed after confirming deletion", detectorForm.isDetectorRowDisplayed(detector));
    //
    //		//Click Close in Detectors window and logout
    //		detectorForm.clickCloseBtn();
    //		simPage.logout(testUser);
    //	}
}