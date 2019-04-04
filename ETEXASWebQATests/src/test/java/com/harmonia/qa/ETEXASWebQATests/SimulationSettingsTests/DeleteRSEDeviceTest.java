package com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests;

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
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal.Btn;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.ConfigureRSEDevicesPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;

/**
 * Test class which executes steps for the Delete a Road Side Equipment Device
 * Test, TC-066 & ITC-047
 *
 * @author llaroussini
 */
public class DeleteRSEDeviceTest extends ETexasAfterTestResetTestBase {

    /**
     * The user used in the test case
     */
    private ETexasUser testuser;

    /**
     * The simulation used in the test case.
     */
    private TemplateSimulation simulation;

    /**
     * The composite used in the test case.
     */
    private CompositeSimulation composite;

    /**
     * Simulation object used throughout the internal test case
     */
    private TemplateSimulation itcSim;

    /**
     * The RSE device which will be deleted
     */
    private RSEDevice rseDevice;

    /**
     * The name of the RSE device used in the test case
     */
    private String rseName;

    /**
     * Prerequisite steps/test setup
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //User registered
        testuser = ETexasUserFactory.getUser(true);
        LandingPage landing = ETexasUserUtils.userRegistration(testuser);
        ETexasEntityManager.addEntity(testuser);

        //Simulation created with RSE device
        simulation = SimulationFactory.getTemplateSimulation(testuser, true);
        simulation.setUser(testuser);
        composite = simulation.getComposite();
        rseDevice = RSEDeviceFactory.getRSEDevice(true);
        rseName = rseDevice.getName();
        simulation.addRSEDevice(rseDevice);
        ETexasEntityManager.addEntities(simulation, rseDevice);
        ETexasSimulationUtils.createTemplateSimulationWithRSEDevice(simulation, rseDevice);

        //TODO - re-implement when ITC is updated
        //        //Create additional sim to be used in ITC
        //        itcSim = SimulationFactory.getTemplateSimulation(testuser, true); //gets random simulation
        //        ETexasEntityManager.addEntities(itcSim);
        //        //Create additional simulation with started execution to be used in ITC
        //        ETexasSimulationUtils.createTemplateSimulation(itcSim);
        //        ETexasExecutionUtils.createNewExecution(itcSim);

        //User logged in
        landing.loginAs(testuser);
    }

    /**
     * Test steps for TC-066
     */
    @Test
    public void deleteRSEExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Select a simulation within a composite that has no executions and at least one RSE device.
        SimulationsPage simPage = getPage(SimulationsPage.class);
        simPage.expandComposite(composite, true);
        simPage.selectSim(simulation, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Simulation Settings option and verify the Simulation Settings modal is displayed
        SimulationSettingsModal simSettingsModal = simPage.clickSimulationSettings();

        //Click the RSE Devices tab.
        ConfigureRSEDevicesPartialPage rseTab = simSettingsModal.clickRSEDevicesTab();

        //Verify the RSE device associated with the selected simulation is displayed.
        Assert.assertTrue("RSE device with name: " + rseName + " could not be found.", rseTab.isRSEDisplayed(rseDevice));

        //Select the RSE device.
        rseTab.selectRow(rseName, true);

        //Verify the Delete button is enabled.
        Assert.assertTrue("Delete button is not enabled when an RSE device is selected.", rseTab.isRSEDeleteBtnEnabled());

        //Click the Delete button.
        ConfirmDeleteModal deleteModal = rseTab.clickDeleteBtn();

        //Verify a Confirm Delete modal is displayed with text confirming deletion of the selected RSE device.
        deleteModal.checkDeleteWarningRSEContent(rseDevice);

        //Verify an ‘x’ icon is displayed in the upper right corner of the modal.
        Assert.assertTrue("Close icon not displayed in Confirm Delete modal.", deleteModal.isCloseIconDisplayed());

        //Verify that Yes and No buttons are displayed at the bottom of the modal.
        Assert.assertTrue("Yes button not displayed in Confirm Delete modal.", deleteModal.isBtnDisplayed(Btn.YES));
        Assert.assertTrue("No button not displayed in Confirm Delete modal.", deleteModal.isBtnDisplayed(Btn.NO));

        //Click the No button.
        deleteModal.clickBtn(Btn.NO);

        //Verify Confirm Delete modal is no longer displayed.
        Assert.assertFalse("Confirm Delete modal is still displayed after clicking No.", deleteModal.isConfirmDeleteHeaderDisplayed());

        //Verify the RSE Device table is unchanged.
        Assert.assertTrue("RSE device with name: " + rseName + " is no longer displayed after clicking 'No' in Confirm Delete modal.", rseTab.isRSEDisplayed(rseDevice));

        //Verify the device is still selected, if not, select the device.
        if (!rseTab.isRSERowSelected(rseDevice)) {
            rseTab.selectRow(rseName, true);
        }

        //Click the Delete button.
        deleteModal = rseTab.clickDeleteBtn();

        //Verify the Confirm Delete modal is displayed.
        deleteModal.checkDeleteWarningRSEContent(rseDevice);

        //Click the ‘x’ icon.
        deleteModal.clickCloseIcon();

        //Verify Confirm Delete modal is no longer displayed.
        Assert.assertFalse("Confirm Delete modal is still displayed after clicking Close.", deleteModal.isConfirmDeleteHeaderDisplayed());

        //Verify the RSE Device table is unchanged.
        Assert.assertTrue("RSE device with name: " + rseName + " is no longer displayed after clicking the Close icon in Confirm Delete modal.", rseTab.isRSEDisplayed(rseDevice));

        //Verify the device is still selected, if not, select the device.
        if (!rseTab.isRSERowSelected(rseDevice)) {
            rseTab.selectRow(rseName, true);
        }

        //Click the Delete button.
        deleteModal = rseTab.clickDeleteBtn();

        //Verify the Confirm Delete modal is displayed.
        deleteModal.checkDeleteWarningRSEContent(rseDevice);

        //Click the Yes button.
        deleteModal.clickBtn(Btn.YES);

        //Verify Confirm Delete modal is no longer displayed.
        Assert.assertFalse("Confirm Delete modal is still displayed after clicking Yes.", deleteModal.isConfirmDeleteHeaderDisplayed());

        //Verify the RSE device is no longer displayed in the RSE Devices table.
        rseTab.waitUntilLoaded();
        Assert.assertFalse("RSE device with name: " + rseName + " is still displayed after clicking 'Yes' in Confirm Delete modal.", rseTab.isRSENameDisplayed(rseDevice));

        //Click the Close button.
        rseTab.clickClose();

        //Log out
        simPage.logout(testuser);
    }

    /**
     * Test steps for ITC-047
     */
    //TODO - needs to be updated based on UI changes
    //	@Test
    public void deleteRSEInternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");
        //Ensure Simulations page is loaded
        SimulationsPage simPage = getPage(SimulationsPage.class);
        simPage.waitUntilLoaded();

        //TODO - update for version 3.0
        //        //Select multiple existing simulations.
        //        simPage.selectAllSimulations(true);
        //
        //        //Select an existing simulation with existing executions.
        //        simPage.selectAllSimulations(false);
        //        simPage.selectSim(itcSim, true);

        //De-select the execution.
        simPage.selectSim(itcSim, false);

        //Select an execution with no existing executions and at least one detector.
        simPage.selectSim(simulation, true);

        //Hover over the Configure button and click the Devices option.
        simPage.clickEdit();
        SimulationSettingsModal devicesForm = simPage.clickSimulationSettings();

        //Verify the RSE tab is selected.
        ConfigureRSEDevicesPartialPage rseForm = devicesForm.clickRSEDevicesTab();

        //Select a listed RSE.
        rseForm.selectRow(rseDevice.getName(), true);

        //Click the Delete button.
        ConfirmDeleteModal deleteForm = rseForm.clickDeleteBtn();

        //Click the Yes button to confirm deletion.
        deleteForm.clickBtn(Btn.YES);

        //Verify the RSE device is no longer displayed.
        getPage(SimulationsPage.class);
        Assert.assertFalse("Delete Warning window still displayed after clicking 'Yes'.", deleteForm.isRSEDeletionContentDisplayed(rseDevice));
        Assert.assertFalse("RSE device is still displayed after confirming deletion", rseForm.isRSEDisplayed(rseDevice));

        //Click Close in RSE window and logout
        rseForm.clickClose();
        simPage.logout(testuser);
    }
}