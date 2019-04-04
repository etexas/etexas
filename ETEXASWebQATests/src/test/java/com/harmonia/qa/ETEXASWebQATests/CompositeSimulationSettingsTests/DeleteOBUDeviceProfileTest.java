package com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.OBUDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.OBUDeviceFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal.Btn;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeSettingsModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.ConfigureOBUDeviceProfilesPartialPage;

/**
 * Test class which executes steps for the Delete an OBU Device Profile Test,
 * TC-067 & ITC-048
 *
 * @author llaroussini
 */
public class DeleteOBUDeviceProfileTest extends ETexasAfterTestResetTestBase {

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
     * The OBU device which will be deleted
     */
    private OBUDevice obu;

    /**
     * The name of the OBU device which will be deleted
     */
    private String obuName;

    /**
     * Prerequisite steps/test setup
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get test user, simulation/composite, and OBU device
        testuser = ETexasUserFactory.getUser(true);
        simulation = SimulationFactory.getTemplateSimulation(testuser, true);
        simulation.setUser(testuser);
        composite = simulation.getComposite();
        obu = OBUDeviceFactory.getOBUDevice(true);
        obuName = obu.getName();
        ETexasEntityManager.addEntities(testuser, simulation, obu);

        //Register user and create simulation/composite with OBU
        LandingPage landing = ETexasUserUtils.userRegistration(testuser);
        ETexasSimulationUtils.createTemplateSimulationWithOBUDevice(simulation, obu);

        //TODO - update when ITC is update for version 3.0
        //        //Create additional sim to be used in ITC
        //        itcSim = SimulationFactory.getTemplateSimulation(testuser, true); //gets random simulation
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
    public void deleteOBUExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify simulations page displayed
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select a composite that has no executions and at least one OBU device profile.
        simPage.selectComposite(composite, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Composite Settings option.
        CompositeSettingsModal compositeSettingsModal = simPage.clickCompositeSettings();

        //Verify the Composite Settings modal is displayed.
        Assert.assertTrue("The Composite Settings modal is not displayed after clicking Edit.", compositeSettingsModal.isCompositeSettingsHeaderDisplayed());

        //Click the OBU Device Profiles tab.
        ConfigureOBUDeviceProfilesPartialPage obuTab = compositeSettingsModal.clickOBUTab();

        //Verify the OBU device profile associated with the selected simulation is displayed.
        Assert.assertTrue("OBU Device Profile named: " + obuName + " associated with composite is not displayed as expected.", obuTab.isOBUDisplayed(obu));

        //Select the OBU device profile.
        obuTab.selectRow(obuName, true);

        //Verify the Delete button is enabled.
        Assert.assertTrue("Delete button is not enabled as expected when OBU named: " + obuName + " is selected.", obuTab.isOBUDeleteBtnEnabled());

        //Click the Delete button.
        ConfirmDeleteModal deleteModal = obuTab.clickDeleteBtn();

        //Verify a Confirm Delete modal is displayed with text confirming deletion of the selected OBU device profile.
        deleteModal.checkConfirmDeleteHeader();
        deleteModal.checkDeleteWarningOBUContent(obu);

        //Verify an ‘x’ icon is displayed in the upper right corner of the modal.
        Assert.assertTrue("Close icon is not displayed in the Confirm Delete modal as expected.", deleteModal.isCloseIconDisplayed());

        //Verify that Yes and No buttons are displayed at the bottom of the modal.
        deleteModal.checkConfirmDeleteBtns();

        //Click the No button.
        deleteModal.clickBtn(Btn.NO);

        //Verify Confirm Delete modal is no longer displayed.
        Assert.assertFalse("The Confirm Delete modal is still displayed after clicking No.", deleteModal.isConfirmDeleteHeaderDisplayed());

        //Verify the OBU Device Profiles table is unchanged.
        Assert.assertTrue("The OBU Device Profile named: " + obuName + " is no longer displayed after clicking No in the Confirm Delete modal.", obuTab.isOBUDisplayed(obu));

        //Verify the device profile is still selected, if not, select the device profile.
        if (obuTab.isOBURowSelected(obu) == false) {
            obuTab.selectRow(obuName, true);
        }

        //Click the Delete button.
        obuTab.clickDeleteBtn();

        //Verify the Confirm Delete modal is displayed.
        Assert.assertTrue("Delete button is not enabled as expected when OBU named: " + obuName + " is selected.", obuTab.isOBUDeleteBtnEnabled());

        //Click the ‘x’ icon.
        deleteModal.clickCloseIcon();

        //Verify Confirm Delete modal is no longer displayed.
        Assert.assertFalse("The Confirm Delete modal is still displayed after closing the Confirm Delete Modal.", deleteModal.isConfirmDeleteHeaderDisplayed());

        //Verify the OBU Device Profiles table is unchanged.
        Assert.assertTrue("The OBU Device Profile named: " + obuName + " is no longer displayed after closing the Confirm Delete modal.", obuTab.isOBUDisplayed(obu));

        //Verify the device profile is still selected, if not, select the device profile.
        if (obuTab.isOBURowSelected(obu) == false) {
            obuTab.selectRow(obuName, true);
        }

        //Click the Delete button.
        obuTab.clickDeleteBtn();

        //Verify the Confirm Delete modal is displayed.
        Assert.assertTrue("Delete button is not enabled as expected when OBU named: " + obuName + " is selected.", obuTab.isOBUDeleteBtnEnabled());

        //Click the Yes button.
        deleteModal.clickBtn(Btn.YES);

        //Verify Confirm Delete modal is no longer displayed.
        Assert.assertFalse("The Confirm Delete modal is still displayed after clicking No.", deleteModal.isConfirmDeleteHeaderDisplayed());

        //Verify the device profile is no longer displayed in the OBU Device Profiles table.
        Assert.assertFalse("The OBU Device Profile named: " + obuName + " is still displayed after clicking Yes in the Confirm Delete modal.", obuTab.isOBUDisplayed(obu));

        //Click the Close button.
        obuTab.clickCloseBtn();

        //Log out
        simPage.logout(testuser);
    }

    /**
     * Test steps for TC-048
     */
    //TODO - needs to be updated based on UI changes
    //@Test
    public void deleteOBUInternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");

        //Ensure Simulations page is loaded
        SimulationsPage simPage = getPage(SimulationsPage.class);
        simPage.waitUntilLoaded();

        //Select multiple existing simulations.
        //simPage.selectAllSimulations(true); TODO - update for version 3.0

        //Verify the Configure button is disabled.
        //Assert.assertFalse("The configure button is not disabled as expected when more than one simulation is selected.", simPage.isSimBtnEnabled(SimBtns.CONFIGURE_BTN)); TODO - update for version 3.0

        //Select an existing simulation with existing executions.
        //simPage.selectAllSimulations(false); TODO - update for version 3.0
        simPage.selectSim(itcSim, true);

        //Verify the Configure button is disabled.
        //Assert.assertFalse("The configure button is not disabled as expected when a simulation with an existing execution is selected.", simPage.isBtnEnabled(SimBtns.CONFIGURE_BTN)); TODO - update for version 3.0

        //De-select the execution.
        simPage.selectSim(itcSim, false);

        //Select an execution with no existing executions and at least one obu device.
        simPage.selectSim(simulation, true);

        //Hover over the Configure button and click the Devices option.
        simPage.clickEdit();
        CompositeSettingsModal devicesForm = simPage.clickCompositeSettings();

        //Verify the OBU tab is selected.
        ConfigureOBUDeviceProfilesPartialPage obuForm = devicesForm.clickOBUTab();

        //Select a listed OBU.
        obuForm.selectRow(obu.getName(), true);

        //Click the Delete button.
        ConfirmDeleteModal deleteForm = obuForm.clickDeleteBtn();

        //Click the Yes button to confirm deletion.
        deleteForm.clickBtn(Btn.YES);

        //Verify the OBU device is no longer displayed.
        simPage.waitUntilLoaded();
        //Assert.assertFalse("Delete Warning window still displayed after clicking 'Yes'.", deleteForm.isRSEDeletionContentDisplayed()); TODO - update for version 3.0
        Assert.assertFalse("OBU device is still displayed after confirming deletion", obuForm.isOBUDisplayed(obu));

        //Click Close in OBU window and logout
        obuForm.clickCloseBtn();
        simPage.logout(testuser);
    }
}