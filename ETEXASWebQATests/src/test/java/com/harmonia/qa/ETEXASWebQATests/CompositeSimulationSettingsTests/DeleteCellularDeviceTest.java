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
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal.Btn;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeSettingsModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.ConfigureCellularDevicePartialPage;

/**
 * Test class which executes steps for the Delete a Cellular Device Test, TC-070
 *
 * @author llaroussini
 * @author saistrop
 */
public class DeleteCellularDeviceTest extends ETexasAfterTestResetTestBase {

    /**
     * The user used in the test case
     */
    private ETexasUser testuser;

    /**
     * The simulation used in the test case.
     */
    private TemplateSimulation simulation;

    /**
     * The composite used throughout the test case
     */
    private CompositeSimulation composite;

    /**
     * The cellular device which will be configured
     */
    private CellularDevice testCellDevice;

    /**
     * The name of the cellular device used in the test case
     */
    private String testCellDeviceName;

    /**
     * Prerequisite steps/test setup
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Gets test user, test composite, and cellular device
        testuser = ETexasUserFactory.getUser(true);
        ETexasEntityManager.addEntity(testuser);
        simulation = SimulationFactory.getTemplateSimulation(testuser, true);
        composite = simulation.getComposite(); //gets the Composite from the simulation
        simulation.setUser(testuser);
        testCellDevice = CellularDeviceFactory.getCellularDevice(true);
        testCellDeviceName = testCellDevice.getName();

        //User registration and creation of Composite with Cellular Device
        LandingPage landing = ETexasUserUtils.userRegistration(testuser);
        ETexasSimulationUtils.createTemplateSimulationWithCellularDevice(simulation, testCellDevice);

        //User logged in
        landing.waitUntilLoaded();
        landing.loginAs(testuser);
    }

    /**
     * Test steps for TC-070
     */
    @Test
    public void deleteCellularExternalTest() {
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

        //Select cellular device profile and verify Delete button is enabled
        cellDevicesPage.selectCellularDevice(testCellDevice);
        Assert.assertTrue("Delete button was not enabled for the Cellular Device when expected.", cellDevicesPage.isCellularDeleteBtnEnabled());

        //Click the Delete button and verify a Confirm Delete modal appears along with necessary buttons
        ConfirmDeleteModal deleteModal = cellDevicesPage.clickDeleteBtn();
        Assert.assertTrue("Delete Cellular Device confirmation modal did not appear after clicking the Delete button.", deleteModal.isConfirmDeleteHeaderDisplayed());
        Assert.assertTrue("Delete Cellular Device confirmation text did not appear after clicking the Delete button.", deleteModal.isCellularDeviceDeletionContentDisplayed(testCellDevice));
        Assert.assertTrue("Delete Cellular Device confirmation modal did now display an 'x' icon where expected.", deleteModal.isCloseIconDisplayed());
        deleteModal.checkConfirmDeleteBtns();

        //Click the No button and verify modal disappears and device has not changed
        deleteModal.clickBtn(Btn.NO);
        Assert.assertFalse("Delete Cellualar Device confirmation modal is still present after clicking the No button.", deleteModal.isConfirmDeleteHeaderDisplayed());
        Assert.assertTrue("Cellular Device is no longer present after clicking No in the delete confirmation modal.", cellDevicesPage.isCellularDeviceDisplayed(testCellDeviceName));

        //Select the cellular device
        cellDevicesPage.selectCellularDevice(testCellDevice);

        //Click the Delete button and verify the modal appears
        deleteModal = cellDevicesPage.clickDeleteBtn();
        Assert.assertTrue("Delete Cellular Device confirmation modal did not appear after clicking the Delete button.", deleteModal.isConfirmDeleteHeaderDisplayed());

        //Click the 'x' icon and verify modal disappears without changing device
        deleteModal.clickCloseIcon();
        Assert.assertFalse("Delete Cellualar Device confirmation modal is still present after clicking the 'x' icon.", deleteModal.isConfirmDeleteHeaderDisplayed());
        Assert.assertTrue("Cellular Device is no longer present after clicking No in the delete confirmation modal.", cellDevicesPage.isCellularDeviceDisplayed(testCellDeviceName));

        //Select the cellular device
        cellDevicesPage.selectCellularDevice(testCellDevice);

        //Click Yes and verify device is no longer present in the table
        deleteModal = cellDevicesPage.clickDeleteBtn();
        Assert.assertTrue("Delete Cellular Device confirmation modal did not appear after clicking the Delete button.", deleteModal.isConfirmDeleteHeaderDisplayed());

        //Click the Yes button and verify modal disappears without changing device
        deleteModal.clickBtn(Btn.YES);
        Assert.assertFalse("Delete Cellualar Device confirmation modal is still present after clicking the Yes button.", deleteModal.isConfirmDeleteHeaderDisplayed());
        Assert.assertFalse("Cellular Device is still present after clicking No in the delete confirmation modal.", cellDevicesPage.isCellularDeviceDisplayed(testCellDeviceName));

        //Click Close button
        cellDevicesPage.clickCloseBtn();

        //Log out
        simPage.logout(testuser);

    }
}