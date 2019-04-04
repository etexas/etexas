package com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.FixedCellularDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.FixedCellularDeviceFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal.Btn;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.ConfigureFixedCellularDevicePartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;

/**
 * Test class which executes steps for the Delete a Cellular Device Test, TC-080
 *
 * @author llaroussini
 * @author rsmith
 */
public class DeleteFixedCellularDeviceTest extends ETexasAfterTestResetTestBase {

    /**
     * Composite object used throughout the test case
     */
    private CompositeSimulation composite;

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Simulation object used throughout the test case
     */
    private TemplateSimulation testSim;

    /**
     * Fixed Cellular Device object used throughout the test case.
     */
    private FixedCellularDevice fixedCellular;

    /**
     * Fixed cellular device name used throughout the test case
     */
    private String testFixedCellDeviceName;

    /**
     * Prerequisite steps/test setup
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get test user, test simulation, and test fixed cellular device
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        testSim = SimulationFactory.getTemplateSimulation(testUser, true);
        testSim.setUser(testUser);
        composite = testSim.getComposite();
        fixedCellular = FixedCellularDeviceFactory.getFixedCellularDevice(true);//get a random fixed Cellular device
        testFixedCellDeviceName = fixedCellular.getName();
        ETexasEntityManager.addEntities(testUser, testSim, composite, fixedCellular);

        //Register user and create new simulation from template with Fixed Cellular
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulationWithFixedCellularDevice(testSim, fixedCellular);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-079
     */
    @Test
    public void deleteFixedCellularExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select simulation
        simPage.expandComposite(composite, true);
        simPage.selectSim(testSim, true);

        //Click the Edit button.
        simPage.clickEdit();
        simPage.checkEditOptions();

        //Click the Simulation Settings option and verify the Simulation Settings modal displays.
        SimulationSettingsModal simSettingsModal = simPage.clickSimulationSettings();
        Assert.assertTrue("Simulations Settings header is not displayed as expected.", simSettingsModal.isSimSettingsHeaderDisplayed());

        //Click the Fixed Cellular tab.
        ConfigureFixedCellularDevicePartialPage fixedCellDevicesPage = simSettingsModal.clickFixedCellularDevicesTab();

        //Select an existing fixed cellular device
        fixedCellDevicesPage.selectRow(testFixedCellDeviceName, true);

        //Verify Delete button is enabled
        Assert.assertTrue("Delete button is not enabled as expected after selecting a fixed cell device.", fixedCellDevicesPage.isFixedCellularDeleteBtnEnabled());

        //Click the Delete button.
        ConfirmDeleteModal deleteModal = fixedCellDevicesPage.clickDelete();

        //Verify a Confirm Delete modal is displayed with text confirming deletion of the selected Fixed Cellular device.
        deleteModal.checkConfirmDeleteHeader();
        deleteModal.checkDeleteWarningFixedCellularDeviceContent(fixedCellular);

        //Verify an ‘x’ icon is displayed in the upper right corner of the modal.
        Assert.assertTrue("Close icon not displayed in Confirm Delete modal.", deleteModal.isCloseIconDisplayed());

        //Verify that Yes and No buttons are displayed at the bottom of the modal.
        Assert.assertTrue("Yes button not displayed in Confirm Delete modal.", deleteModal.isBtnDisplayed(Btn.YES));
        Assert.assertTrue("No button not displayed in Confirm Delete modal.", deleteModal.isBtnDisplayed(Btn.NO));

        //Click the No Button
        deleteModal.clickBtn(Btn.NO);

        //Verify Confirm Delete modal is no longer displayed.
        Assert.assertFalse("Confirm Delete modal is still displayed after clicking No.", deleteModal.isConfirmDeleteHeaderDisplayed());

        //Verify the Fixed Cellular Device list is unchanged.
        Assert.assertTrue("Fixed Cellular Device is not displayed in list after deletion is cancelled.", fixedCellDevicesPage.isFixedCellularDeviceDisplayed(testFixedCellDeviceName));

        //Verify fixed cellular device is selected, if not, select device
        if (fixedCellDevicesPage.isFixedCellularDeviceRowSelected(fixedCellular) == false) {
            fixedCellDevicesPage.selectRow(testFixedCellDeviceName, true);
        }

        //Click the Delete button.
        deleteModal = fixedCellDevicesPage.clickDelete();

        //Verify the Delete Warning modal is displayed
        deleteModal.checkConfirmDeleteHeader();

        //Click the 'x' icon
        deleteModal.clickCloseIcon();

        //Verify modal is no longer displayed.
        Assert.assertFalse("Delete Warning window is still displayed after clicking 'x' icon.", deleteModal.isFixedCellularDeviceDeletionContentDisplayed(fixedCellular));

        //Verify the detector list is unchanged.
        Assert.assertTrue("Fixed Cellular Device is not displayed in list after deletion is cancelled.", fixedCellDevicesPage.isFixedCellularDeviceDisplayed(testFixedCellDeviceName));

        //Verify detector is selected, if not, select detector
        if (fixedCellDevicesPage.isFixedCellularDeviceRowSelected(fixedCellular) == false) {
            fixedCellDevicesPage.selectRow(testFixedCellDeviceName, true);
        }

        //Click the Delete button.
        deleteModal = fixedCellDevicesPage.clickDelete();

        //Verify a Confirm Delete modal is displayed
        deleteModal.checkConfirmDeleteHeader();

        //Click the Yes button.
        deleteModal.clickBtn(Btn.YES);

        fixedCellDevicesPage.waitUntilLoaded();

        //Verify modal is no longer displayed.
        Assert.assertFalse("Delete Warning window is still displayed after clicking the Yes button.", deleteModal.isFixedCellularDeviceDeletionContentDisplayed(fixedCellular));

        //Verify the detector is no longer displayed in the Detectors list.
        Assert.assertFalse("Detector is still displayed in list after deletion.", fixedCellDevicesPage.isFixedCellularDeviceDisplayed(testFixedCellDeviceName));

        //Click Close button
        fixedCellDevicesPage.clickClose();

        //Log out
        simPage.logout(testUser);
    }
}
