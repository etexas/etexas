package com.harmonia.qa.ETEXASWebQATests.XXXTestsNoLongerUsed;

import org.junit.Before;

import com.harmonia.qa.ETEXASWebQATests.entities.CellularDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.FixedCellularDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.OBUDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.RSEDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.CellularDeviceFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.FixedCellularDeviceFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.OBUDeviceFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.RSEDeviceFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;

/**
 * Test class for viewing all devices, TC-081
 *
 * @author llaroussini
 */
public class ViewAllDevicesTest extends ETexasAfterTestResetTestBase {

    /**
     * The user used in the test case
     */
    private ETexasUser testuser;

    /**
     * The simulation used in the test case
     */
    private TemplateSimulation simulation;

    /**
     * The RSE device used in the test case
     */
    private RSEDevice rse;

    /**
     * The OBU used in the test case
     */
    private OBUDevice obu;

    /**
     * The Cellular device used in the test case
     */
    private CellularDevice cell;

    /**
     * The Fixed Cellular device used in the test case
     */
    private FixedCellularDevice fixedCell;

    /**
     * Prerequisite steps/test setup
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");
        //Get test user, test simulation, and test devices
        testuser = ETexasUserFactory.getUser(true); //Get a random user.
        simulation = SimulationFactory.getTemplateSimulation(testuser, true); //get a random template simulation
        rse = RSEDeviceFactory.getRSEDevice(true); //get a random RSE device
        obu = OBUDeviceFactory.getOBUDevice(true); //get a random OBU device
        cell = CellularDeviceFactory.getCellularDevice(true); //get random cellular device
        fixedCell = FixedCellularDeviceFactory.getFixedCellularDevice(true); //get random fixed cellular device
        ETexasEntityManager.addEntities(testuser, simulation, rse, obu, cell, fixedCell);

        //Register user and setup simulation with all devices
        LandingPage landing = ETexasUserUtils.userRegistration(testuser);
        ETexasSimulationUtils.createTemplateSimulationWithAllDevices(simulation, rse, obu, cell, fixedCell);

        //User logged in
        landing.waitUntilLoaded();
        landing.loginAs(testuser);
    }

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Test steps for TC-081
    //	 */
    //	@Test
    //	public void viewAllDevicesExternalTest() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");
    //
    //		//Verify the Simulations page is displayed.
    //		SimulationsPage simPage = getPage(SimulationsPage.class);
    //
    //		//Select an existing simulation with no executions and at least one of each of the device types (RSE, OBU, cellular, and fixed cellular).
    //		simPage.selectSimCheckBox(simulation, true);
    //
    //		//Verify the Configure button becomes enabled.
    //		Assert.assertTrue("Configure button is not enabled when simulation is selected.", simPage.isSimBtnEnabled(SimBtns.CONFIGURE_BTN));
    //
    //		//Hover over the Configure button and verify the following options display: Devices, Environment, Reporting, and Edit Sim Source.
    //		simPage.clickEdit();
    //		simPage.checkEditOptions();
    //
    //		//Click the Devices option.
    //		ConfigureDevicesForm devicesPage = simPage.clickSimulationSettings();
    //
    //		//Verify the Configure Sim Devices pop-up window is displayed.
    //		Assert.assertTrue("Configure Devices window not displayed after clicking Devices option.", devicesPage.isConfigureDevicesHeaderDisplayed());
    //
    //		//Verify the following tabs are displayed across the top of the pop-up window: Overview, RSE, OBU, Cellular, and Fixed Cellular.
    //		devicesPage.checkTabs();
    //
    //		//Click Overview tab
    //		OverviewDeviceConfigurationPartialPage overviewPage = devicesPage.clickOverviewTab();
    //
    //		//Verify that a ‘?’ icon and an ‘x’ icon are displayed in the upper right corner of the pop-up window.
    //		overviewPage.checkHeaderIcons();
    //
    //		//Click the ‘?’ icon.
    //		overviewPage.clickHelp();
    //
    //		//Verify that that the Simulation Device Configuration Help pop-up window is displayed with instructions for configuring devices and applications.
    //		Assert.assertTrue("Configure Devices Help header is not displayed as expected after clicking help icon.", overviewPage.isConfigureDevicesHelpHeaderDisplayed());
    //		overviewPage.checkHelpText();
    //
    //		//Verify an OK button is displayed in the pop-up window.
    //		Assert.assertTrue("OK button not displayed in help window as expected.", overviewPage.isHelpOKBtnDisplayed());
    //
    //		//Click the OK button.
    //		overviewPage.clickHelpOKBtn();
    //
    //		//Verify that the Simulation Device Configuration Help pop-up window closes.
    //		Assert.assertFalse("Configure Devices Help header is still displayed after clicking OK.", overviewPage.isConfigureDevicesHelpHeaderDisplayed());
    //
    //		//Verify the following buttons are displayed across the top of the Overview tab: Configure, and Delete.
    //		overviewPage.checkOverviewBtns();
    //
    //		//Verify the following buttons are disabled: Configure and Delete.
    //		Assert.assertFalse("The configure button is not enabled as expected.", overviewPage.isConfigureBtnEnabled());
    //		Assert.assertFalse("The delete button is not enabled as expected.", overviewPage.isDeleteBtnEnabled());
    //
    //		//Verify a table is displayed listing the ID, Device Name, and Device Type for available RSE, OBU, Cellular, and Fixed Cellular devices.
    //		overviewPage.checkOverviewDeviceColumnHeaders();
    //		overviewPage.checkDeviceDisplayed(rse);
    //		overviewPage.checkDeviceDisplayed(obu);
    //		overviewPage.checkDeviceDisplayed(cell);
    //		overviewPage.checkDeviceDisplayed(fixedCell);
    //
    //		//Verify a Close button is displayed at the bottom of the window.
    //		Assert.assertTrue("Close button not displayed as expected.", overviewPage.isCloseBtnDisplayed());
    //
    //		//Click the Close button.
    //		overviewPage.clickCloseBtn();
    //
    //		//Log out
    //		simPage.logout(testuser);
    //
    //	}
}
