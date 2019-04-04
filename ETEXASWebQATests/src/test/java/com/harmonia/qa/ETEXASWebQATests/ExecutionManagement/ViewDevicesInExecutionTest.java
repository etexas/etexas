package com.harmonia.qa.ETEXASWebQATests.ExecutionManagement;

import com.harmonia.qa.ETEXASWebQATests.entities.EmbeddedApp;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.OBUDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.RSEDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;

/**
 * Test class which executes steps for the View Devices Associated with an
 * Executing test, TC-025
 *
 * @author llaroussini
 */
public class ViewDevicesInExecutionTest extends ETexasAfterTestResetTestBase {

    /**
     * The user used in the test case
     */
    private ETexasUser testuser;

    /**
     * The simulation used in the test case.
     */
    private TemplateSimulation simulation;

    /**
     * The OBU device which will be configured
     */
    private OBUDevice obu;

    /**
     * The BSM app to be configured with OBU device
     */
    private EmbeddedApp bsmApp;

    /**
     * The name of a BSM app
     */
    private String bsmAppName = "BSMProducerApp";

    /**
     * The RSE device associated with execution
     */
    private RSEDevice rse;

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Prerequisite steps/test setup
    //	 */
    //	@Before
    //	public void warmUp() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");
    //		//Section preconditions:
    //		//User registered
    //		testuser = ETexasUserFactory.getUser(true);
    //		LandingPage landing = ETexasUserUtils.userRegistration(testuser);
    //		//User logged in and is on the simulations page (user will be left logged in after simulation creation)
    //
    //		//Simulation created, no executions
    //		simulation = SimulationFactory.getTemplateSimulation(testuser, true);
    //		simulation.setUser(testuser);
    //		ETexasEntityManager.addEntities(testuser, simulation);
    //		ETexasSimulationUtils.createTemplateSimulation(simulation);
    //
    //		//An OBU Device has been added
    //		obu = OBUDeviceFactory.getOBUDevice(false); //gets static OBU rule configured to 100%
    //		simulation.addOBUDevice(obu);
    //		ETexasDeviceUtils.createObuDevice(obu);
    //		ETexasEntityManager.addEntity(obu);
    //
    //		//OBU Device configured with BSM app
    //		List<BuiltInApp> apps = new ArrayList<BuiltInApp>(1);
    //		bsmApp = ETexasEntityManager.getApp(bsmAppName);
    //		apps.add(bsmApp);
    //		ETexasDeviceUtils.addAppToDevice(obu, bsmAppName);
    //		bsmApp.setOBUDevice(obu);
    //		obu.setApps(apps);
    //
    //		//An RSE Device has been added
    //		rse = RSEDeviceFactory.getRSEDevice(false); //gets static RSE device
    //		simulation.addRSEDevice(rse);
    //		ETexasDeviceUtils.createRseDevice(rse);
    //		ETexasEntityManager.addEntity(rse);
    //
    //		//Create an execution with an in-progress status
    //		String execName = ETexasExecutionUtils.createNewExecution(simulation);
    //
    //		//Create execution entities
    //		List<Execution> execList = new ArrayList<Execution>(3);
    //		Execution execution = new Execution();
    //		execution.setSimulation(simulation);
    //		execution.setName(execName);
    //		execution.setStatus(Status.NOT_STARTED);
    //		execList.add(execution);
    //
    //		//Set execution with associated simulation
    //		simulation.setExecutions(execList);
    //		ETexasEntityManager.addEntities(execution);
    //
    //		//User logged in
    //		landing.waitUntilLoaded();
    //		landing.loginAs(testuser);
    //	}

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Test steps for TC-053
    //	 */
    //	@Test
    //	public void viewDevicesInExecution() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");
    //		//Ensure the Simulations page is loaded
    //		SimulationsPage simPage = getPage(SimulationsPage.class);
    //
    //		//Select execution, click control execution, and verify Executions Details page displays
    //		simPage.selectSimCheckBox(simulation, true);
    //		simPage.selectNewestExecutionCheckBox(true);
    //		ExecutionsPage excPage = simPage.clickControlExecution();
    //		Assert.assertTrue("Executions header could not be found.", excPage.isExecutionDetailsHeaderDisplayed());
    //
    //		//Step through execution by 1000 steps
    //		excPage.enterSteps("1000");
    //		excPage.clickNextStep();
    //
    //		//Verify messages section displays
    //		Assert.assertTrue("The Devices section could not be found.", excPage.isSectionDisplayed(SectionHeaders.DEVICES));
    //		Assert.assertTrue("The Devices header could not be found.", excPage.isSectionHeaderDisplayed(SectionHeaders.DEVICES));
    //
    //		//Verify minimize and help icons display
    //		Assert.assertTrue("The Minimize icon could not be found in the Devices header.", excPage.isMinimizeIconDisplayed(SectionHeaders.DEVICES));
    //		Assert.assertTrue("The Help icon could not be found in the Devices header.", excPage.isSectionHelpIconDisplayed(SectionHeaders.DEVICES));
    //
    //		//Click minimize, then verify section collapses and maximize icon displays
    //		excPage.clickDevicesMinimize(); //verification occurs within method
    //
    //		//Click maximize, then verify section expands and minimize icon displays
    //		excPage.clickDevicesMaximize(); //verification occurs within method
    //
    //		//Click the help icon
    //		DevicesHelpWindow help = excPage.clickDevicesHelp();
    //
    //		//Verify help window displays with expected header and content
    //		Assert.assertTrue("The Devices Help header is not displayed.", help.isDevicesHelpHeaderDisplayed());
    //		Assert.assertTrue("The Devices Help content is not displayed.", help.isHelpContentDisplayed());
    //
    //		//Verify OK button displays and click OK
    //		Assert.assertTrue("OK button is not displayed.", help.isHelpOKBtnDisplayed());
    //		help.clickHelpOKBtn();
    //
    //		//Verify window no longer displays
    //		Assert.assertFalse("The Devices Help header is still displayed after clicking OK.", help.isDevicesHelpHeaderDisplayed());
    //
    //		//Verify expected columns (ID, Name, and Percentage) display in Messages section
    //		excPage.checkAllDevicesColumns();
    //
    //		//Verify devices configured with simulation are displayed
    //		String obuName = obu.getName();
    //		String rseName = rse.getName();
    //		Assert.assertTrue("Row for OBU device could not be found.", excPage.isDeviceRowDisplayed(obuName));
    //		Assert.assertTrue("Row for RSE device could not be found.", excPage.isDeviceRowDisplayed(rseName));
    //
    //		//Verify (i) icon displays next to each device
    //		excPage.checkDeviceInfoIcons();
    //
    //		//Verify tool-tip text displays with each device
    //		excPage.checkDeviceIconToolTips();
    //
    //		//Click (i) and verify View Device window displays
    //		ViewDeviceWindow viewDvc = excPage.clickDeviceInfoIcon(obuName);
    //		Assert.assertTrue("View Device window was not displayed after clicking info icon in the OBU device.", viewDvc.isViewDeviceHeaderDisplayed(obuName));
    //
    //		//Verify Installed Apps and App Parameters display in window
    //		viewDvc.checkDeviceAppSections();
    //
    //		//Verify Installed Apps are displayed
    //		Assert.assertTrue("Installed App, " + bsmAppName + ", was not found in Installed Apps table.", viewDvc.isInstalledAppDisplayed(bsmApp));
    //
    //		//Select an installed app and verify parameters display
    //		viewDvc.clickInstalledApp(bsmApp);
    //		viewDvc.waitForAppParamsList();
    //		String paramName = bsmApp.getParameters().get(0).getParameterName();
    //		Assert.assertTrue("App Parameter, " + paramName + ", was not found in App Parameters table.", viewDvc.isAppParameterDisplayed(bsmApp));
    //
    //		//Verify Close button displays and click Close
    //		Assert.assertTrue("Close button is not displayed.", viewDvc.isCloseBtnDisplayed(obuName));
    //		viewDvc.clickCloseButton(obuName);
    //
    //		//Click Finish and verify Completed Execution Details display (details tabs displayed during execution no longer displayed and completed tabs ARE displayed)
    //		CompletedExecutionPage completedExec = excPage.clickFinish();
    //		Assert.assertTrue("App Log search area on Completed Execution page not displayed as expected upon completion of execution.", completedExec.isAppLogSearchResultsAreaDisplayed());
    //
    //		//Logout
    //		excPage.logout(testuser);
    //
    //	}
}
