package com.harmonia.qa.ETEXASWebQATests.ExecutionManagement;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.OBUDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.RSEDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;

/**
 * Test class which executes steps for the Filter Messages In In Progress
 * Execution test, TC-056/ITC-037
 *
 * @author llaroussini
 */
public class FilterMessagesInInProgressExecutionTest extends ETexasAfterTestResetTestBase {

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
     * The name of the BSM app to be configured with OBU device
     */
    private String bsmAppName = "BSMVerboseProducerApp (";

    /**
     * The RSE device which will be configured
     */
    private RSEDevice rse;

    /**
     * The name of the SPAR app to be configured with RSE device
     */
    private String spatAppName = "SPATProducerApp (";

    /**
     * The name of the app parameter being configured for the SPAT app
     */
    private String spatAppParamName = "hasFormattedData";

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
    //		//Get OBU and RSE Devices
    //		obu = OBUDeviceFactory.getOBUDevice(false); //gets static OBU rule configured to 100%
    //		rse = RSEDeviceFactory.getRSEDevice(true); //gets random RSE device
    //
    //		//Simulation created, no executions
    //		simulation = ETexasEntityManager.getTemplateSimulation(defaultEx05Sim); //get default EX_05 simulation
    //		simulation.setUser(testuser);
    //		simulation.addOBUDevice(obu);
    //		simulation.addRSEDevice(rse);
    //		ETexasEntityManager.addEntities(testuser, simulation, obu, rse);
    //		ETexasSimulationUtils.createTemplateSimulationWithOBUAndRSEDevices(simulation, obu, rse);
    //
    //		//OBU Device configured with BSM app
    //		ETexasDeviceUtils.addAppToDevice(obu, bsmAppName);
    //
    //		//RSE Device configured with SPAT app
    //		ETexasDeviceUtils.addAppToDevice(rse, spatAppName);
    //		//Set App Parameters for RSE/SPAT app
    //		ETexasDeviceUtils.updateAppParameter(rse, spatAppName, spatAppParamName, "true");
    //
    //		//Create an execution with an in-progress status
    //		String execName = ETexasExecutionUtils.createNewExecutionWithKnownRandomSeed(simulation, "1234");
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
    //		SimulationsPage simPage = landing.loginAs(testuser);
    //
    //		//Select execution, click control execution, and verify Executions Details page displays
    //		simPage.selectSimCheckBox(simulation, true);
    //		simPage.selectNewestExecutionCheckBox(true);
    //		ExecutionsPage execPage = simPage.clickControlExecution();
    //		Assert.assertTrue("Executions header could not be found.", execPage.isExecutionDetailsHeaderDisplayed());
    //
    //		//Step through execution by 505 steps
    //		execPage.enterSteps("505");
    //		execPage.clickNextStep();
    //	}

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Test steps for TC-056
    //	 */
    //	@Test
    //	public void filterMessagesInExecutionExternalTest() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");
    //		//Ensure the Executions page is loaded
    //		ExecutionsPage execPage = getPage(ExecutionsPage.class);
    //
    //		//Verify BSMV and SPAT messages display in Messages section
    //		execPage.clickCommandQueueMinimize();
    //		execPage.clickDevicesMinimize();
    //		Assert.assertTrue("Messages section is not expanded as expected.", execPage.isMinimizeIconDisplayed(SectionHeaders.MESSAGES));
    //		ETexasCommonUtils.sleep(2000);
    //		int initialCountBSMVMsgs = execPage.getCountOfBSMVMessages();
    //		int initialCountSPATMsgs = execPage.getCountOfSPATMessages();
    //		Assert.assertTrue("No BSMV messages are displayed 505 steps into execution.", initialCountBSMVMsgs > 0);
    //		Assert.assertTrue("No SPAT messages are displayed 505 steps into execution.", initialCountSPATMsgs > 0);
    //
    //		//Hover over Type column header in Messages section, verify arrow icon displays
    //		Assert.assertTrue("The Messages section could not be found.", execPage.isSectionDisplayed(SectionHeaders.MESSAGES));
    //		execPage.checkAllMessageColumnHeaders();
    //		execPage.hoverOverMessageColumnHeader(MessageColumns.TYPE);
    //		execPage.isMessagesAscendingArrowDisplayed(MessageColumns.TYPE);
    //
    //		//Click Arrow and verify menu displays
    //		FilterOptionsMenu menu = execPage.clickMessageColumnHeaderDropdown(MessageColumns.TYPE);
    //		Assert.assertTrue("Menu with filtering options not displayed as expected after clicking Messages Type column header.", menu.isFilterMenuDisplayed());
    //
    //		//Verify sorting options display: Sort Ascending and Sort Descending and Columns and Filters
    //		menu.checkAllFilterOptions();
    //
    //		//Click Columns and verify ID, Type, and Actions check boxes display
    //		menu.hoverOverFilterOption(HeaderMenuOption.COLUMNS);
    //		menu.checkAllMessagesFilterCheckBoxOptions();
    //
    //		//De-select a check box and verify column no longer displays in Messages section
    //		menu.clickMessageIdFilterCheckBox();
    //		Assert.assertTrue("ID column header still displayed after de-selecting ID column in filter.", execPage.isMessagesColumnHeaderDisplayed(MessageColumns.ID));
    //
    //		//Select a check box and verify column is added to Messages section
    //		menu.clickMessageIdFilterCheckBox();
    //		Assert.assertTrue("ID column header could not be found after selecting ID column in filter.", execPage.isMessagesColumnHeaderDisplayed(MessageColumns.ID));
    //
    //		//Hover over Filters option and verify text box displays
    //		menu.hoverOverFilterOption(HeaderMenuOption.FILTERS);
    //		Assert.assertTrue("Filter text box not displayed as expected after hovering over Filters option.", menu.isFilterTextBoxDisplayed());
    //
    //		//Enter 'BSM' in text box and verify only BSMV messages display
    //		menu.setFilterText("BSM");
    //		int filterdBSMVMsgs = execPage.getCountOfBSMVMessages();
    //		int filteredOutSPATMsgs = execPage.getCountOfSPATMessages();
    //		Assert.assertEquals("All BSMV message are not displayed as expected when filtering to only display BSMV messages.", initialCountBSMVMsgs, filterdBSMVMsgs);
    //		Assert.assertEquals("SPAT messages are displayed after filtering to only display BSMV messages.", filteredOutSPATMsgs, 0);
    //
    //		//Enter 'SPAT' in text box and verify only SPAT messages display
    //		execPage.clickMessageColumnHeaderDropdown(MessageColumns.TYPE);
    //		menu.hoverOverFilterOption(HeaderMenuOption.FILTERS);
    //		menu.setFilterText("SPAT");
    //		int filterdSPATMsgs = execPage.getCountOfSPATMessages();
    //		int filteredOutBSMVMsgs = execPage.getCountOfBSMVMessages();
    //		Assert.assertEquals("All SPAT message are not displayed as expected when filtering to only display SPAT messages.", initialCountSPATMsgs, filterdSPATMsgs);
    //		Assert.assertEquals("BSMV messages are displayed after filtering to only display SPAT messages.", filteredOutBSMVMsgs, 0);
    //
    //		//Delete text and verify all messages again display
    //		execPage.clickMessageColumnHeaderDropdown(MessageColumns.TYPE);
    //		menu.hoverOverFilterOption(HeaderMenuOption.FILTERS);
    //		menu.clearFilterText();
    //		int currentCountBSMVMsgs = execPage.getCountOfBSMVMessages();
    //		int currentCountSPATMsgs = execPage.getCountOfSPATMessages();
    //		Assert.assertEquals("All BSMV message are not displayed as expected when filter text is cleared.", initialCountBSMVMsgs, currentCountBSMVMsgs);
    //		Assert.assertEquals("All SPAT message are not displayed as expected when filter text is cleared.", initialCountSPATMsgs, currentCountSPATMsgs);
    //
    //		//Click Id column header and verify options display
    //		execPage.hoverOverMessageColumnHeader(MessageColumns.ID);
    //		execPage.clickMessageColumnHeaderDropdown(MessageColumns.ID);
    //
    //		//Click Sort Ascending, verify ascending arrow icon displays and messages display in ascending order
    //		String totalMsgs = Integer.toString(initialCountBSMVMsgs + initialCountSPATMsgs - 1);
    //		menu.clickFilterOption(HeaderMenuOption.SORT_ASCENDING);
    //		execPage.isMessagesAscendingArrowDisplayed(MessageColumns.ID);
    //		execPage.checkFirstMessageRowId("0"); //verifies message with ID of 0 is displayed first (ascending order)
    //
    //		//Click column header, verify descending arrow icon displays and messages display in descending order
    //		execPage.hoverOverMessageColumnHeader(MessageColumns.ID);
    //		execPage.clickMessageColumnHeader(MessageColumns.ID);
    //		execPage.isMessagesDescendingArrowDisplayed(MessageColumns.ID);
    //		execPage.checkFirstMessageRowId(totalMsgs); //verifies message with highest ID is displayed first (descending order)
    //
    //		//Click Finish button and verify Completed Execution Details page displays
    //		CompletedExecutionPage completedExec = execPage.clickFinish();
    //		Assert.assertTrue("App Log search area on Completed Execution page not displayed as expected upon completion of execution.", completedExec.isAppLogSearchResultsAreaDisplayed());
    //
    //		//Logout
    //		completedExec.logout(testuser);
    //	}

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Test steps for ITC-037
    //	 */
    //	@Test
    //	public void filterMessagesInExecutionInternalTest() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");
    //		//Ensure the Executions page is loaded
    //		ExecutionsPage execPage = getPage(ExecutionsPage.class);
    //
    //		//Verify messages are displayed
    //		int initialCountBSMVMsgs = execPage.getCountOfBSMVMessages();
    //		int initialCountSPATMsgs = execPage.getCountOfSPATMessages();
    //		Assert.assertTrue("No BSMV messages are displayed 505 steps into execution.", initialCountBSMVMsgs > 0);
    //		Assert.assertTrue("No SPAT messages are displayed 505 steps into execution.", initialCountSPATMsgs > 0);
    //
    //		//Hover over the Type column header in the Messages section.
    //		execPage.hoverOverMessagesColumn(ColumnName.TYPE);
    //
    //		//Click the arrow and verify a menu is displayed.
    //		FilterOptionsMenu menu = execPage.clickMessageColumnHeaderDropdown(MessageColumns.TYPE);
    //
    //		//Hover over the Filters option.
    //		menu.hoverOverFilterOption(HeaderMenuOption.FILTERS);
    //
    //		//Enter a random value in the Filter text box.
    //		Assert.assertTrue("Filer text box not displayed as expected when hovering over Filters option.", menu.isFilterTextBoxDisplayed());
    //		menu.setFilterText(RandomStringGenerator.nextNumStringOfLength(5));
    //
    //		//Verify the Messages section is filtered and no messages are displayed.
    //		int countBSMVMsgs = execPage.getCountOfBSMVMessages();
    //		int countSPATMsgs = execPage.getCountOfSPATMessages();
    //		Assert.assertEquals("BSMV messages are still displayed when random filter text is used.", 0, countBSMVMsgs);
    //		Assert.assertEquals("SPAT messages are still displayed when random filter text is used.", 0, countSPATMsgs);
    //
    //		//Delete the text in the Filter text box.
    //		execPage.clickMessageColumnHeaderDropdown(MessageColumns.TYPE);
    //		menu.hoverOverFilterOption(HeaderMenuOption.FILTERS);
    //		menu.clearFilterText();
    //
    //		//Verify all messages are displayed in the Messages section.
    //		int currentCountBSMVMsgs = execPage.getCountOfBSMVMessages();
    //		int currentCountSPATMsgs = execPage.getCountOfSPATMessages();
    //		Assert.assertEquals("All BSMV message are not displayed as expected when filter text is cleared.", initialCountBSMVMsgs, currentCountBSMVMsgs);
    //		Assert.assertEquals("All SPAT message are not displayed as expected when filter text is cleared.", initialCountSPATMsgs, currentCountSPATMsgs);
    //
    //		//Click Finish button and verify Completed Execution Details page displays
    //		CompletedExecutionPage completedExec = execPage.clickFinish();
    //		Assert.assertTrue("App Log search area on Completed Execution page not displayed as expected upon completion of execution.", completedExec.isAppLogSearchResultsAreaDisplayed());
    //
    //		//Logout
    //		completedExec.logout(testuser);
    //
    //	}
}
