package com.harmonia.qa.ETEXASWebQATests.ExecutionManagement;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.Execution;
import com.harmonia.qa.ETEXASWebQATests.entities.OBUDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;

/**
 * Test class which executes steps for the View Messages Produced while
 * Executing test, TC-025
 *
 * @author llaroussini
 */
public class ViewMessagesInExecutionTest extends ETexasAfterTestResetTestBase {

    /**
     * The user used in the test case
     */
    private ETexasUser testuser;

    /**
     * The simulation used in the test case.
     */
    private TemplateSimulation simulation;

    /**
     * The execution used in the test case
     */
    private Execution execution;

    /**
     * The OBU device which will be configured
     */
    private OBUDevice obu;

    /**
     * The name of a BSM app to be configured with OBU device
     */
    private String bsmAppName = "BSMVerboseProducerApp";

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
    //		ETexasDeviceUtils.addAppToDevice(obu, bsmAppName);
    //
    //		//Create an execution with an in-progress status
    //		String execName = ETexasExecutionUtils.createNewExecution(simulation);
    //
    //		//Create execution entities
    //		List<Execution> execList = new ArrayList<Execution>(3);
    //		execution = new Execution();
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
    //	 * Test steps for TC-025
    //	 */
    //	@Test
    //	public void viewMessagesInExecution() {
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
    //		//Verify messages section displays
    //		Assert.assertTrue("The Messages section could not be found.", excPage.isSectionDisplayed(SectionHeaders.MESSAGES));
    //		Assert.assertTrue("The Messages header could not be found.", excPage.isSectionHeaderDisplayed(SectionHeaders.MESSAGES));
    //
    //		//Verify minimize and help icons display
    //		Assert.assertTrue("The Minimize icon could not be found in the Messages header.", excPage.isMinimizeIconDisplayed(SectionHeaders.MESSAGES));
    //		Assert.assertTrue("The Help icon could not be found in the Messages header.", excPage.isSectionHelpIconDisplayed(SectionHeaders.MESSAGES));
    //
    //		//Click minimize, then verify section collapses and maximize icon displays
    //		excPage.clickMessagesMinimize(); //verification occurs within method
    //
    //		//Click maximize, then verify section expands and minimize icon displays
    //		excPage.clickMessagesMaximize(); //verification occurs within method
    //
    //		//Click the help icon
    //		MessagesHelpWindow help = excPage.clickMessagesHelp();
    //
    //		//Verify help window displays with expected header and content
    //		Assert.assertTrue("The Messages Help header is not displayed.", help.isMessagesHelpHeaderDisplayed());
    //		Assert.assertTrue("The Messages Help content is not displayed.", help.isHelpContentDisplayed());
    //
    //		//Verify OK button displays and click OK
    //		Assert.assertTrue("OK button is not displayed.", help.isHelpOKBtnDisplayed());
    //		help.clickHelpOKBtn();
    //
    //		//Verify window no longer displays
    //		Assert.assertFalse("The Messages Help header is still displayed after clicking OK.", help.isMessagesHelpHeaderDisplayed());
    //
    //		//Verify expected columns (ID and Type) display in Messages section
    //		excPage.checkAllMessageColumnHeaders();
    //
    //		//Click Next Step (advance 50 steps) and verify messages display
    //		excPage.enterSteps("50");
    //		excPage.clickNextStep();
    //		int messageList = excPage.getCountOfBSMVMessages();
    //		Assert.assertTrue("No message rows for BSMV could be found after advancing 25 steps.", messageList > 0);
    //
    //		//Verify (i) icon displays next to each message
    //		excPage.checkBSMVMessageInfoIcons();
    //
    //		//Verify tool-tip text displays with each message
    //		excPage.checkBSMVMessageIconToolTips();
    //
    //		//Click (i) and verify Viewing Message window displays
    //		String id = excPage.getFirstBSMVMessageId();
    //		ViewingMessageWindow viewMsg = excPage.clickInfoIcon(id);
    //		Assert.assertTrue("Viewing Message window was not displayed after clicking info icon in the message row with ID of " + id + ".", viewMsg.isViewingBSMVMessageHeaderDisplayed(id));
    //
    //		//Verify message details display in window
    //		Assert.assertTrue("Content area not displayed in Viewing Message window for message with ID of " + id + ".", viewMsg.isViewingBSMVMessageContentDisplayed(id));
    //		viewMsg.checkContentText(id);
    //
    //		//Verify OK button displays and click OK
    //		Assert.assertTrue("OK button is not displayed.", viewMsg.isViewingMessageOKBtnDisplayed());
    //		viewMsg.clickViewingMessageOKBtn();
    //
    //		//Advance 100 steps and verify Messages section updates
    //		excPage.enterSteps("100");
    //		excPage.clickNextStep();
    //		int updatedMssageList = excPage.getCountOfBSMVMessages();
    //		Assert.assertNotSame("Message list does not update after advancing and additional 25 steps.", updatedMssageList, messageList);
    //
    //		//BUG 12400 - remove active code and enable commented code when resolved
    //		excPage.clickFinish();
    //		excPage.logout(testuser);
    //		//Click Finish and verify Completed Execution Details display (details tabs displayed during execution no longer displayed and completed tabs ARE displayed)
    //		//CompletedExecutionPage completedExec = excPage.clickFinish();
    //		//Verify Execution Details page updated to display Completed Execution Details
    //		//Verifies tabs displayed during in progress execution are no longer present, and completed execution tabs are preset
    //		//Assert.assertFalse("Vehicles tab is still displayed after finishing execution.", completedExec.isTabDisplayed(ExecutionDetailsTabs.VEHICLES_TAB));
    //		//Assert.assertFalse("Signals tab is still displayed after finishing execution.", completedExec.isTabDisplayed(ExecutionDetailsTabs.SIGNALS_TAB));
    //		//Assert.assertFalse("Detectors tab is still displayed after finishing execution.", completedExec.isTabDisplayed(ExecutionDetailsTabs.DETECTORS_TAB));
    //		//completedExec.checkAllCompletedExecTabsDisplayed();
    //
    //		//Logout
    //		//completedExec.logout(testuser);
    //
    //	}
}
