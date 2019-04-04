package com.harmonia.qa.ETEXASWebQATests.ReportManagement;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.EmbeddedApp;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.enums.DeviceType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage.EditOptions;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.EditParameterModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.DeviceApplicationsModal;

/**
 * Test class for configuring reporting, TC-065 & ITC-046
 *
 * @author llaroussini
 */
public class ConfigureReportingTest extends ETexasAfterTestResetTestBase {

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
     * The reporting app used in the test case
     */
    private EmbeddedApp app;

    /**
     * The name of an app which can be configured (i.e. has parameters) and is
     * available on Report Device
     */
    private String configurableReportAppName = "ReportMOEApp";

    /**
     * Prerequisite steps/test setup
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get user, composite, and simulation
        testuser = ETexasUserFactory.getUser(true);
        simulation = SimulationFactory.getTemplateSimulation(testuser, true);
        simulation.setUser(testuser);
        composite = simulation.getComposite();
        ETexasEntityManager.addEntities(testuser, simulation, composite);

        //Register user and create composite/simulation created
        LandingPage landing = ETexasUserUtils.userRegistration(testuser);
        ETexasSimulationUtils.createTemplateSimulation(simulation);

        //TODO - update for version 3.0
        //        //Create additional simulation with started execution to be used in ITC
        //        itcSim = SimulationFactory.getTemplateSimulation(testuser, true);
        //        itcSim.setUser(testuser);
        //        ETexasEntityManager.addEntity(itcSim);
        //        ETexasSimulationUtils.createTemplateSimulation(itcSim);
        //        ETexasExecutionUtils.createNewExecution(itcSim);

        //An app exists with definable parameters (either to be uploaded/added or as built-in app, i.e. ReportMOEApp)
        app = ETexasEntityManager.getApp(configurableReportAppName);

        //User logged in
        landing.loginAs(testuser);
    }

    /**
     * Test steps for TC-065
     */
    @Test
    public void configureReportingExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify Simulations page is displayed
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select a composite that has no executions.
        simPage.selectComposite(composite, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Verify the Reporting option is enabled.
        Assert.assertTrue("The reporting option in the edit menu is not enabled when a composite has been selected.", simPage.isEditOptionDisplayed(EditOptions.REPORTING));

        //Click the Reporting option.
        DeviceApplicationsModal appModal = simPage.clickReporting();

        //Verify the Device Applications modal is displayed
        appModal.checkHeader();

        //Verify the following icons are displayed in the top right corner of the modal: ‘?’ and ‘x’.
        appModal.checkHeaderIcons();

        //Click the ‘?’ icon.
        appModal.clickDeviceApplicationsHelpIcon();

        //Verify that that the Device Applications Help modal is displayed with instructions for configuration of devices.
        appModal.checkHelpModal();

        //Verify an OK button is displayed in the modal.
        Assert.assertTrue("OK button could not be found in Device Applications Help modal.", appModal.isDeviceApplicationsHelpOKBtnDisplayed());

        //Click the OK button.
        appModal.clickDeviceApplicationsHelpOKBtn();

        //Verify the Help modal closes.
        Assert.assertFalse("Device Applications Help modal is still displayed after clicking OK.", appModal.isDeviceApplicationsHelpHeaderDisplayed());

        //Verify the following sections are displayed in the Device Applications modal: Available, Hosted, and Parameters.
        appModal.checkAppSectionsDisplayed();

        //Verify a list of reporting applications are displayed in the Available section.
        List<EmbeddedApp> apps = ETexasEntityManager.getAllEmbeddedApps();
        for (EmbeddedApp app : apps) {
            if (app.getDeviceType() == DeviceType.REPORT) {
                Assert.assertTrue("App named: " + app.getName() + " could not be found in the Available section as expected", appModal.isAppAvailable(app.getName()));
            }
        }

        //Verify the Hosted section is currently empty.
        Assert.assertEquals("Apps are displayed in the Hosted section by default.", 0, appModal.getHostedApps().size());

        //Verify an Edit button is displayed in the Parameters section.
        Assert.assertTrue("Edit button could not be found in Parameters section.", appModal.isEditBtnDisplayed());

        //Verify a table with a Name column and Value column is displayed in the Parameters section.
        appModal.checkParametersTableHeaderCells();

        //Verify the following icons are displayed between the Available and Hosted sections: ‘>’ and ‘<’.
        Assert.assertTrue("Add app button ('>') is not displayed as expected.", appModal.isAddAppBtnDisplayed());
        Assert.assertTrue("Remove app button ('<') is not displayed as expected.", appModal.isRemoveAppBtnDisplayed());

        //Verify a Close button is displayed at the bottom of the Device Applications modal.
        Assert.assertTrue("Close button is not displayed as expected in the Device Applications modal.", appModal.isCloseBtnDisplayed());

        //Select an application in the Available application list that has configurable parameters (i.e., ReportMOEApp).
        Assert.assertTrue("Configurable app named: " + configurableReportAppName + " could not be found.", appModal.isAppAvailable(configurableReportAppName));
        appModal.selectAvailableApp(configurableReportAppName);

        //Click the ‘>’ icon.
        appModal.clickAddApp();
        appModal.waitForSpecificHostedApp(configurableReportAppName);

        //Verify the selected application is moved from the Available section to the Hosted section.
        Assert.assertTrue("Newly added app is not listed as 'Hosted' after clicking the add button.  App name: '" + configurableReportAppName + "'", appModal.isAppHosted(configurableReportAppName));

        //Select the application in the Hosted section.
        appModal.selectHostedApp(configurableReportAppName);

        //Verify the definable parameters for the application are displayed in the table in the Parameters section.
        Map<String, String> params = appModal.getParamsAndValues();
        Assert.assertTrue("No parameters were found in the list.", params.size() > 0);

        //Verify a name is displayed in the Name column of the Parameters table for each of the application’s parameters.
        appModal.checkAllParamNamesDisplayed(app);

        //Verify a value is displayed in the Value column of the Parameters table for each of the application’s parameters.
        appModal.checkAllParamValuesDisplayed(app);

        //Verify the Edit button is disabled.
        Assert.assertFalse("Edit button is enabled without any parameters selected.", appModal.isEditBtnEnabled());

        //Select any listed parameter.
        String paramName = app.getParameters().get(0).getParameterName();
        String paramValue = app.getParameters().get(0).getParameterValue();
        appModal.selectParam(paramName);

        //Verify the Edit button is enabled.
        Assert.assertTrue("Edit button is disabled when a parameter is selected.", appModal.isEditBtnEnabled());

        //Click the Edit button.
        EditParameterModal editParamForm = appModal.clickEdit();

        //Verify the Edit Parameter modal is displayed.
        Assert.assertTrue("The Edit Parameter modal is not displayed after clicking Edit.", editParamForm.isHeaderDisplayed());

        //Verify the following icons are displayed in the top right corner of the modal: ‘?’ and ‘x’.
        editParamForm.checkHeaderIcons();

        //Click the ‘?’ icon.
        editParamForm.clickHelp();

        //Verify that that the Edit Parameter Help modal is displayed with instructions for editing parameter values.
        editParamForm.checkHelpModal();

        //Verify an OK button is displayed in the modal.
        Assert.assertTrue("The OK button is not displayed on the Edit Parameter Help modal.", editParamForm.isHelpOKBtnDisplayed());

        //Click the OK button.
        editParamForm.clickHelpOKBtn();

        //Verify that the Help modal closes.
        Assert.assertFalse("The help window is still displayed after clicking OK.", editParamForm.isHelpHeaderDisplayed());

        //Verify the parameter name is displayed in the Edit Parameter modal.
        editParamForm.checkParameterNameDisplayed(paramName);

        //Verify a Value text box is displayed in the Edit Parameter modal.
        editParamForm.checkFields();

        //Verify the default value for the selected parameter is populated in the Value text box.
        Assert.assertEquals("The expected value was not displayed on the edit parameter form.", paramValue, editParamForm.getParameterValue());

        //Verify the following buttons are displayed at the bottom of the modal: Update, Reset, and Cancel
        editParamForm.checkBtns();

        //Enter a new value in the Value text box.
        String newValue = "2.0";
        editParamForm.setParameterValue(newValue);

        //Click the Reset button.
        editParamForm.clickReset();

        //Verify the Value resets to the original value.
        Assert.assertEquals("The expected value was not displayed on the edit parameter form.", paramValue, editParamForm.getParameterValue());

        //Enter a new value in the Value text box.
        editParamForm.setParameterValue(newValue);

        //Click the Cancel button.
        editParamForm.clickCancel();

        //Verify the Edit Parameter modal is no longer displayed.
        Assert.assertFalse("The Edit Parameter modal is still displayed after clicking Cancel.", editParamForm.isHeaderDisplayed());

        //Verify the value does not update in the Parameters table.
        Map<String, String> updatedParams = appModal.getParamsAndValues();
        for (Entry<String, String> e : updatedParams.entrySet()) {
            Assert.assertEquals("Value mismatch after editing the parameter was cancelled.", params.get(e.getKey()), e.getValue());
        }

        //Select any parameter value.
        appModal.selectParam(paramName);

        //Click the Edit button.
        appModal.clickEdit();

        //Enter a new value in the Value text box.
        editParamForm.setParameterValue(newValue);

        //Click the ‘x’ icon.
        editParamForm.clickCloseIcon();

        //Verify the Edit Parameter modal is no longer displayed.
        Assert.assertFalse("The Edit Parameter modal is still displayed after clicking Close.", editParamForm.isHeaderDisplayed());

        //Verify the value does not update in the Parameters table.
        for (Entry<String, String> e : updatedParams.entrySet()) {
            Assert.assertEquals("Value mismatch after editing the parameter was cancelled.", params.get(e.getKey()), e.getValue());
        }

        //Select any parameter value.
        appModal.selectParam(paramName);

        //Click the Edit button.
        appModal.clickEdit();

        //Enter a new value in the Value text box.
        editParamForm.setParameterValue(newValue);

        //Click the Update button.
        editParamForm.clickUpdate(true);

        //Verify the value updates in the Parameters table.
        updatedParams = appModal.getParamsAndValues();
        String updatedValue = updatedParams.get(paramName);
        Assert.assertEquals("The parameter value was not successfully updated.", newValue, updatedValue);

        //Click the Close button.
        appModal.clickCloseBtn();

        //Logout
        simPage.logout(testuser);
    }

    /**
     * Test steps for ITC-046
     */
    //TODO - needs to be updated based on UI changes
    //@Test
    public void configureReportingInternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select multiple existing simulations.
        //simPage.selectAllSimulations(true); TODO - update for version 3.0
        //Select an existing simulation with existing executions.
        //simPage.selectAllSimulations(false); TODO - update for version 3.0
        simPage.selectSim(itcSim, true);

        //De-select the execution.
        simPage.selectSim(itcSim, false);

        //Select a simulation with no existing executions.
        simPage.selectSim(simulation, true);

        //Hover over the Configure button.
        simPage.clickEdit();

        //Click the Reporting option.
        DeviceApplicationsModal deviceConfig = simPage.clickReporting();

        //Select a configurable app from the Available Apps list (e.g., ReportMOEApp).
        deviceConfig.selectAvailableApp(configurableReportAppName);

        //Click the right arrow icon.
        deviceConfig.clickAddApp();

        //Verify app is moved to Installed Apps.
        Assert.assertTrue(configurableReportAppName + "is not displayed as installed as expected.", deviceConfig.isAppHosted(configurableReportAppName));

        //Select the app.
        deviceConfig.selectHostedApp(configurableReportAppName);

        //Select a parameter listed in the App Parameters list.
        deviceConfig.selectFirstParam();

        //Click the Edit button.
        EditParameterModal editParam = deviceConfig.clickEdit();

        //Take note of the value in the Parameter Value text box, then delete the text and click the Save button
        String origParamValue = editParam.getParameterValue();
        editParam.setParameterValue("");
        editParam.clickUpdate(false);

        //Verify an error is displayed associated with the Parameter Value text box indicating the field is required.
        editParam.checkParamValueFieldRequiredErrorDisplayed();

        //Enter one or more space characters in the Parameter Value text box and click the Save button.
        editParam.setParameterValue("   ");
        editParam.clickUpdate(false);

        //Verify an error is displayed associated with the Parameter Value text box indicating the field is required.
        editParam.checkParamValueFieldRequiredErrorDisplayed();

        //Enter a valid value (the original value noted in previous steps) in the Parameter Value text box and click the Save button.
        editParam.setParameterValue(origParamValue);
        editParam.clickUpdate(false);

        //Click the close btn
        deviceConfig.clickCloseBtn();

        //Click the username, logout
        simPage.logout(testuser);

    }
}