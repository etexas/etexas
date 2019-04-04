package com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.EmbeddedApp;
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
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.EditParameterModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeSettingsModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.DeviceApplicationsModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.ConfigureOBUDeviceProfilesPartialPage;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class for Configuring OBU Device Profile, TC-072/ITC-053
 *
 * @author llaroussini
 */
public class ConfigureOBUDeviceProfileTest extends ETexasAfterTestResetTestBase {

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
     * The OBU Device to be configured
     */
    private OBUDevice obu;

    /**
     * The name of the OBU device being configured
     */
    private String obuName;

    /**
     * The build in app used in the test case
     */
    private EmbeddedApp app;

    /**
     * The name of an app which can be configured (i.e. has parameters) and is
     * available on OBU device
     */
    private String configurableOBUAppName = "BSMVerboseProducerApp";

    /**
     * Prerequisite steps/test setup
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get test user, simulation/composite, OBU device
        testuser = ETexasUserFactory.getUser(true);
        simulation = SimulationFactory.getTemplateSimulation(testuser, true);
        simulation.setUser(testuser);
        composite = simulation.getComposite();
        obu = OBUDeviceFactory.getOBUDevice(true);
        obuName = obu.getName();
        ETexasEntityManager.addEntities(testuser, simulation, composite, obu);

        //User registered, simulation/composite created with OBU device
        LandingPage landing = ETexasUserUtils.userRegistration(testuser);
        ETexasSimulationUtils.createTemplateSimulationWithOBUDevice(simulation, obu);

        //An app exists with definable parameters (either to be uploaded/added or as built-in app, i.e. BSMVerboseProducerApp)
        app = ETexasEntityManager.getApp(configurableOBUAppName);

        //User logged in
        landing.loginAs(testuser);
    }

    /**
     * Test steps for TC-072
     */
    @Test
    public void configureOBUDeviceExternalTest() {
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
        Assert.assertTrue("The Composite Settings modal is not displayed as expected after clicking Edit.", compositeSettingsModal.isCompositeSettingsHeaderDisplayed());

        //Click the OBU Device Profiles tab.
        ConfigureOBUDeviceProfilesPartialPage obuTab = compositeSettingsModal.clickOBUTab();

        //Verify the OBU device profile associated with the selected composite is displayed.
        Assert.assertTrue("OBU Device Profile named: " + obuName + " associated with the selected composite is not displayed.", obuTab.isOBUDisplayed(obu));

        //Select the OBU device profile.
        obuTab.selectRow(obuName, true);

        //Verify the Applications button is enabled.
        Assert.assertTrue("Applications button is not enabled as expected when OBU Device Profile named: " + obuName + " is selected.", obuTab.isOBUApplicationsBtnEnabled());

        //Click the Applications button.
        DeviceApplicationsModal appsModal = obuTab.clickApplications();

        //Verify a Device Applications modal is displayed.
        Assert.assertTrue("The Device Applications modal is not displayed as expected when Applications button is clicked.", appsModal.isDeviceApplicationsHeaderDisplayed());

        //Verify the following icons are displayed in the top right corner of the modal: ‘?’ and ‘x’.
        appsModal.checkHeaderIcons();

        //Click the ‘?’ icon.
        appsModal.clickDeviceApplicationsHelpIcon();

        //Verify that that the Device Applications Help modal is displayed with instructions for configuration of devices.
        appsModal.checkHelpModal();

        //Verify an OK button is displayed in the modal.
        Assert.assertTrue("OK button is not displayed as expected in Device Applications Help modal.", appsModal.isDeviceApplicationsHelpOKBtnDisplayed());

        //Click the OK button.
        appsModal.clickDeviceApplicationsHelpOKBtn();

        //Verify the Help modal closes.
        Assert.assertFalse("The Device Applications Help modal is still displayed after clicking OK.", appsModal.isDeviceApplicationsHelpHeaderDisplayed());

        //Verify the following sections are displayed in the Device Applications modal: Available, Hosted, and Parameters.
        appsModal.checkAppSectionsDisplayed();

        //Verify all available apps for the OBU device display in the available apps section
        /*
         * Note: We are not tracking all the apps that may be available in the
         * application, so instead we are just going to check that *some* apps
         * are available. Note that this check presumes a) that the expected
         * default apps are installed (those included with the default
         * deployment) and b) the OBU Device is new and has not had some of
         * those devices assigned already
         */
        List<String> availableApps = appsModal.getAvailableApps();
        Assert.assertTrue("There were no available apps located in the Available Apps section.", availableApps.size() > 0);

        //Verify the Hosted section is currently empty.
        List<String> hostedApps = appsModal.getHostedApps();
        Assert.assertEquals("There were hosted apps located in the Hosted Apps section when none were expected.", 0, hostedApps.size());

        //Verify an Edit button is displayed in the Parameters section.
        Assert.assertTrue("Edit button could not be found in Parameters section.", appsModal.isEditBtnDisplayed());

        //Verify a table with a Name column and Value column is displayed in the Parameters section.
        appsModal.checkParametersTableHeaderCells();

        //Verify the following icons are displayed between the Available and Hosted sections: ‘>’ and ‘<’.
        Assert.assertTrue("Add app button ('>') is not displayed as expected.", appsModal.isAddAppBtnDisplayed());
        Assert.assertTrue("Remove app button ('<') is not displayed as expected.", appsModal.isRemoveAppBtnDisplayed());

        //Verify a Close button is displayed at the bottom of the Device Applications modal.
        Assert.assertTrue("Close button is not displayed as expected in the Device Applications modal.", appsModal.isCloseBtnDisplayed());

        //Verify all available apps for the selected device display in the available apps section
        /*
         * Note: We are not tracking all the apps that may be available in the
         * application, so instead we are just going to check that *some* apps
         * are available. Note that this check presumes a) that the expected
         * default apps are installed (those included with the default
         * deployment) and b) the OBU Device is new and has not had some of
         * those devices assigned already
         */
        Assert.assertTrue("There were no available apps located in the Available Apps section.", availableApps.size() > 0);

        //Select an application in the Available application list that has configurable parameters (i.e., BSMProducerApp).
        Assert.assertTrue("Configurable app named: " + configurableOBUAppName + " could not be found.", appsModal.isAppAvailable(configurableOBUAppName));
        appsModal.selectAvailableApp(configurableOBUAppName);

        //Click the ‘>’ icon.
        appsModal.clickAddApp();
        appsModal.waitForSpecificHostedApp(configurableOBUAppName);

        //Verify the selected application is moved from the Available section to the Hosted section.
        Assert.assertTrue("Newly added app is not listed as 'Hosted' after clicking the add button.  App name: '" + configurableOBUAppName + "'", appsModal.isAppHosted(configurableOBUAppName));

        //Select the application in the Hosted section.
        appsModal.selectHostedApp(configurableOBUAppName);

        //Verify the definable parameters for the application are displayed in the table in the Parameters section.
        Map<String, String> params = appsModal.getParamsAndValues();
        Assert.assertTrue("No parameters were found in the list.", params.size() > 0);

        //Verify a name is displayed in the Name column of the Parameters table for each of the application’s parameters.
        appsModal.checkAllParamNamesDisplayed(app);

        //Verify a value is displayed in the Value column of the Parameters table for each of the application’s parameters.
        appsModal.checkAllParamValuesDisplayed(app);

        //Verify the Edit button is disabled.
        Assert.assertFalse("Edit button is enabled without any parameters selected.", appsModal.isEditBtnEnabled());

        //Select any listed parameter..
        String paramName = app.getParameters().get(0).getParameterName();
        String paramValue = app.getParameters().get(0).getParameterValue();
        appsModal.selectParam(paramName);

        //Verify the Edit button is enabled.
        Assert.assertTrue("Edit button is disabled when a parameter is selected.", appsModal.isEditBtnEnabled());

        //Click the Edit button.
        EditParameterModal editParamForm = appsModal.clickEdit();

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
        Map<String, String> updatedParams = appsModal.getParamsAndValues();
        for (Entry<String, String> e : updatedParams.entrySet()) {
            Assert.assertEquals("Value mismatch after editing the parameter was cancelled.", params.get(e.getKey()), e.getValue());
        }

        //Select any parameter value.
        appsModal.selectParam(paramName);

        //Click the Edit button.
        appsModal.clickEdit();

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
        appsModal.selectParam(paramName);

        //Click the Edit button.
        appsModal.clickEdit();

        //Enter a new value in the Value text box.
        editParamForm.setParameterValue(newValue);

        //Click the Update button.
        editParamForm.clickUpdate(true);

        //Verify the value updates in the Parameters table.
        updatedParams = appsModal.getParamsAndValues();
        String updatedValue = updatedParams.get(paramName);
        Assert.assertEquals("The parameter value was not successfully updated.", newValue, updatedValue);

        //Click the Close button.
        appsModal.clickCloseBtn();

        //Logout
        simPage.logout(testuser);
    }

    /**
     * Test steps for ITC-053
     */
    @Test
    public void configureOBUDeviceInternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");

        //Verify Simulations page is displayed
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select a composite that has no executions and at least one OBU device.
        simPage.selectComposite(composite, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Composite Settings option.
        CompositeSettingsModal compositeSettingsModal = simPage.clickCompositeSettings();

        //Click the OBU Device Profiles tab.
        ConfigureOBUDeviceProfilesPartialPage obuTab = compositeSettingsModal.clickOBUTab();

        //Select any OBU device.
        obuTab.selectRow(obuName, true);

        //Click the Applications button.
        DeviceApplicationsModal appModal = obuTab.clickApplications();

        //Select an application in the Available application list that has configurable parameters (i.e., BSMProducerApp).
        appModal.selectAvailableApp(configurableOBUAppName);

        //Click the ‘>’ icon.
        appModal.clickAddApp();

        //Select the application in the Hosted section.
        appModal.selectHostedApp(configurableOBUAppName);

        //Select any listed parameter in the Parameters table.
        String paramName = app.getParameters().get(0).getParameterName();
        appModal.selectParam(paramName);

        //Click the Edit button.
        EditParameterModal editModal = appModal.clickEdit();

        //Delete the value in the Value text box.
        editModal.setParameterValue("");
        editModal.clickUpdate(false);

        //Verify an error is displayed associated with the Value text box indicating a valid parameter value is required.
        Assert.assertTrue("Parameter value required error not displayed when Value text box is empty.", editModal.isValueRequiredErrorDisplayed());

        //Enter a few spaces in the Value text box.
        editModal.setParameterValue("   ");

        //Verify an error is displayed associated with the Value text box indicating a valid parameter value is required.
        Assert.assertTrue("Parameter value required error not displayed when Value text box contains whitespace only.", editModal.isValueRequiredErrorDisplayed());

        //Enter a value in the Value text box that begins with a space (e.g., ‘ Test’).
        editModal.setParameterValue(" " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Value text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error not displayed when Value text box contains leading whitespace.", editModal.isValueLeadingTrailingWhitespaceErrorDisplayed());

        //Enter a value in the Value text box that ends with a space (e.g., ‘Test ’).
        editModal.setParameterValue(RandomStringGenerator.nextLetterString(5) + " ");

        //Verify an error is displayed associated with the Value text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error not displayed when Value text box contains trailing whitespace.", editModal.isValueLeadingTrailingWhitespaceErrorDisplayed());

        //Enter a new value in the Value text box.
        String newValue = RandomStringGenerator.nextLetterString(5);
        editModal.setParameterValue(newValue);

        //Click the Update button.
        editModal.clickUpdate(true);

        //Verify the value updates in the Parameters table.
        Map<String, String> updatedParams = appModal.getParamsAndValues();
        String updatedValue = updatedParams.get(paramName);
        Assert.assertEquals("The parameter value was not successfully updated.", newValue, updatedValue);

        //Click the Close button.
        appModal.clickCloseBtn();

        //Logout
        simPage.logout(testuser);
    }
}