package com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.io.FileUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.AppParameter;
import com.harmonia.qa.ETEXASWebQATests.entities.CellularDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.UserJarApp;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.AppFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.CellularDeviceFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.enums.DeviceType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.apps.ETexasAppUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.EditParameterModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeSettingsModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.DeviceApplicationsModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.ConfigureCellularDevicePartialPage;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class for configuring cellular devices, TC-073/ITC-054
 *
 * @author llaroussini
 * @author saistrop
 */
public class ConfigureCellDeviceTest extends ETexasAfterTestResetTestBase {

    /**
     * The user used in the test case
     */
    private ETexasUser testuser;

    /**
     * The simulation used in the test case.
     */
    private TemplateSimulation simulation;

    /**
     * The Cellular Device to be configures
     */
    private CellularDevice testCellDevice;

    /**
     * The name of the Cellular Device used throughout the test case
     */
    private String testCellDeviceName;

    /**
     * The user jar app used in the test case
     */
    private UserJarApp app;

    /**
     * The App file which will be uploaded
     */
    private File uploadedAppFile;

    /**
     * The name of an app file which can be uploaded for a cellular device
     */
    private String uploadCellAppFileName = "cellular-apps-2.0.jar";

    /**
     * File path to access JAR file
     */
    private String applicationFilePath = "src/test/resources";

    /**
     * The name of an app which can be configured (i.e. has parameters) and is
     * available on cellular device
     */
    private String configurableCellAppName = "CellMessageProducerApp";

    /**
     * The Composite Simulation used throughout the test case
     */
    private CompositeSimulation composite;

    /**
     * Prerequisite steps/test setup
     *
     * @throws IOException
     */
    @Before
    public void warmUp() throws IOException {
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

        //An application jar file exists
        uploadedAppFile = FileUtils.getFile(applicationFilePath, uploadCellAppFileName);

        //Application to be used throughout test
        app = AppFactory.getUserJarApp(configurableCellAppName, DeviceType.CELLULAR, uploadedAppFile);
        app.setUser(testuser);

        //TODO Break this section out into the base data once it becomes embedded so it can be reused in other test classes
        app.setDeviceType(DeviceType.CELLULAR);
        List<AppParameter> appParams = new ArrayList<AppParameter>(2);
        AppParameter destMacParam = new AppParameter();
        destMacParam.setParameterName("destMac");
        destMacParam.setParameterValue("0");
        destMacParam.setApp(app);
        AppParameter frequencyParam = new AppParameter();
        frequencyParam.setParameterName("frequency");
        frequencyParam.setParameterValue("0.1");
        frequencyParam.setApp(app);
        appParams.add(destMacParam);
        appParams.add(frequencyParam);
        app.setParameters(appParams);
        ETexasEntityManager.addEntities(destMacParam, frequencyParam);

        //Add test simulation to Entity Manager
        ETexasEntityManager.addEntities(testuser, simulation, testCellDevice, app);

        //User registration and creation of Composite with Cellular Device
        LandingPage landing = ETexasUserUtils.userRegistration(testuser);
        ETexasSimulationUtils.createTemplateSimulationWithCellularDevice(simulation, testCellDevice);

        //Create the User Jar application in the UI
        ETexasAppUtils.createJARApp(app, uploadedAppFile);

        //User logged in
        landing.loginAs(testuser);
    }

    /**
     * Test steps for TC-073
     *
     * @throws IOException if errors are encountered reading the file name/path
     *         - likely an issue with file permissions
     */
    @Test
    public void configureCellularDeviceExternalTest() throws IOException {
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

        //Select cellular device profile and verify Edit button is enabled
        cellDevicesPage.selectCellularDevice(testCellDevice);

        //Verify Application button is enabled
        Assert.assertTrue("Application button is not enabled for device " + testCellDeviceName + " when expected.", cellDevicesPage.isCellularApplicationsBtnEnabled());

        //Click Applications button and get DeviceApplicationsModal
        DeviceApplicationsModal deviceAppForm = cellDevicesPage.clickApplications();
        Assert.assertTrue("Device Applications modal did not appear after clicking the Applications button.", deviceAppForm.isDeviceApplicationsHeaderDisplayed());

        //Verify Help and Close icons appear in the Device Applications modal
        deviceAppForm.checkHeaderIcons();

        //Click the help icon and verify help modal appears
        deviceAppForm.clickDeviceApplicationsHelpIcon();
        Assert.assertTrue("The Device Applications Help header is not displayed after clicking the Help icon.", deviceAppForm.isDeviceApplicationsHelpHeaderDisplayed());
        Assert.assertTrue("The Help content did not display where expected.", deviceAppForm.isDeviceApplicationsHelpContentDisplayed());
        Assert.assertTrue("The OK button was not displayed for the help modal after clicking the Help icon.", deviceAppForm.isDeviceApplicationsHelpOKBtnDisplayed());

        //Click OK Button and verify Help modal closes
        deviceAppForm.clickDeviceApplicationsHelpOKBtn();
        Assert.assertFalse("Help modal is still present after clicking the OK button.", deviceAppForm.isDeviceApplicationsHelpHeaderDisplayed());

        //Verify the Available, Hosted, and Parameters headers are displayed
        deviceAppForm.checkAppSectionsDisplayed();

        //Verify all available apps for the Cellular device display in the available apps section
        /*
         * Note: We are not tracking all the apps that may be available in the
         * application, so instead we are just going to check that *some* apps
         * are available. Note that this check presumes a) that the expected
         * default apps are installed (those included with the default
         * deployment) and b) the Cellular Device is new and has not had some of
         * those devices assigned already
         */
        List<String> availableApps = deviceAppForm.getAvailableApps();
        Assert.assertTrue("There were no available apps located in the Available Apps section.", availableApps.size() > 0);

        //Verify the Hosted section is currently empty.
        List<String> hostedApps = deviceAppForm.getHostedApps();
        Assert.assertEquals("There were hosted apps located in the Hosted Apps section when none were expected.", 0, hostedApps.size());

        //Verify an Edit button is displayed in the Parameters section.
        Assert.assertTrue("Edit button could not be found in Parameters section.", deviceAppForm.isEditBtnDisplayed());

        //Verify a table with a Name column and Value column is displayed in the Parameters section.
        deviceAppForm.checkParametersTableHeaderCells();

        //Verify the following icons are displayed between the Available and Hosted sections: ‘>’ and ‘<’.
        Assert.assertTrue("Add app button ('>') is not displayed as expected.", deviceAppForm.isAddAppBtnDisplayed());
        Assert.assertTrue("Remove app button ('<') is not displayed as expected.", deviceAppForm.isRemoveAppBtnDisplayed());

        //Verify a Close button is displayed at the bottom of the Device Applications modal.
        Assert.assertTrue("Close button is not displayed as expected in the Device Applications modal.", deviceAppForm.isCloseBtnDisplayed());

        //Verify all available apps for the selected device display in the available apps section
        /*
         * Note: We are not tracking all the apps that may be available in the
         * application, so instead we are just going to check that *some* apps
         * are available. Note that this check presumes a) that the expected
         * default apps are installed (those included with the default
         * deployment) and b) the Cellular Device is new and has not had some of
         * those devices assigned already
         */
        Assert.assertTrue("There were no available apps located in the Available Apps section.", availableApps.size() > 0);

        //Select an application in the Available application list that has configurable parameters (i.e., BSMProducerApp).
        Assert.assertTrue("Configurable app named: " + configurableCellAppName + " could not be found.", deviceAppForm.isAppAvailable(configurableCellAppName));
        deviceAppForm.selectAvailableApp(configurableCellAppName);

        //Click the ‘>’ icon.
        deviceAppForm.clickAddApp();
        deviceAppForm.waitForSpecificHostedApp(configurableCellAppName);

        //Verify the selected application is moved from the Available section to the Hosted section.
        Assert.assertTrue("Newly added app is not listed as 'Hosted' after clicking the add button.  App name: '" + configurableCellAppName + "'", deviceAppForm.isAppHosted(configurableCellAppName));

        //Select the application in the Hosted section.
        deviceAppForm.selectHostedApp(configurableCellAppName);

        //Verify the definable parameters for the application are displayed in the table in the Parameters section.
        Map<String, String> params = deviceAppForm.getParamsAndValues();
        Assert.assertTrue("No parameters were found in the list.", params.size() > 0);

        //Verify a name is displayed in the Name column of the Parameters table for each of the application’s parameters.
        deviceAppForm.checkAllParamNamesDisplayed(app);

        //Verify a value is displayed in the Value column of the Parameters table for each of the application’s parameters.
        deviceAppForm.checkAllParamValuesDisplayed(app);

        //Verify the Edit button is disabled.
        Assert.assertFalse("Edit button is enabled without any parameters selected.", deviceAppForm.isEditBtnEnabled());

        //Select any listed parameter.
        String paramName = app.getParameters().get(0).getParameterName();
        String paramValue = app.getParameters().get(0).getParameterValue();
        deviceAppForm.selectParam(paramName);

        //Verify the Edit button is enabled.
        Assert.assertTrue("Edit button is disabled when a parameter is selected.", deviceAppForm.isEditBtnEnabled());

        //Click the Edit button.
        EditParameterModal editParamForm = deviceAppForm.clickEdit();

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
        Map<String, String> updatedParams = deviceAppForm.getParamsAndValues();
        for (Entry<String, String> e : updatedParams.entrySet()) {
            Assert.assertEquals("Value mismatch after editing the parameter was cancelled.", params.get(e.getKey()), e.getValue());
        }

        //Select any parameter value.
        deviceAppForm.selectParam(paramName);

        //Click the Edit button.
        deviceAppForm.clickEdit();

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
        deviceAppForm.selectParam(paramName);

        //Click the Edit button.
        deviceAppForm.clickEdit();

        //Enter a new value in the Value text box.
        editParamForm.setParameterValue(newValue);

        //Click the Update button.
        editParamForm.clickUpdate(true);

        //Verify the value updates in the Parameters table.
        updatedParams = deviceAppForm.getParamsAndValues();
        String updatedValue = updatedParams.get(paramName);
        Assert.assertEquals("The parameter value was not successfully updated.", newValue, updatedValue);

        //Click the Close button.
        deviceAppForm.clickCloseBtn();

        //Log out
        simPage.logout(testuser);
    }

    /**
     * Test steps for ITC-054
     */
    @Test
    public void configureCellularDeviceInternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Ensure Simulations page is loaded
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select a composite that has no executions and at least one cellular device.
        simPage.selectComposite(composite, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Composite Settings option.
        CompositeSettingsModal compositeSettingsModal = simPage.clickCompositeSettings();

        //Click the Cellular Device Profiles tab.
        ConfigureCellularDevicePartialPage cellTab = compositeSettingsModal.clickCellularTab();

        //Select any Cellular device.
        cellTab.selectCellularDevice(testCellDevice);

        //Click the Applications button.
        DeviceApplicationsModal appModal = cellTab.clickApplications();

        //Select an application in the Available application list that has configurable parameters (i.e., BSMProducerApp).
        appModal.selectAvailableApp(configurableCellAppName);

        //Click the ‘>’ icon.
        appModal.clickAddApp();

        //Select the application in the Hosted section.
        appModal.selectHostedApp(configurableCellAppName);

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
