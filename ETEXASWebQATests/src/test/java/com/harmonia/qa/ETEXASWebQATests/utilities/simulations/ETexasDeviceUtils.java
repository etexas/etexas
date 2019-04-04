package com.harmonia.qa.ETEXASWebQATests.utilities.simulations;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.OBUDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.RSEDevice;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeSettingsModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.ConfigureRSEDevicesPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CreateRSEDeviceModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.ConfigureOBUDeviceProfilesPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.CreateOBUDeviceProfileModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;

/**
 * Utility class to assist with RSE Devices
 *
 * @author cbulloss
 */
public class ETexasDeviceUtils extends ETexasCommonUtils {

    /**
     * Creates an RSE Device in the UI. Assumes the user is not logged in, and
     * includes navigating to the landing page. Also assumes that the device
     * being passed has all values (i.e. the simulation and the simulation's
     * user) are correctly set and exist in the application.
     *
     * @param device the device to create
     */
    public static void createRseDevice(RSEDevice device) {
        ETexasUser user = device.getSimulation().getUser();
        LandingPage landingPage = goToLandingPage();
        landingPage.waitUntilLoaded();
        SimulationsPage simPage = landingPage.loginAs(user);
        simPage.waitUntilLoaded();
        simPage.selectSim(device.getSimulation(), true);
        simPage.clickEdit();
        simPage.checkEditOptions();
        SimulationSettingsModal simSettingsModal = simPage.clickSimulationSettings();
        ConfigureRSEDevicesPartialPage rseConfigPage = simSettingsModal.clickRSEDevicesTab();
        CreateRSEDeviceModal rseForm = rseConfigPage.clickCreate();
        rseForm.setAllFields(device);
        rseForm.clickCreate(true);
        rseConfigPage.clickClose();
        simPage.logout(user);
    }

    /**
     * Creates an OBU Device in the UI. Assumes the user is not logged in, and
     * includes navigating to the landing page. Also assumes that the device
     * being passed has all values (i.e. the simulation and the simulation's
     * user) are correctly set and exist in the application.
     *
     * @param device the device to create
     */
    public static void createObuDevice(OBUDevice device) {
        ETexasUser user = device.getSimulation().getUser();
        LandingPage landingPage = goToLandingPage();
        landingPage.waitUntilLoaded();
        SimulationsPage simPage = landingPage.loginAs(user);
        simPage.waitUntilLoaded();
        simPage.selectSim(device.getSimulation(), true);
        simPage.clickEdit();
        CompositeSettingsModal compositeSettingsModal = simPage.clickCompositeSettings();
        ConfigureOBUDeviceProfilesPartialPage obuConfigPage = compositeSettingsModal.clickOBUTab();
        CreateOBUDeviceProfileModal obuModal = obuConfigPage.clickCreate();
        obuModal.setAllFields(device);
        obuModal.clickCreate(true);
        obuConfigPage.clickCloseBtn();
        simPage.waitUntilLoaded();
        simPage.logout(user);
    }

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Adds a given App to device with given name.Assumes the user is not logged
    //	 * in, and includes navigating to the landing page. Also assumes that the
    //	 * device being passed has all values and has been created in the
    //	 * application (i.e. the simulation and the simulation's user) are correctly
    //	 * set and exist in the application.
    //	 *
    //	 * @param sim -the simulation associted with the device
    //	 * @param deviceName -the name of the device on which the app should be
    //	 *        installed
    //	 * @param appName -the name of the app to install on the device
    //	 */
    //	public static void addAppToDevice(Simulation sim, String deviceName, String appName) {
    //		ETexasUser user = sim.getUser();
    //		LandingPage landingPage = goToLandingPage();
    //		landingPage.waitUntilLoaded();
    //		SimulationsPage simPage = landingPage.loginAs(user);
    //		simPage.waitUntilLoaded();
    //		simPage.selectSimCheckBox(sim, true);
    //		simPage.clickEdit();
    //		simPage.checkEditOptions();
    //		ConfigureDevicesForm devicesForm = simPage.clickSimulationSettings();
    //		devicesForm.selectCheckBox(deviceName, true);
    //		DeviceConfigurationForm deviceConfigForm = devicesForm.clickConfigure();
    //		deviceConfigForm.selectAvailableApp(appName);
    //		deviceConfigForm.clickAddApp();
    //		deviceConfigForm = getPage(DeviceConfigurationForm.class); //Update the page object to get most recent DOM
    //		deviceConfigForm.waitUntilLoaded();
    //		Assert.assertTrue("App, " + appName + ", is not displayed as installed after being added.", deviceConfigForm.isAppInstalled(appName));
    //		deviceConfigForm.clickClose();
    //		devicesForm.clickCloseBtn();
    //		simPage.logout(user);
    //	}

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Adds a given App to given OBU device.Assumes the user is not logged in,
    //	 * and includes navigating to the landing page. Also assumes that the device
    //	 * being passed has all values and has been created in the application (i.e.
    //	 * the simulation and the simulation's user) are correctly set and exist in
    //	 * the application.
    //	 *
    //	 * @param device -the OBU device on which the app should be installed
    //	 * @param appName -the name of the app to install on the device
    //	 */
    //	public static void addAppToDevice(OBUDevice device, String appName) {
    //		addAppToDevice(device.getSimulation(), device.getName(), appName);
    //	}

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Adds a given App to given RSE device.Assumes the user is not logged in,
    //	 * and includes navigating to the landing page. Also assumes that the device
    //	 * being passed has all values and has been created in the application (i.e.
    //	 * the simulation and the simulation's user) are correctly set and exist in
    //	 * the application.
    //	 *
    //	 * @param device -the RSE device on which the app should be installed
    //	 * @param appName -the name of the app to install on the device
    //	 */
    //	public static void addAppToDevice(RSEDevice device, String appName) {
    //		addAppToDevice(device.getSimulation(), device.getName(), appName);
    //	}

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Updates given app parameter value with associated device and app. Assumes
    //	 * the user is not logged in, and includes navigating to the landing page.
    //	 * Also assumes that the device being passed has all values, has been
    //	 * created in the application (i.e. the simulation and the simulation's
    //	 * user) are correctly set and exist in the application AND specified app
    //	 * has already been added to the device.
    //	 *
    //	 * @param sim -the simulation being configured
    //	 * @param deviceName -the name of the device being configured
    //	 * @param appName -the name of the app being configured
    //	 * @param paramName -the name of the app parameter being configured
    //	 * @param paramValue -the app parameter being updated
    //	 */
    //	public static void updateAppParameter(Simulation sim, String deviceName, String appName, String paramName, String paramValue) {
    //		ETexasUser user = sim.getUser();
    //		LandingPage landingPage = goToLandingPage();
    //		landingPage.waitUntilLoaded();
    //		SimulationsPage simPage = landingPage.loginAs(user);
    //		simPage.waitUntilLoaded();
    //		simPage.selectSimCheckBox(sim, true);
    //		simPage.clickEdit();
    //		simPage.checkEditOptions();
    //		ConfigureDevicesForm devicesForm = simPage.clickSimulationSettings();
    //		devicesForm.selectCheckBox(deviceName, true);
    //		DeviceConfigurationForm deviceConfigForm = devicesForm.clickConfigure();
    //		deviceConfigForm.selectInstalledApp(appName);
    //		Assert.assertTrue("Given paramter: " + paramName + " not displayed as expected.", deviceConfigForm.isAppParameterDisplayed(paramName));
    //		deviceConfigForm.selectParam(paramName);
    //		EditParameterForm editForm = deviceConfigForm.clickEdit();
    //		editForm.setParameterValue(paramValue);
    //		editForm.clickSaveAndWait();
    //		Assert.assertEquals("App parameter value not updated as expected.", paramValue, deviceConfigForm.getAppParamValue(paramName));
    //		deviceConfigForm.clickClose();
    //		devicesForm.clickCloseBtn();
    //		simPage.logout(user);
    //	}

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Updates given app parameter value with associated device and app. Assumes
    //	 * the user is not logged in, and includes navigating to the landing page.
    //	 * Also assumes that the device being passed has all values, has been
    //	 * created in the application (i.e. the simulation and the simulation's
    //	 * user) are correctly set and exist in the application AND specified app
    //	 * has already been added to the device.
    //	 *
    //	 * @param device -the OBU device on which the app should be installed
    //	 * @param appName -the name of the app installed on the device
    //	 * @param paramName -the app parameter being updated
    //	 * @param paramValue -the value of the parameter to set
    //	 */
    //	public static void updateAppParameter(OBUDevice device, String appName, String paramName, String paramValue) {
    //		updateAppParameter(device.getSimulation(), device.getName(), appName, paramName, paramValue);
    //	}

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Updates given app parameter value with associated device and app. Assumes
    //	 * the user is not logged in, and includes navigating to the landing page.
    //	 * Also assumes that the device being passed has all values, has been
    //	 * created in the application (i.e. the simulation and the simulation's
    //	 * user) are correctly set and exist in the application AND specified app
    //	 * has already been added to the device.
    //	 *
    //	 * @param device -the RSE device on which the app should be installed
    //	 * @param appName -the name of the app installed on the device
    //	 * @param paramName -the app parameter being updated
    //	 * @param paramValue -the value of the parameter to set
    //	 */
    //	public static void updateAppParameter(RSEDevice device, String appName, String paramName, String paramValue) {
    //		updateAppParameter(device.getSimulation(), device.getName(), appName, paramName, paramValue);
    //	}
}
