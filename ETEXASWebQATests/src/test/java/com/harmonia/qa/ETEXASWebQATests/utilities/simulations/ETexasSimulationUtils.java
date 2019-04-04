package com.harmonia.qa.ETEXASWebQATests.utilities.simulations;

import java.util.List;

import org.junit.Assert;

import com.harmonia.qa.ETEXASWebQATests.entities.CellTower;
import com.harmonia.qa.ETEXASWebQATests.entities.CellularDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.Detector;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.FixedCellularDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.OBUDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.RSEDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.parital.environment.ConfigureDetectorsPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.parital.environment.CreateDetectorModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeSettingsModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.ConfigureCellTowersPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.ConfigureFixedCellularDevicePartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.ConfigureRSEDevicesPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CreateCellTowerModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CreateFixedCellularDeviceModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CreateRSEDeviceModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.DeviceApplicationsModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.ConfigureCellularDevicePartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.ConfigureOBUDeviceProfilesPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.ConfigureOBUDeviceProfilesPartialPage.OBUDeviceTableColumn;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.CreateCellularDeviceModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.CreateOBUDeviceProfileModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulations.CreateSimulationFromTemplateModal;

/**
 * Utilities pertaining to ETexas Simulations
 *
 * @author llaroussini
 * @author cbulloss
 */
public class ETexasSimulationUtils extends ETexasCommonUtils {

    /**
     * Creates a new template simulation in the UI. Assumes the user is not
     * logged in; handles navigation to the landing page. Logs the user out at
     * completion
     *
     * @param sim the simulation to create
     */
    public static void createTemplateSimulation(TemplateSimulation sim) {
        ETexasUser user = sim.getUser();
        //Login and navigate to simulations page
        LandingPage landing = ETexasCommonUtils.goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        //Click New, Template, enter details
        simPage.clickCreate();
        CreateSimulationFromTemplateModal templateForm = simPage.clickTemplate();
        templateForm.setAllFields(sim);
        templateForm.clickCreate(true);
        simPage.logout(user);
    }

    /**
     * Creates a new template simulation with given detector in the UI. Also
     * handles verifying the details of the simulation. Assumes the user is not
     * logged in; handles navigation to the landing page. Logs the user out at
     * completion
     *
     * @param sim the simulation to create
     * @param detector the detector to be associated with the simulation
     */
    public static void createTemplateSimulationWithDetector(TemplateSimulation sim, Detector detector) {
        ETexasUser user = sim.getUser();
        //Login and navigate to simulations page
        LandingPage landing = goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        simPage.waitUntilLoaded();
        //Click New, Template, enter details
        simPage.clickCreate();
        CreateSimulationFromTemplateModal templateForm = simPage.clickTemplate();
        templateForm.setAllFields(sim);
        templateForm.clickCreate(true);
        simPage.checkSimulationDetails(sim);
        //Create RSE device
        simPage.clickEdit();
        SimulationSettingsModal simSettingsModal = simPage.clickSimulationSettings();
        ConfigureDetectorsPartialPage detectorsForm = simSettingsModal.clickDetectorsTab();
        CreateDetectorModal addDetector = detectorsForm.clickCreateDetectorBtn();
        addDetector.setAllFields(detector);
        addDetector.clickCreate(true);
        Assert.assertTrue("Detector in lane " + detector.getLane().getLaneID() + " could not be found.", detectorsForm.isDetectorDisplayed(detector.getHeight()));
        detector.setID(detectorsForm.getDetectorID(detector));
        detectorsForm.clickClose();
        simPage.logout(user);
    }

    /**
     * Creates a new template simulation with given RSE device in the UI. Also
     * handles verifying the details of the simulation. Assumes the user is not
     * logged in; handles navigation to the landing page. Logs the user out at
     * completion
     *
     * @param sim the simulation to create
     * @param device the rse device to be associated with the simulation
     */
    public static void createTemplateSimulationWithRSEDevice(TemplateSimulation sim, RSEDevice device) {
        ETexasUser user = sim.getUser();
        //Login and navigate to simulations page
        LandingPage landing = goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        simPage.waitUntilLoaded();
        //Click New, Template, enter details
        simPage.clickCreate();
        CreateSimulationFromTemplateModal templateForm = simPage.clickTemplate();
        templateForm.setAllFields(sim);
        templateForm.clickCreate(true);
        simPage.checkSimulationDetails(sim);
        //Create RSE device
        simPage.clickEdit();
        SimulationSettingsModal simSettingsModal = simPage.clickSimulationSettings();
        ConfigureRSEDevicesPartialPage rseConfigPage = simSettingsModal.clickRSEDevicesTab();
        CreateRSEDeviceModal rseForm = rseConfigPage.clickCreate();
        rseForm.setAllFields(device);
        rseForm.clickCreate(true);
        device.setID(rseConfigPage.getRSEID(device));
        Assert.assertTrue("RSE device with name " + device.getName() + " is not displayed after being created.", rseConfigPage.isRSEDisplayed(device));
        rseConfigPage.clickClose();
        simPage.logout(user);
    }

    /**
     * Creates a new template simulation with given list of RSE devices in the
     * UI. Also handles verifying the details of the simulation. Assumes the
     * user is not logged in; handles navigation to the landing page. Logs the
     * user out at completion
     *
     * @param sim the simulation to create
     * @param devices the list of rse devices to be associated with the
     *        simulation
     */
    public static void createTemplateSimulationWithRSEDevices(TemplateSimulation sim, List<RSEDevice> devices) {
        ETexasUser user = sim.getUser();
        //Login and navigate to simulations page
        LandingPage landing = goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        simPage.waitUntilLoaded();
        //Click New, Template, enter details
        simPage.clickCreate();
        CreateSimulationFromTemplateModal templateForm = simPage.clickTemplate();
        templateForm.setAllFields(sim);
        templateForm.clickCreate(true);
        simPage.checkSimulationDetails(sim);
        //Create RSE device
        simPage.clickEdit();
        SimulationSettingsModal simSettingsModal = simPage.clickSimulationSettings();
        ConfigureRSEDevicesPartialPage rseConfigPage = simSettingsModal.clickRSEDevicesTab();
        for (RSEDevice device : devices) {
            CreateRSEDeviceModal rseForm = rseConfigPage.clickCreate();
            rseForm.setAllFields(device);
            rseForm.clickCreate(true);
            device.setID(rseConfigPage.getRSEID(device));
            Assert.assertTrue("RSE device with name " + device.getName() + " is not displayed after being created.", rseConfigPage.isRSEDisplayed(device));
        }
        rseConfigPage.clickClose();
        simPage.logout(user);
    }

    /**
     * Creates a new template simulation with given OBU device in the UI. Also
     * handles verifying the details of the simulation. Assumes the user is not
     * logged in; handles navigation to the landing page. Logs the user out at
     * completion
     *
     * @param sim the simulation to create
     * @param device the obu device to be associated with the simulation
     */
    public static void createTemplateSimulationWithOBUDevice(TemplateSimulation sim, OBUDevice device) {
        ETexasUser user = sim.getUser();
        //Login and navigate to simulations page
        LandingPage landing = goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        simPage.waitUntilLoaded();
        //Click New, Template, enter details
        simPage.clickCreate();
        CreateSimulationFromTemplateModal templateForm = simPage.clickTemplate();
        templateForm.setAllFields(sim);
        templateForm.clickCreate(true);
        simPage.checkSimulationDetails(sim);
        //Create OBU device
        simPage.selectComposite(sim.getComposite(), true);
        simPage.clickEdit();
        CompositeSettingsModal compositeSettingsModal = simPage.clickCompositeSettings();
        ConfigureOBUDeviceProfilesPartialPage obuConfigPage = compositeSettingsModal.clickOBUTab();
        CreateOBUDeviceProfileModal obuModal = obuConfigPage.clickCreate();
        obuModal.setAllFields(device);
        obuModal.clickCreate(true);
        Assert.assertTrue("OBU device with name " + device.getName() + " is not displayed after being created.", obuConfigPage.isOBUDisplayed(device));
        device.setID(obuConfigPage.getOBUCellValue(device, OBUDeviceTableColumn.ID));
        obuConfigPage.clickCloseBtn();
        simPage.logout(user);
    }

    /**
     * Creates a new template simulation with given OBU devices in the UI. Also
     * handles verifying the details of the simulation. Assumes the user is not
     * logged in; handles navigation to the landing page. Logs the user out at
     * completion
     *
     * @param sim the simulation to create
     * @param devices list of obu devices to be associated with the simulation
     */
    public static void createTemplateSimulationWithOBUDevices(TemplateSimulation sim, List<OBUDevice> devices) {
        ETexasUser user = sim.getUser();
        //Login and navigate to simulations page
        LandingPage landing = goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        simPage.waitUntilLoaded();
        //Click New, Template, enter details
        simPage.clickCreate();
        CreateSimulationFromTemplateModal templateForm = simPage.clickTemplate();
        templateForm.setAllFields(sim);
        templateForm.clickCreate(true);
        simPage.checkSimulationDetails(sim);
        //Create OBU device
        simPage.selectComposite(sim.getComposite(), true);
        simPage.clickEdit();
        CompositeSettingsModal compositeSettingsModal = simPage.clickCompositeSettings();
        ConfigureOBUDeviceProfilesPartialPage obuConfigPage = compositeSettingsModal.clickOBUTab();
        for (OBUDevice device : devices) {
            CreateOBUDeviceProfileModal obuModal = obuConfigPage.clickCreate();
            obuModal.setAllFields(device);
            obuModal.clickCreate(true);
            Assert.assertTrue("OBU device with name " + device.getName() + " is not displayed after being created.", obuConfigPage.isOBUDisplayed(device));
            device.setID(obuConfigPage.getOBUCellValue(device, OBUDeviceTableColumn.ID));
        }
        obuConfigPage.clickCloseBtn();
        simPage.logout(user);
    }

    /**
     * Creates a new template simulation with given OBU device AND RSE device in
     * the UI. Also handles verifying the details of the simulation. Assumes the
     * user is not logged in; handles navigation to the landing page. Logs the
     * user out at completion
     *
     * @param sim the simulation to create
     * @param obu the obu device to be associated with the simulation
     * @param rse the rse device to be associated with the simulation
     */
    public static void createTemplateSimulationWithOBUAndRSEDevices(TemplateSimulation sim, OBUDevice obu, RSEDevice rse) {
        ETexasUser user = sim.getUser();
        //Login and navigate to simulations page
        LandingPage landing = goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        simPage.waitUntilLoaded();
        //Click New, Template, enter details
        simPage.clickCreate();
        CreateSimulationFromTemplateModal templateForm = simPage.clickTemplate();
        templateForm.setAllFields(sim);
        templateForm.clickCreate(true);
        simPage.checkSimulationDetails(sim);
        //Create RSE device
        simPage.clickEdit();
        SimulationSettingsModal simSettingsModal = simPage.clickSimulationSettings();
        ConfigureRSEDevicesPartialPage rseConfigPage = simSettingsModal.clickRSEDevicesTab();
        CreateRSEDeviceModal rseForm = rseConfigPage.clickCreate();
        rseForm.setAllFields(rse);
        rseForm.clickCreate(true);
        rse.setID(rseConfigPage.getRSEID(rse));
        Assert.assertTrue("RSE device with name " + rse.getName() + " is not displayed after being created.", rseConfigPage.isRSEDisplayed(rse));
        rseConfigPage.clickClose();
        //Create OBU device
        simPage.clickEdit();
        CompositeSettingsModal compositeSettingsModal = simPage.clickCompositeSettings();
        ConfigureOBUDeviceProfilesPartialPage obuConfigPage = compositeSettingsModal.clickOBUTab();
        CreateOBUDeviceProfileModal obuModal = obuConfigPage.clickCreate();
        obuModal.setAllFields(obu);
        obuModal.clickCreate(true);
        Assert.assertTrue("OBU device with name " + obu.getName() + " is not displayed after being created.", obuConfigPage.isOBUDisplayed(obu));
        obu.setID(obuConfigPage.getOBUCellValue(obu, OBUDeviceTableColumn.ID));
        obuConfigPage.clickCloseBtn();
        //Logout
        simPage.logout(user);
    }

    /**
     * Creates a new template simulation with given cellular device in the UI.
     * Also handles verifying the details of the simulation. Assumes the user is
     * not logged in; handles navigation to the landing page. Logs the user out
     * at completion
     *
     * @param sim the simulation to create
     * @param device the cellular device to be associated with the simulation
     */
    public static void createTemplateSimulationWithCellularDevice(TemplateSimulation sim, CellularDevice device) {
        ETexasUser user = sim.getUser();
        //Login and navigate to simulations page
        LandingPage landing = goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        simPage.waitUntilLoaded();
        //Click New, Template, enter details
        simPage.clickCreate();
        CreateSimulationFromTemplateModal templateForm = simPage.clickTemplate();
        templateForm.setAllFields(sim);
        templateForm.clickCreate(true);
        simPage.checkSimulationDetails(sim);
        simPage.selectComposite(sim.getComposite(), true);
        //Create cellular device
        simPage.clickEdit();
        CompositeSettingsModal compositeSettingsModal = simPage.clickCompositeSettings();
        ConfigureCellularDevicePartialPage cellConfigPage = compositeSettingsModal.clickCellularTab();
        CreateCellularDeviceModal cellForm = cellConfigPage.clickCreateCellularDeviceBtn();
        cellForm.setAllFields(device);
        cellForm.clickCreate(true);
        device.setDeviceId(cellConfigPage.getCellularDeviceRowID(device.getName()).getText());
        Assert.assertTrue("Cellular device with name " + device.getName() + " is not displayed after being created.", cellConfigPage.isCellularDeviceDisplayed(device.getName()));
        cellConfigPage.clickCloseBtn();
        simPage.logout(user);
    }

    /**
     * Creates a new template simulation with multiple cellular devices in the
     * UI. Also handles verifying the details of the simulation. Assumes the
     * user is not logged in; handles navigation to the landing page. Logs the
     * user out at completion
     *
     * @param sim the simulation to create
     * @param devices the list of cellular device to be associated with the
     *        simulation
     */
    public static void createTemplateSimulationWithCellularDevices(TemplateSimulation sim, List<CellularDevice> devices) {
        ETexasUser user = sim.getUser();
        //Login and navigate to simulations page
        LandingPage landing = goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        simPage.waitUntilLoaded();
        //Click New, Template, enter details
        simPage.clickCreate();
        CreateSimulationFromTemplateModal templateForm = simPage.clickTemplate();
        templateForm.setAllFields(sim);
        templateForm.clickCreate(true);
        simPage.checkSimulationDetails(sim);
        simPage.selectComposite(sim.getComposite(), true);
        simPage.clickEdit();
        CompositeSettingsModal compositeSettingsModal = simPage.clickCompositeSettings();
        ConfigureCellularDevicePartialPage cellConfigPage = compositeSettingsModal.clickCellularTab();
        for (CellularDevice device : devices) {
            CreateCellularDeviceModal cellForm = cellConfigPage.clickCreateCellularDeviceBtn();
            cellForm.setAllFields(device);
            cellForm.clickCreate(true);
            device.setDeviceId(cellConfigPage.getCellularDeviceRowID(device.getName()).getText());
            Assert.assertTrue("Cellular device with name " + device.getName() + " is not displayed after being created.", cellConfigPage.isCellularDeviceDisplayed(device.getName()));
        }
        cellConfigPage.clickCloseBtn();
        simPage.logout(user);
    }

    /**
     * Creates a new template simulation with given fixed cellular device in the
     * UI. Also handles verifying the details of the simulation. Assumes the
     * user is not logged in; handles navigation to the landing page. Logs the
     * user out at completion
     *
     * @param sim the simulation to create
     * @param device the fixed cellular device to be associated with the
     *        simulation
     */
    public static void createTemplateSimulationWithFixedCellularDevice(TemplateSimulation sim, FixedCellularDevice device) {
        ETexasUser user = sim.getUser();
        //Login and navigate to simulations page
        LandingPage landing = goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        simPage.waitUntilLoaded();
        //Click New, Template, enter details
        simPage.clickCreate();
        CreateSimulationFromTemplateModal templateForm = simPage.clickTemplate();
        templateForm.setAllFields(sim);
        templateForm.clickCreate(true);
        simPage.checkSimulationDetails(sim);
        //Create cellular device
        simPage.clickEdit();
        SimulationSettingsModal simSettingModal = simPage.clickSimulationSettings();
        ConfigureFixedCellularDevicePartialPage cellConfigPage = simSettingModal.clickFixedCellularDevicesTab();
        CreateFixedCellularDeviceModal cellForm = cellConfigPage.clickCreateFixedCellularDeviceBtn();
        cellForm.setAllFields(device);
        cellForm.clickCreate();
        Assert.assertTrue("Fixed cellular device with name " + device.getName() + " is not displayed after being created.", cellConfigPage.isFixedCellularDeviceDisplayed(device.getXCoordinate()));
        device.setID(cellConfigPage.getFixedCellularID(device));
        cellConfigPage.clickClose();
        simPage.logout(user);
    }

    /**
     * Creates a new template simulation with given list of fixed cellular
     * devices in the UI. Also handles verifying the details of the simulation.
     * Assumes the user is not logged in; handles navigation to the landing
     * page. Logs the user out at completion
     *
     * @param sim the simulation to create
     * @param devices the list of fixed cellular devices to be associated with
     *        the simulation
     */
    public static void createTemplateSimulationWithFixedCellularDevices(TemplateSimulation sim, List<FixedCellularDevice> devices) {
        ETexasUser user = sim.getUser();
        //Login and navigate to simulations page
        LandingPage landing = goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        simPage.waitUntilLoaded();
        //Click New, Template, enter details
        simPage.clickCreate();
        CreateSimulationFromTemplateModal templateForm = simPage.clickTemplate();
        templateForm.setAllFields(sim);
        templateForm.clickCreate(true);
        simPage.checkSimulationDetails(sim);
        //Create cellular device
        simPage.clickEdit();
        SimulationSettingsModal simSettingModal = simPage.clickSimulationSettings();
        ConfigureFixedCellularDevicePartialPage cellConfigPage = simSettingModal.clickFixedCellularDevicesTab();
        for (FixedCellularDevice device : devices) {
            CreateFixedCellularDeviceModal cellForm = cellConfigPage.clickCreateFixedCellularDeviceBtn();
            cellForm.setAllFields(device);
            cellForm.clickCreate();
            device.setID(cellConfigPage.getFixedCellularID(device));
            Assert.assertTrue("Fixed cellular device with name " + device.getName() + " is not displayed after being created.", cellConfigPage.isFixedCellularDeviceDisplayed(device.getXCoordinate()));
        }
        cellConfigPage.clickClose();
        simPage.logout(user);
    }

    /**
     * Creates a template simulation configured with the given app list
     * configured on the Report device. Method verifies apps are displayed as
     * installed apps prior to closing. Method also handles user logout
     * following configuration.
     *
     * @param sim -the simulation to create
     * @param appNames -list of the names of apps to add to ReportDevice
     */
    public static void createTemplateSimulationWithDefaultReportDevice(TemplateSimulation sim, List<String> appNames) {
        ETexasUser user = sim.getUser();
        //Login and navigate to simulations page
        LandingPage landing = goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        simPage.waitUntilLoaded();
        //Click New, Template, enter details
        simPage.clickCreate();
        CreateSimulationFromTemplateModal templateForm = simPage.clickTemplate();
        templateForm.setAllFields(sim);
        templateForm.clickCreate(true);
        simPage.checkSimulationDetails(sim);
        //Configure Report Device
        simPage.clickEdit();
        DeviceApplicationsModal deviceConfig = simPage.clickReporting();
        for (String appName : appNames) {
            deviceConfig.selectAvailableApp(appName);
            deviceConfig.clickAddApp();
            deviceConfig.waitForSpecificHostedApp(appName);
            Assert.assertTrue(appName + " not displayed in installed apps as expected.", deviceConfig.isAppHosted(appName));
        }
        deviceConfig.clickCloseBtn();
        simPage.logout(user);
    }

    /**
     * Creates a template simulation configured with the given report app list
     * configured on the Report device AND given OBU app(s) on OBU device.
     * Method verifies apps are displayed as installed apps prior to closing.
     * Method also handles user logout following configuration.
     *
     * @param sim -the simulation to create
     * @param reportAppNames -list of the names of apps to add to ReportDevice
     * @param obu -the OBU device to add
     * @param obuAppNames -list of the names of app to add to OBU
     */
    public static void createTemplateSimulationWithDefaultReportDeviceAndOBU(TemplateSimulation sim, List<String> reportAppNames, OBUDevice obu, List<String> obuAppNames) {
        ETexasUser user = sim.getUser();
        //Login and navigate to simulations page
        LandingPage landing = goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        simPage.waitUntilLoaded();
        //Click New, Template, enter details
        simPage.clickCreate();
        CreateSimulationFromTemplateModal templateForm = simPage.clickTemplate();
        templateForm.setAllFields(sim);
        templateForm.clickCreate(true);
        simPage.checkSimulationDetails(sim);
        //Configure Report Device
        simPage.clickEdit();
        DeviceApplicationsModal reportConfig = simPage.clickReporting();
        for (String appName : reportAppNames) {
            reportConfig.selectAvailableApp(appName);
            reportConfig.clickAddApp();
            reportConfig.waitForSpecificHostedApp(appName);
            Assert.assertTrue(appName + " not displayed in installed apps for Report Device as expected.", reportConfig.isAppHosted(appName));
        }
        reportConfig.clickCloseBtn();
        //Add and Configure OBU device
        simPage.clickEdit();
        CompositeSettingsModal compositeSettingsModal = simPage.clickCompositeSettings();
        ConfigureOBUDeviceProfilesPartialPage obuDevicesForm = compositeSettingsModal.clickOBUTab();
        CreateOBUDeviceProfileModal obuModal = obuDevicesForm.clickCreate();
        obuModal.setAllFields(obu);
        obuModal.clickCreate(true);
        DeviceApplicationsModal appsModal = obuDevicesForm.clickApplications();
        for (String appName : obuAppNames) {
            appsModal.selectAvailableApp(appName);
            appsModal.clickAddApp();
            appsModal.waitForSpecificHostedApp(appName);
            Assert.assertTrue(appName + " not displayed in hosted apps as expected.", appsModal.isAppHosted(appName));
        }
        appsModal.clickCloseBtn();
        obuDevicesForm.clickCloseBtn();
        simPage.logout(user);
    }

    /**
     * Creates a new template simulation with given cell tower in the UI. Also
     * handles verifying the details of the simulation. Assumes the user is not
     * logged in; handles navigation to the landing page. Logs the user out at
     * completion
     *
     * @param sim the simulation to create
     * @param tower the cell tower to be associated with the simulation
     */
    public static void createTemplateSimulationWithCellTower(TemplateSimulation sim, CellTower tower) {
        ETexasUser user = sim.getUser();
        //Login and navigate to simulations page
        LandingPage landing = goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        simPage.waitUntilLoaded();
        //Click New, Template, enter details
        simPage.clickCreate();
        CreateSimulationFromTemplateModal templateForm = simPage.clickTemplate();
        templateForm.setAllFields(sim);
        templateForm.clickCreate(true);
        simPage.checkSimulationDetails(sim);
        //Create Cell Tower
        simPage.clickEdit();
        SimulationSettingsModal simSettingsModal = simPage.clickSimulationSettings();
        ConfigureCellTowersPartialPage towersPage = simSettingsModal.clickCellTowersTab();
        CreateCellTowerModal addTowerForm = towersPage.clickCreateCellTowerBtn();
        addTowerForm.setAllFields(tower);
        addTowerForm.clickCreate(true);
        Assert.assertTrue("Cell Tower with Provider, " + tower.getProvider() + ", could not be found.", towersPage.isCellTowerDisplayed(tower.getProvider()));
        tower.setID(towersPage.getCellTowerID(tower));
        towersPage.clickClose();
        simPage.logout(user);
    }

    /**
     * Creates a new template simulation with given rse, obu, cell device, and
     * fixed cell device in the UI. Also handles verifying the details of the
     * simulation. Assumes the user is not logged in; handles navigation to the
     * landing page. Logs the user out at completion
     *
     * @param sim - the simulation to create
     * @param rse - the rse device to be added to the sim
     * @param obu - the obu device to be added to the sim
     * @param cell - the cell device to be added to the sim
     * @param fixedCell - the fixed cell device to be added to the sim
     */
    public static void createTemplateSimulationWithAllDevices(TemplateSimulation sim, RSEDevice rse, OBUDevice obu, CellularDevice cell, FixedCellularDevice fixedCell) {
        ETexasUser user = sim.getUser();
        //Login and navigate to simulations page
        LandingPage landing = goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        simPage.waitUntilLoaded();
        //Click New, Template, enter details
        simPage.clickCreate();
        CreateSimulationFromTemplateModal templateForm = simPage.clickTemplate();
        templateForm.setAllFields(sim);
        templateForm.clickCreate(true);
        simPage.checkSimulationDetails(sim);
        //Create RSE device
        simPage.clickEdit();
        SimulationSettingsModal simSettingsModal = simPage.clickSimulationSettings();
        ConfigureRSEDevicesPartialPage rseConfigPage = simSettingsModal.clickRSEDevicesTab();
        CreateRSEDeviceModal rseForm = rseConfigPage.clickCreate();
        rseForm.setAllFields(rse);
        rseForm.clickCreate(true);
        rse.setID(rseConfigPage.getRSEID(rse));
        Assert.assertTrue("RSE device with name " + rse.getName() + " is not displayed after being created.", rseConfigPage.isRSEDisplayed(rse));
        //Create OBU device
        simPage.clickEdit();
        CompositeSettingsModal compositeSettingsModal = simPage.clickCompositeSettings();
        ConfigureOBUDeviceProfilesPartialPage obuConfigPage = compositeSettingsModal.clickOBUTab();
        CreateOBUDeviceProfileModal obuModal = obuConfigPage.clickCreate();
        obuModal.setAllFields(obu);
        obuModal.clickCreate(true);
        Assert.assertTrue("OBU device with name " + obu.getName() + " is not displayed after being created.", obuConfigPage.isOBUDisplayed(obu));
        //Create Cellular device
        ConfigureCellularDevicePartialPage cellConfigPage = compositeSettingsModal.clickCellularTab();
        CreateCellularDeviceModal cellForm = cellConfigPage.clickCreateCellularDeviceBtn();
        cellForm.setAllFields(cell);
        cellForm.clickCreate(true);
        //Assert.assertTrue("Cellular device with name " + cell.getName() + " is not displayed after being created.", cellConfigPage.isDeviceDisplayed(cell.getName())); TODO -update for version 3.0
        cellConfigPage.clickCloseBtn();
        //Create Fixed Cellular device
        simPage.clickEdit();
        simSettingsModal = simPage.clickSimulationSettings();
        ConfigureFixedCellularDevicePartialPage fixedCellConfigPage = simSettingsModal.clickFixedCellularDevicesTab();
        CreateFixedCellularDeviceModal fixedCellForm = fixedCellConfigPage.clickCreateFixedCellularDeviceBtn();
        fixedCellForm.setAllFields(fixedCell);
        fixedCellForm.clickCreate();
        //Assert.assertTrue("Fixed cellular device with name " + fixedCell.getName() + " is not displayed after being created.",
        //        fixedCellConfigPage.isDeviceDisplayed(Integer.toString(fixedCell.getXCoordinate()))); TODO -update for version 3.0
        cellConfigPage.clickCloseBtn();
        //Logout
        simPage.logout(user);
    }

}
