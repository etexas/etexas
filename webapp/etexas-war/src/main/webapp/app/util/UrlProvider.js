/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
/**
 * @class ETexas.util.UrlProvider
 * @singleton
 * 
 * Provides global utility functions to retrieve URLs for REST service operations.
 * 
 * @author emyers
 * @author ttevendale
 */
Ext.define('ETexas.util.UrlProvider', {
    singleton : true,

    config : {

        /** @cfg {String} apiUrl The prefix for REST API service URLs. */
        apiUrl : '/rest/api',

        /** @cfg {String} restUrl The prefix for REST service URLs. */
        restUrl : '/rest'
    },

    /**
     * @constructor
     * 
     * Creates a new UrlProvider instance.
     * 
     * @param {Object} config The instance configuration.
     */
    constructor : function(config) {

        this.initConfig(config);
    },

    /**
     * @method getApplicationProfilesUrl
     * 
     * Returns the URL for application profile REST services.
     * 
     * @public
     * @param {ETexas.model.ApplicationProfileModel} [applicationProfile] The target application
     * profile.
     * @return {String} The URL for application profile REST services.
     */
    getApplicationProfilesUrl : function(applicationProfile) {

        var url = this.getApiUrl() + '/applicationprofiles';

        if (applicationProfile) {

            url = url.concat('/' + applicationProfile.get('id'));
        }

        return url;
    },

    /**
     * @method getBlogUrl
     * 
     * Returns the URL for blog REST services.
     * 
     * @public
     * @return {String} The URL for blog REST services.
     */
    getBlogUrl : function() {

        return this.getRestUrl() + '/info/blogrss';
    },

    /**
     * @method getBuildingsUrl
     * 
     * Returns the URL for building REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.TopographyFeatureModel} [feature] The target building.
     * @return {String} The URL for building REST services.
     */
    getBuildingsUrl : function(composite, feature) {

        var url = this.getTopographyFeaturesUrl(composite) + '/buildings';

        if (feature) {

            url = url.concat('/' + feature.get('id'));
        }

        return url;
    },

    /**
     * @method getCellTowersUrl
     * 
     * Returns the URL for cell tower REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.CellTowerModel} [cellTower] The target cell tower.
     * @return {String} The URL for cell tower REST services.
     */
    getCellTowersUrl : function(composite, cellTower) {

        var url = this.getCompositesUrl(composite) + '/celltowers';

        if (cellTower) {

            url = url.concat('/' + cellTower.get('id'));
        }

        return url;
    },

    /**
     * @method getCellularConfigurationsUrl
     * 
     * Returns the URL for cellular configurations REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @return {String} The URL for cellular configurations REST services.
     */
    getCellularConfigurationsUrl : function(composite) {

        return this.getCompositesUrl(composite) + '/cellularconfigurations';
    },

    /**
     * @method getCellularDeviceProfilesUrl
     * 
     * Returns the URL for cellular device profile REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.DeviceProfileModel} [deviceProfile] The target device profile.
     * @return {String} The URL for cellular device profile REST services.
     */
    getCellularDeviceProfilesUrl : function(composite, deviceProfile) {

        var url = this.getDeviceProfilesUrl(composite) + '/cellulardeviceprofiles';

        if (deviceProfile) {

            url = url.concat('/' + deviceProfile.get('id'));
        }

        return url;
    },

    /**
     * @method getCommandsUrl
     * 
     * Returns the URL for command REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.ExecutionModel} execution The target execution.
     * @return {String} The URL for command REST services.
     */
    getCommandsUrl : function(composite, execution) {

        return this.getExecutionsUrl(composite, execution) + '/commands';
    },

    /**
     * @method getCompositeFilesUrl
     * 
     * Returns the URL for composite files REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @return {String} The URL for composite files REST services.
     */
    getCompositeFilesUrl : function(composite) {

        return this.getRestUrl() + '/files/composites/' + composite.get('id');
    },

    /**
     * @method getCompositeOptionsUrl
     * 
     * Returns the URL for composite options REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @return {String} The URL for composite options REST services.
     */
    getCompositeOptionsUrl : function(composite) {

        return this.getCompositesUrl(composite) + '/options';
    },

    /**
     * @method getCompositesUrl
     * 
     * Returns the URL for composite REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} [composite] The target composite.
     * @return {String} The URL for composite REST services.
     */
    getCompositesUrl : function(composite) {

        var url = this.getApiUrl() + '/composites';

        if (composite) {

            url = url.concat('/' + composite.get('id'));
        }

        return url;
    },

    /**
     * @method getCurrentUserUrl
     * 
     * Returns the URL for current user REST services.
     * 
     * @public
     * @return {String} The URL for current user REST services.
     */
    getCurrentUserUrl : function() {

        return this.getApiUrl() + '/currentuser';
    },

    /**
     * @method getCurrentUserPasswordUrl
     * 
     * Returns the URL for current user password REST services.
     * 
     * @public
     * @return {String} The URL for current user password REST services.
     */
    getCurrentUserPasswordUrl : function() {

        return this.getCurrentUserUrl() + '/password';
    },

    /**
     * @method getDeployedDetectorsUrl
     * 
     * Returns the URL for deployed detector REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.ExecutionModel} execution The target execution.
     * @return {String} The URL for deployed detector REST services.
     */
    getDeployedDetectorsUrl : function(composite, execution) {

        return this.getExecutionsUrl(composite, execution) + '/detectors';
    },

    /**
     * @method getDetectorsUrl
     * 
     * Returns the URL for detector REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.SimulationModel} simulation The target simulation.
     * @param {ETexas.model.DetectorModel} [detector] The target detector.
     * @return {String} The URL for detector REST services.
     */
    getDetectorsUrl : function(composite, simulation, detector) {

        var url = this.getSimulationsUrl(composite, simulation) + '/detectors';

        if (detector) {

            url = url.concat('/' + detector.get('id'));
        }

        return url;
    },

    /**
     * @method getDeviceProfileApplicationsUrl
     * 
     * Returns the URL for device profile application REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.DeviceProfileModel} deviceProfile The target device profile.
     * @param {ETexas.model.ApplicationModel} [application] The target application.
     * @return {String} The URL for device profile application REST services.
     */
    getDeviceProfileApplicationsUrl : function(composite, deviceProfile, application) {

        var url = this.getDeviceProfilesUrl(composite, deviceProfile) + '/applications';

        if (application) {

            url = url.concat('/' + application.get('id'));
        }

        return url;
    },

    /**
     * @method getDeviceProfilesUrl
     * 
     * Returns the URL for device profile REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.DeviceProfileModel} [deviceProfile] The target device profile.
     * @return {String} The URL for device profile REST services.
     */
    getDeviceProfilesUrl : function(composite, deviceProfile) {

        var url = this.getCompositesUrl(composite) + '/deviceprofiles';

        if (deviceProfile) {

            url = url.concat('/' + deviceProfile.get('id'));
        }

        return url;
    },

    /**
     * @method getDeviceApplicationsUrl
     * 
     * Returns the URL for device application REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.DeviceModel} device The target device.
     * @param {ETexas.model.ApplicationModel} [application] The target application.
     * @return {String} The URL for device application REST services.
     */
    getDeviceApplicationsUrl : function(composite, device, application) {

        var url = this.getDevicesUrl(composite, device) + '/applications';

        if (application) {

            url = url.concat('/' + application.get('id'));
        }

        return url;
    },

    /**
     * @method getDevicesUrl
     * 
     * Returns the URL for device REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.DeviceModel} [device] The target device.
     * @return {String} The URL for device REST services.
     */
    getDevicesUrl : function(composite, device) {

        var url = this.getCompositesUrl(composite) + '/devices';

        if (device) {

            var id = device.get('id') || device.get('deviceRuleId');
            url = url.concat('/' + id);
        }

        return url;
    },

    /**
     * @method getEmailVerificationUrl
     * 
     * Returns the URL for email verification REST services.
     * 
     * @public
     * @return {String} The URL for email verification REST services.
     */
    getEmailVerificationUrl : function() {

        return this.getUsersUrl() + '/verify';
    },

    /**
     * @method getExecutionsUrl
     * 
     * Returns the URL for execution REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.ExecutionModel} [execution] The target execution.
     * @return {String} The URL for execution REST services.
     */
    getExecutionsUrl : function(composite, execution) {

        var url = this.getCompositesUrl(composite) + '/executions';

        if (execution) {

            url = url.concat('/' + execution.get('id'));
        }

        return url;
    },

    /**
     * @method getExecutionMessagesUrl
     * 
     * Returns the URL for execution messages REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.ExecutionModel} execution The target execution.
     * @return {String} The URL for execution messages REST services.
     */
    getExecutionMessagesUrl : function(composite, execution) {

        return this.getExecutionsUrl(composite, execution) + '/executionmessages';
    },

    /**
     * @method getFixedCellularDevicesUrl
     * 
     * Returns the URL for fixed cellular device REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.DeviceModel} [device] The target device.
     * @return {String} The URL for fixed cellular device REST services.
     */
    getFixedCellularDevicesUrl : function(composite, device) {

        var url = this.getDevicesUrl(composite) + '/fixedcellulardevices';

        if (device) {

            url = url.concat('/' + device.get('id'));
        }

        return url;
    },

    /**
     * @method getHelpContentUrl
     * 
     * Returns the URL for help content pages.
     * 
     * @public
     * @return {String} The URL for help content pages.
     */
    getHelpContentUrl : function() {

        return '/help/Content';
    },

    /**
     * @method getInjectionCommandsUrl
     * 
     * Returns the URL for injection command REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.ExecutionModel} execution The target execution.
     * @return {String} The URL for injection command REST services.
     */
    getInjectionCommandsUrl : function(composite, execution) {

        return this.getCommandsUrl(composite, execution) + '/injectioncommands';
    },

    /**
     * @method getJarApplicationProfilesUrl
     * 
     * Returns the URL for JAR application profile REST services.
     * 
     * @public
     * @param {ETexas.model.ApplicationProfileModel} applicationProfile The target JAR application
     * profile.
     * @return {String} The URL for JAR application profile REST services.
     */
    getJarApplicationProfilesUrl : function(applicationProfile) {

        var url = this.getApplicationProfilesUrl() + '/jarapplicationprofiles';

        if (applicationProfile) {

            url = url.concat('/' + applicationProfile.get('id'));
        }

        return url;
    },

    /**
     * @method getLaneCommandsUrl
     * 
     * Returns the URL for lane command REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.ExecutionModel} execution The target execution.
     * @return {String} The URL for lane command REST services.
     */
    getLaneCommandsUrl : function(composite, execution) {

        return this.getCommandsUrl(composite, execution) + '/lanecommands';
    },

    /**
     * @method getLaneMappingsUrl
     * 
     * Returns the URL for lane mapping REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.LaneMappingModel} [laneMapping] The target lane mapping.
     * @return {String} The URL for lane mapping REST services.
     */
    getLaneMappingsUrl : function(composite, laneMapping) {

        var url = this.getCompositesUrl(composite) + '/lanemappings';

        if (laneMapping) {

            url = url.concat('/' + laneMapping.get('id'));
        }

        return url;
    },

    /**
     * @method getLanesUrl
     * 
     * Returns the URL for lane REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.SimulationModel} [simulation] The target simulation.
     * @return {String} The URL for lane REST services.
     */
    getLanesUrl : function(composite, simulation) {

        return (simulation ? this.getSimulationsUrl(composite, simulation) : this.getCompositesUrl(composite)) + '/lanes';
    },

    /**
     * @method getLoginUrl
     * 
     * Returns the URL for REST login services.
     * 
     * @public
     * @return {String} The URL for REST login services.
     */
    getLoginUrl : function() {

        return this.getUsersUrl() + '/login';
    },

    /**
     * @method getLogoutUrl
     * 
     * Returns the URL for REST logout services.
     * 
     * @public
     * @return {String} The URL for REST logout services.
     */
    getLogoutUrl : function() {

        return this.getCurrentUserUrl() + '/logout';
    },

    /**
     * @method getLogApplicationsUrl
     * 
     * Returns the URL for REST log application services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.ExecutionModel} execution The target execution.
     * @return {String} The URL for REST log application services.
     */
    getLogApplicationsUrl : function(composite, execution) {

        return this.getLogsUrl(composite, execution) + '/applications';
    },

    /**
     * @method getLogDevicesUrl
     * 
     * Returns the URL for REST log device services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.ExecutionModel} execution The target execution.
     * @return {String} The URL for REST log device services.
     */
    getLogDevicesUrl : function(composite, execution) {

        return this.getLogsUrl(composite, execution) + '/devices';
    },

    /**
     * @method getLogKeysUrl
     * 
     * Returns the URL for REST log key services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.ExecutionModel} execution The target execution.
     * @return {String} The URL for REST log key services.
     */
    getLogKeysUrl : function(composite, execution) {

        return this.getLogsUrl(composite, execution) + '/keys';
    },

    /**
     * @method getLogsUrl
     * 
     * Returns the URL for REST log services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.ExecutionModel} execution The target execution.
     * @param {ETexas.model.LogModel} [log] The target log.
     * @return {String} The URL for REST log services.
     */
    getLogsUrl : function(composite, execution, log) {

        var url = this.getExecutionsUrl(composite, execution) + '/logs';

        if (log) {

            url = url.concat('/' + log.get('id'));
        }

        return url;
    },

    /**
     * @method getMessagesUrl
     * 
     * Returns the URL for messages services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.ExecutionModel} execution The target execution.
     * @return {String} The URL for the message REST services.
     */
    getMessagesUrl : function(composite, execution) {

        return this.getExecutionsUrl(composite, execution) + '/messages';
    },

    /**
     * @method getMessagesByDeviceUrl
     * 
     * Returns the URL for messages by device services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.ExecutionModel} execution The target execution.
     * @param {Number} deviceMac The MAC address of the target device.
     * @return {String} The URL for the message REST services.
     */
    getMessagesByDeviceUrl : function(composite, execution, deviceMac) {

        return this.getExecutionsUrl(composite, execution) + '/devices/' + deviceMac + '/messages';
    },

    /**
     * @method getNativeApplicationProfilesUrl
     * 
     * Returns the URL for native application profile REST services.
     * 
     * @public
     * @param {ETexas.model.ApplicationProfileModel} applicationProfile The target native
     * application profile.
     * @return {String} The URL for native application profile REST services.
     */
    getNativeApplicationProfilesUrl : function(applicationProfile) {

        var url = this.getApplicationProfilesUrl() + '/nativeapplicationprofiles';

        if (applicationProfile) {

            url = url.concat('/' + applicationProfile.get('id'));
        }

        return url;
    },

    /**
     * @method getObuDeviceProfilesUrl
     * 
     * Returns the URL for OBU device profile REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.DeviceProfileModel} [deviceProfile] The target device profile.
     * @return {String} The URL for OBU device profile REST services.
     */
    getObuDeviceProfilesUrl : function(composite, deviceProfile) {

        var url = this.getDeviceProfilesUrl(composite) + '/obudeviceprofiles';

        if (deviceProfile) {

            url = url.concat('/' + deviceProfile.get('id'));
        }

        return url;
    },

    /**
     * @method getOnBoardDevicesUrl
     * 
     * Returns the URL for on board device REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.ExecutionModel} execution The target execution.
     * @param {ETexas.model.VehicleModel} vehicle The target vehicle.
     * @return {String} The URL for on board device REST services.
     */
    getOnBoardDevicesUrl : function(composite, execution, vehicle) {

        return this.getVehiclesUrl(composite, execution, vehicle) + '/devices';
    },

    /**
     * @method getRecoveryUrl
     * 
     * Returns the URL for username/password recovery REST services.
     * 
     * @public
     * @return {String} The URL for username/password recovery REST services.
     */
    getRecoveryUrl : function() {

        return this.getUsersUrl() + '/recovery';
    },

    /**
     * @method getRemoteApplicationProfilesUrl
     * 
     * Returns the URL for remote application profile REST services.
     * 
     * @public
     * @param {ETexas.model.ApplicationProfileModel} applicationProfile The target application
     * profile.
     * @return {String} The URL for remote application profile REST services.
     */
    getRemoteApplicationProfilesUrl : function(applicationProfile) {

        var url = this.getApplicationProfilesUrl() + '/remoteapplicationprofiles';

        if (applicationProfile) {

            url = url.concat('/' + applicationProfile.get('id'));
        }

        return url;
    },

    /**
     * @method getReportApplicationsUrl
     * 
     * Returns the URL for report application REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.ApplicationModel} [application] The target application.
     * @return {String} The URL for report application REST services.
     */
    getReportApplicationsUrl : function(composite, application) {

        var url = this.getCompositesUrl(composite) + '/reportapplications';

        if (application) {

            url = url.concat('/' + application.get('id'));
        }

        return url;
    },

    /**
     * @method getRseDevicesUrl
     * 
     * Returns the URL for RSE device REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.DeviceModel} [device] The target device.
     * @return {String} The URL for RSE device REST services.
     */
    getRseDevicesUrl : function(composite, device) {

        var url = this.getDevicesUrl(composite) + '/rsedevices';

        if (device) {

            url = url.concat('/' + device.get('id'));
        }

        return url;
    },

    /**
     * @method getSignalCommandsUrl
     * 
     * Returns the URL for signal command REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.ExecutionModel} execution The target execution.
     * @return {String} The URL for signal command REST services.
     */
    getSignalCommandsUrl : function(composite, execution) {

        return this.getCommandsUrl(composite, execution) + '/signalcommands';
    },

    /**
     * @method getSignalsUrl
     * 
     * Returns the URL for signal REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.ExecutionModel} execution The target execution.
     * @return {String} The URL for signal REST services.
     */
    getSignalsUrl : function(composite, execution) {

        return this.getExecutionsUrl(composite, execution) + '/signals';
    },

    /**
     * @method getSimulationFilesUrl
     * 
     * Returns the URL for simulation files REST services.
     * 
     * @public
     * @param {ETexas.model.SimulationModel} simulation The target simulation.
     * @return {String} The URL for simulation files REST services.
     */
    getSimulationFilesUrl : function(simulation) {

        return this.getRestUrl() + '/files/simulations/' + simulation.get('id');
    },

    /**
     * @method getSimulationsUrl
     * 
     * Returns the URL for simulation REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.SimulationModel} [simulation] The target simulation.
     * @return {String} The URL for simulation REST services.
     */
    getSimulationsUrl : function(composite, simulation) {

        var url = this.getCompositesUrl(composite) + '/simulations';

        if (simulation) {

            url = url.concat('/' + simulation.get('id'));
        }

        return url;
    },

    /**
     * @method getSpeedCommandsUrl
     * 
     * Returns the URL for speed command REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.ExecutionModel} execution The target execution.
     * @return {String} The URL for speed command REST services.
     */
    getSpeedCommandsUrl : function(composite, execution) {

        return this.getCommandsUrl(composite, execution) + '/speedcommands';
    },

    /**
     * @method getStandaloneDevicesUrl
     * 
     * Returns the URL for standalone device REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.ExecutionModel} execution The target execution.
     * @return {String} The URL for standalone device REST services.
     */
    getStandaloneDevicesUrl : function(composite, execution) {

        return this.getExecutionsUrl(composite, execution) + '/standalonedevices';
    },

    /**
     * @method getTopographyFeaturesUrl
     * 
     * Returns the URL for topography feature REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.TopographyFeatureModel} [feature] The target topography feature.
     * @return {String} The URL for topography feature REST services.
     */
    getTopographyFeaturesUrl : function(composite, feature) {

        var url = this.getCompositesUrl(composite) + '/topography';

        if (feature) {

            url = url.concat('/' + feature.get('id'));
        }

        return url;
    },

    /**
     * @method getUploadsUrl
     * 
     * Returns the URL for file upload services.
     * 
     * @public
     * @return {String} The URL for file upload services.
     */
    getUploadsUrl : function() {

        return this.getApiUrl() + '/upload';
    },

    /**
     * @method getUsersUrl
     * 
     * Returns the URL for user REST services.
     * 
     * @public
     * @return {String} The URL for user REST services.
     */
    getUsersUrl : function() {

        return this.getRestUrl() + '/users';
    },

    /**
     * @method getVehiclesUrl
     * 
     * Returns the URL for vehicle REST services.
     * 
     * @public
     * @param {ETexas.model.CompositeModel} composite The target composite.
     * @param {ETexas.model.ExecutionModel} execution The target execution.
     * @param {ETexas.model.VehicleModel} [vehicle] The target vehicle.
     * @return {String} The URL for vehicle REST services.
     */
    getVehiclesUrl : function(composite, execution, vehicle) {

        var url = this.getExecutionsUrl(composite, execution) + '/vehicles';

        if (vehicle) {

            url = url.concat('/' + vehicle.get('vehicleID'));
        }

        return url;
    },

    /**
     * @method getVersionUrl
     * 
     * Returns the URL for application version information.
     * 
     * @public
     * @return {String} The URL for application version information.
     */
    getVersionUrl : function() {

        return this.getRestUrl() + '/info/version';
    },

    /**
     * @method getWebStartUrl
     * 
     * Returns the URL for Web Start services.
     * 
     * @public
     * @return {String} The URL for Web Start services.
     */
    getWebStartUrl : function() {

        return this.getRestUrl() + '/info/webstart';
    }
});
