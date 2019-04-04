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
 * Creates the THREEDISPLAY name space if it doesn't exist yet.
 */
var THREEDISPLAY = THREEDISPLAY || {};

/**
 * @class StandaloneDevice
 * 
 * This class will handle the creation of an individual device.
 * 
 * @author ttevendale
 * @param {Object} deviceData The device data that will be used to make the device.
 */
THREEDISPLAY.StandaloneDevice = function(deviceData) {

    /**
     * @property {THREE.Group} mesh The mesh of the device.
     */
    this.mesh = _createDeviceMesh(deviceData);

    /**
     * @property {number} id The identifier for this device.
     */
    this.id = deviceData.deviceMac;

    /**
     * @property {number} macAddress The mac address of the device.
     */
    this.macAddress = deviceData.deviceMac;

    /**
     * @property {string} type The type of device (RSE, Fixed Cell).
     */
    this.type = deviceData.deviceType;

    /**
     * @method animate
     * 
     * Handles the animations associated with a stand-alone device.
     * 
     * @public
     * @param {THREE.Camera} camera The camera for the scene.
     */
    this.animate = function(camera) {};

    /**
     * @method _createDeviceMesh
     * 
     * General function for creating a device mesh.
     * 
     * @private
     * @param {Object} deviceData The device data that will be used to make the device.
     * @return {THREE.Group} The created device mesh.
     */
    function _createDeviceMesh(deviceData) {

        var mesh;
        var type = deviceData.deviceType;

        if (type === 'RSE') {

            mesh = THREEDISPLAY.Static.Models.getRSU();
        }
        else if (type === 'Fixed Cellular') {

            mesh = THREEDISPLAY.Static.Models.getCellPhone();
        }
        else {

            return;
        }

        mesh.position.set(deviceData.x, deviceData.y, deviceData.z);
        mesh.userData = {

            objectType : 'Model',
            type : 'Standalone Device',
            id : deviceData.deviceMac
        };

        return mesh;
    }
};
