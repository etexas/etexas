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
 * @class Vehicle
 * 
 * This class will handle the creation of an individual vehicle.
 * 
 * @private
 * @param {Object} vehicleData The vehicle data that will be used to make the vehicle.
 */
THREEDISPLAY.Vehicle = function(vehicleData) {

    /**
     * @private
     * @property {THREEDISPLAY.Models.Vehicles.Vehicle} vehicleModel The vehicle model.
     */
    var vehicleModel = _createVehicleMesh(vehicleData);

    /**
     * @property {THREE.Group} mesh The mesh of the vehicle.
     */
    this.mesh = vehicleModel.mesh;

    /**
     * @property {Number} id the ID of the vehicle.
     */
    this.id = vehicleData.vehicleID;

    /**
     * @property {Object[]} devices The array of devices attached to this vehicle.
     */
    this.devices = vehicleData.devices || [];

    /**
     * @property {Number} heading The vehicle heading.
     */
    this.heading = vehicleData.heading;

    /**
     * @method animate
     * 
     * Handles the animations associated with a vehicle.
     * 
     * @public
     * @param {THREE.Camera} camera The camera for the scene.
     */
    this.animate = function(camera) {};

    /**
     * @method update
     * 
     * Updates this vehicle.
     * 
     * @public
     * @param {Object} vehicleData The vehicle data to update this vehicle with.
     */
    this.update = function(vehicleData) {

        vehicleModel.update(vehicleData, this.heading);
        this.heading = vehicleData.heading;
    };

    /**
     * @method _createVehicleMesh
     * 
     * General method for creating a vehicle mesh.
     * 
     * @private
     * @param {Object} vehicleData The vehicle data to make the mesh out of.
     * @return {THREE.Group} The created vehicle mesh.
     */
    function _createVehicleMesh(vehicleData) {

        var tempModel;
        var type = vehicleData.type;
        var objectType;

        if (type === 'CAR') {

            objectType = 'Model';
            tempModel = THREEDISPLAY.Static.Models.getCar();
        }
        else if (type === 'BUS') {

            objectType = 'Model';
            tempModel = THREEDISPLAY.Static.Models.getBus();
        }
        else if (type === 'TRACTOR_TRAILER') {

            objectType = 'Model';
            tempModel = THREEDISPLAY.Static.Models.getSemiTruck();
        }
        else {

            objectType = 'ThreeJS';
            tempModel = THREEDISPLAY.Static.Models.getBasicVehicle();
        }

        tempModel.update(vehicleData);

        tempModel.mesh.userData = {

            objectType : objectType,
            type : 'Vehicle',
            id : vehicleData.vehicleID
        };

        return tempModel;
    }
};
