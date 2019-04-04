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
 * @class CellTower
 * 
 * This class will handle the creation of an individual cell tower.
 * 
 * @author ttevendale
 * @param {Object} cellTowerData The cell tower data that will be used to make the cell tower.
 */
THREEDISPLAY.CellTower = function(cellTowerData) {

    /**
     * @property {THREE.Group} mesh The mesh of the cell tower.
     */
    this.mesh = _createCellTowerMesh(cellTowerData);

    /**
     * @property {Number} id The ID of the cell tower.
     */
    this.id = cellTowerData.id;

    /**
     * @property {String} provider The Provider of the cell tower (AT&T, Verizon, etc...).
     */
    this.provider = cellTowerData.provider;

    /**
     * @method animate
     * 
     * Handles the animations associated with a cell tower.
     * 
     * @public
     * @param {THREE.Camera} camera The camera for the scene.
     */
    this.animate = function(camera) {};

    /**
     * @method _createCellTowerMesh
     * 
     * Creates the cell tower mesh to be added to the scene.
     * 
     * @private
     * @param {Object} cellTowerData The cell tower data which will be used to make the cell tower.
     * mesh
     * @return {THREE.Group} The cell tower mesh.
     */
    function _createCellTowerMesh(cellTowerData) {

        var tempModel = THREEDISPLAY.Static.Models.getCellTower();

        tempModel.position.set(cellTowerData.x, cellTowerData.y, cellTowerData.z);

        tempModel.userData = {

            objectType : 'Model',
            type : 'Cell Tower',
            id : cellTowerData.id
        };

        return tempModel;
    }
};
