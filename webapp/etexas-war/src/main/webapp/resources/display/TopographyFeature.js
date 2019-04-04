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
 * @class TopographyFeature
 * 
 * This class will handle the creation of a topography feature.
 * 
 * @author ttevendale
 * @param {Object} featureData The topography feature data that will be used to make the feature.
 */
THREEDISPLAY.TopographyFeature = function(featureData) {

    /**
     * @property {THREE.Group} mesh The mesh of the topography feature.
     */
    this.mesh = _createTopographyFeatureMesh(featureData);

    /**
     * @property {number} id The id of the topography feature.
     */
    this.id = featureData.id;

    /**
     * @property {string} name The name of the topography feature.
     */
    this.name = featureData.name;

    /**
     * @property {string} type The type of the topography feature.
     */
    this.type = featureData.type;

    /**
     * @method animate
     * 
     * Handles the animations associated with a topography feature.
     * 
     * @public
     * @param {THREE.Camera} camera The camera for the scene.
     */
    this.animate = function(camera) {};

    /**
     * @method _createTopographyFeatureMesh
     * 
     * General function for creating a topography feature mesh.
     * 
     * @private
     * @param {Object} featureData The topography feature data that will be used to make the
     * feature.
     * @return {THREE.Group} The created feature mesh.
     */
    function _createTopographyFeatureMesh(featureData) {

        var mesh = {};

        if (featureData.type === 'BUILDING') {

            mesh = _createBuildingMesh(featureData);
        }
        else {

            return;
        }

        mesh.userData = {

            type : 'Topography Feature',
            id : featureData.id
        };

        return mesh;

        /**
         * @method _createBuildingMesh
         * 
         * Creates the building mesh to be added to the scene.
         * 
         * @private
         * @param {Object} buildingData The building data which will be used to make the building
         * mesh.
         * @return {THREE.Group} The building mesh.
         */
        function _createBuildingMesh(buildingData) {

            var geomBox = new THREE.BoxGeometry(buildingData.width, buildingData.length, buildingData.height);

            var matBuilding = new THREE.MeshBasicMaterial({
                color : new THREE.Color(0xb8e0e6)
            });

            var meshBuilding = new THREE.Mesh(geomBox, matBuilding);

            var groupedMesh = new THREE.Group();
            groupedMesh.add(meshBuilding);

            // this only works for flat terrain (no terrain)
            groupedMesh.position.set(buildingData.x, buildingData.y, buildingData.height * 0.5);

            return groupedMesh;
        }
    }
};
