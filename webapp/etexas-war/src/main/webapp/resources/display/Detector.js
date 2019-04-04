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
 * @class Detector
 * 
 * This class will handle the creation of a detector.
 * 
 * @author ttevendale
 * @param {Object} detectorData The data that will be used to make the detector.
 */
THREEDISPLAY.Detector = function(detectorData) {

    /**
     * @property {Number} id The ID of the detector.
     */
    this.id = detectorData.detectorID;

    /**
     * @property {THREE.Group} mesh The detector mesh that will be used in Three.js.
     */
    this.mesh = _createDetectorMesh(detectorData);

    /**
     * @property {Number[]} laneIds The lane IDs that the detector is on.
     */
    this.laneIds = detectorData.laneIDs;

    /**
     * @property {Object} area The area of the detector
     */
    this.area = detectorData.area;

    /**
     * @property {Boolean} lengthDetectCap A boolean for if a detector has a length detection event
     * capability.
     */
    this.lengthDetectCap = detectorData.lengthDetectCap;

    /**
     * @property {Boolean} presenceDetectCap A boolean for if a detector has a presence detection
     * event capability.
     */
    this.presenceDetectCap = detectorData.presenceDetectCap;

    /**
     * @property {Boolean} pulseDetectCap A boolean for if a detector has a pulse detection event
     * capability.
     */
    this.pulseDetectCap = detectorData.pulseDetectCap;

    /**
     * @property {Boolean} speedDetectCap A boolean for if a detector has a speed detection event
     * capability.
     */
    this.speedDetectCap = detectorData.speedDetectCap;

    /**
     * @method animate
     * 
     * Handles the animations associated with a detector.
     * 
     * @public
     * @param {THREE.Camera} camera The camera for the scene.
     */
    this.animate = function(camera) {};

    /**
     * @method _createDetectorMesh
     * 
     * Creates the detector mesh to be added to the scene.
     * 
     * @private
     * @param {Object} detectorData The detector data which will be used to make the detector mesh.
     * @return {THREE.Group} The detector mesh.
     */
    function _createDetectorMesh(detectorData) {

        if (detectorData.area.n === 0) {

            return;
        }

        var detectorShape = _createDetectorShape(detectorData.area);

        var detectorGeom = new THREE.ExtrudeGeometry(detectorShape, {

            curveSegments : 3,
            steps : 3,
            amount : 60,
            bevenlEnabled : true,
            bevelThickness : 10,
            bevelSize : 3
        });

        detectorGeom.computeBoundingBox();

        var detectorCenter = new THREE.Vector3();
        detectorCenter.addVectors(detectorGeom.boundingBox.min, detectorGeom.boundingBox.max);
        detectorCenter.divideScalar(2);

        detectorGeom.center();

        var matDetector = new THREE.MeshBasicMaterial({

            color : new THREE.Color(0xD8D0D1),
            side : THREE.FrontSide
        });

        var meshDetector = new THREE.Mesh(detectorGeom, matDetector);

        var groupedMesh = new THREE.Group();
        groupedMesh.add(meshDetector);
        groupedMesh.position.set(detectorCenter.x, detectorCenter.y, detectorCenter.z);

        groupedMesh.userData = {

            type : 'Detector',
            id : detectorData.detectorID
        };

        return groupedMesh;
    }

    /**
     * @method _createDetectorShape
     * 
     * Creates the shape object from the area of the detector.
     * 
     * @private
     * @param {Object} area The detector area which will be used to make the shape.
     * @return {THREE.Shape} The detector shape.
     */
    function _createDetectorShape(area) {

        var detectorShape = new THREE.Shape();

        for (var i = 0; i < area.n; i++) {

            if (i === 0) {

                detectorShape.moveTo(area.x[0], area.y[0]);
            }
            else {

                detectorShape.lineTo(area.x[i], area.y[i]);
            }

            if (i + 1 - area.n === 0) {

                detectorShape.lineTo(area.x[0], area.y[0]);
            }
        }

        return detectorShape;
    }
};
