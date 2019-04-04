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
 * @class Lane
 * 
 * This class will handle an individual lane.
 * 
 * @author ttevendale
 * @param {Object} laneData The lane data which will be used to build the lane.
 */
THREEDISPLAY.Lane = function(laneData) {

    /**
     * @property {Number} id The ID of this lane.
     */
    this.id = THREEDISPLAY.Lane.getProperId(laneData);

    /**
     * @property {THREEDISPLAY.Signal} The signal attached to the lane.
     */
    this.signal = null;

    /**
     * @property {Object[]} laneNodes The lane nodes that make up the lane.
     */
    this.laneNodes = _getLaneNodes(laneData);

    /**
     * @property {THREE.Group} mesh The mesh of this lane.
     */
    this.mesh = _createLaneMesh(laneData.simulationName, laneData.laneId, this.laneNodes);

    /**
     * @method animate
     * 
     * Handles the animations associated with this lane.
     * 
     * @public
     * @param {THREE.Camera} camera The camera for the scene.
     */
    this.animate = function(camera) {

        if (this.signal) {

            this.signal.animate(camera);
        }
    };

    /**
     * @method addSignal
     * 
     * Adds the signal to this lane.
     * 
     * @public
     * @param {Object[]} signalData The signal data which will be added to this lane.
     */
    this.addSignal = function(signalData) {

        if (this.signal) {

            this.signal.update(signalData);
        }
        else {

            var signal = new THREEDISPLAY.Signal(signalData, this.laneNodes[this.laneNodes.length - 1]);

            if (signal.mesh) {

                signal.mesh.visible = THREEDISPLAY.Lane.signalIsVisible;
                this.mesh.parent.add(signal.mesh);
                this.signal = signal;
            }
        }
    };

    /**
     * @method clearSignal
     * 
     * Clears the signal from the lane.
     * 
     * @public
     */
    this.clearSignal = function() {

        if (this.signal) {

            THREEDISPLAY.Utils.dispose(this.signal.mesh);
            this.signal = null;
        }
    };

    /**
     * @method hideSignal
     * 
     * Hides the signal attached to this lane.
     * 
     * @public
     */
    this.hideSignal = function() {

        if (this.signal) {

            this.signal.mesh.visible = false;
        }
    };

    /**
     * @method showSignal
     * 
     * shows the signal attached to this lane.
     * 
     * @public
     */
    this.showSignal = function() {

        if (this.signal) {

            this.signal.mesh.visible = true;
        }
    };

    /**
     * @method _getLaneNodes
     * 
     * Gets the lane nodes.
     * 
     * @private
     * @param {Object} laneData The lane data which contains the lane nodes.
     * @return {THREE.Vector4[]} The lane nodes
     */
    function _getLaneNodes(laneData) {

        var laneNodes = [];
        var numLanes = laneData.laneGeomList.length;
        for (var i = 0; i < numLanes; i++) {

            var laneNode = laneData.laneGeomList[i];
            laneNodes.push(new THREE.Vector4(laneNode.x, laneNode.y, laneNode.z, laneNode.width));
        }

        return laneNodes;
    }

    /**
     * @method _createLaneMesh
     * 
     * Creates the lane mesh to be added to the scene.
     * 
     * @private
     * @param {String} simulationName The name of the simulation that this lane is attached to.
     * @param {Number} id The id of the lane being created.
     * @param {THREE.Vector4[]} laneNodes The lane nodes which will be used to make the lane mesh.
     * @return {THREE.Group} The lane mesh.
     */
    function _createLaneMesh(simulationName, id, laneNodes) {

        var mesh = new THREE.Group();

        var roadMaterial = new THREE.MeshBasicMaterial({
            map : THREEDISPLAY.Static.Textures.getBasicLane()
        });

        var numLaneNodes = laneNodes.length;
        for (var i = 0; i < numLaneNodes - 1; i++) {

            var laneNode1 = laneNodes[i];
            var laneNode2 = laneNodes[i + 1];

            var laneSeg = _buildLaneSection(laneNode1, laneNode2, roadMaterial);

            mesh.add(laneSeg);
        }

        mesh.userData = {

            type : 'Lane',
            simulationName : simulationName,
            id : id
        };

        return mesh;
    }

    /**
     * @method _buildLaneSection
     * 
     * Builds a section of the lane based on two lane nodes.
     * 
     * @private
     * @param {LaneNode} laneNode1 The first point to create the lane section from.
     * @param {LaneNode} laneNode2 The second point to create the lane section from.
     * @param {THREE.Material} roadMaterial The material that will be attached to the lane section.
     * @return {THREE.Mesh} The lane section that was built using the two points.
     */
    function _buildLaneSection(laneNode1, laneNode2, roadMaterial) {

        var location1 = new THREE.Vector3(laneNode1.x, laneNode1.y, laneNode1.z);
        var location2 = new THREE.Vector3(laneNode2.x, laneNode2.y, laneNode2.z);

        var laneCenter = new THREE.Vector3();
        laneCenter.lerpVectors(location1, location2, 0.5);

        var length = location1.distanceTo(location2);

        var laneSegGeom = new THREE.BoxGeometry(laneNode1.w, length, 1);
        laneSegGeom.rotateX(Math.PI / 2);
        var laneSeg = new THREE.Mesh(laneSegGeom, roadMaterial);

        laneSeg.position.set(laneCenter.x, laneCenter.y, laneCenter.z);
        laneSeg.lookAt(laneNode2);

        return laneSeg;
    }
};

/**
 * @static
 * @property signalIsVisible The visibility of the signals.
 */
THREEDISPLAY.Lane.signalIsVisible = true;

/**
 * @method getProperId
 * 
 * Gets the proper ID for the lane or signal.
 * 
 * @static
 * @param {Object} data The data to get the ID from.
 * @return {String} The ID of the data.
 */
THREEDISPLAY.Lane.getProperId = function(data) {

    return data.simulationName + '-' + data.laneId;
};
