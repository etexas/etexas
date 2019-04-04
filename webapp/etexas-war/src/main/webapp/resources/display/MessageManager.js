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
 * @class MessageManager
 * 
 * Manages the messages for the display.
 * 
 * @author ttevendale
 * @param {THREE.Scene} scene The scene for the display.
 */
THREEDISPLAY.MessageManager = function(scene) {

    /**
     * @property {THREE.Mesh[]} messagesMeshes The array of message meshes.
     */
    var messageMeshes = [];

    /**
     * @property {String} propagationLossModel The propagation loss model that the display will use.
     */
    var propagationLossModel = "Urban";

    /**
     * @method addBroadcastMessage
     * 
     * Adds a broadcast message to the mesh passed in.
     * 
     * @public
     * @param {THREE.Mesh} mesh The mesh to add the broadcast message to.
     * @param {String} channel The channel that the message is using.
     */
    this.addBroadcastMessage = function(mesh, channel) {

        var sphereSize = 0;
        if (propagationLossModel === "Urban") {

            // 150 meters max
            sphereSize = 15000;
        }
        else if (propagationLossModel === "Suburban") {

            // 175 meters max
            sphereSize = 17500;
        }
        else if (propagationLossModel === "Open") {

            // 2000 meters max
            sphereSize = 200000;
        }

        var geomSphere = new THREE.SphereGeometry(sphereSize, 32, 32);

        var matSphere = new THREE.MeshBasicMaterial({
            color : getMessageColor(channel),
            transparent : true,
            opacity : 0.5,
            side : THREE.DoubleSide
        });

        var broadcast = new THREE.Mesh(geomSphere, matSphere);

        mesh.add(broadcast);
        messageMeshes.push(broadcast);
    };

    /**
     * @method addUnicastMessage
     * 
     * Adds a unicast message to the scene based on the meshes passed in.
     * 
     * @public
     * @param {THREE.Mesh} txMesh The tx mesh where the start of the message is.
     * @param {THREE.Mesh} rxMesh The rx mesh where the end of the message is.
     * @param {String} channel The channel that the message is using.
     */
    this.addUnicastMessage = function(txMesh, rxMesh, channel) {

        var start = txMesh.position.clone();
        var end = rxMesh.position.clone();

        var mid = new THREE.Vector3();
        mid.lerpVectors(start, end, 0.5);
        mid.z += 1000; // elevation

        var curveQuad = new THREE.QuadraticBezierCurve3(start, mid, end);
        var curvePath = new THREE.CurvePath();

        curvePath.add(curveQuad);

        var curvedLineMaterial = new THREE.LineBasicMaterial({
            color : getMessageColor(channel)
        });

        var unicast = new THREE.Line(curvePath.createPointsGeometry(200), curvedLineMaterial);

        scene.add(unicast);
        messageMeshes.push(unicast);
    };

    /**
     * @method clear
     * 
     * Clears the messages from the display
     * 
     * @public
     */
    this.clear = function() {

        messageMeshes.forEach(function(message) {

            THREEDISPLAY.Utils.dispose(message);
        });
        messageMeshes = [];
    };

    /**
     * @method setPropagationLossModel
     * 
     * Sets the propagation loss model for the display.
     * 
     * @public
     * @param {String} model The propagation loss model to use in the display.
     */
    this.setPropagationLossModel = function(model) {

        propagationLossModel = model;
    };

    /**
     * @method getMessageColor
     * 
     * Get the color of the message by channel if one is being used.
     * 
     * @private
     * @param {String} channel The channel that the message is using.
     */
    function getMessageColor(channel) {

        var messageColor = new THREE.Color('white');

        if (channel === 'CH172') {

            messageColor = new THREE.Color('pink');
        }
        else if (channel === 'CH174') {

            messageColor = new THREE.Color('violet');
        }
        else if (channel === 'CH176') {

            messageColor = new THREE.Color('silver');
        }
        else if (channel === 'CH178') {

            messageColor = new THREE.Color('red');
        }
        else if (channel === 'CH180') {

            messageColor = new THREE.Color('orange');
        }
        else if (channel === 'CH182') {

            messageColor = new THREE.Color('blue');
        }
        else if (channel === 'CH184') {

            messageColor = new THREE.Color('yellow');
        }

        return messageColor;
    }
};
