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

// Updates the Object3D to Z-UP
THREE.Object3D.DefaultUp = new THREE.Vector3(0, 0, 1);

/**
 * @class ThreeMain
 * 
 * This class will create a three.js display of an execution.
 * 
 * @author ttevendale
 */
THREEDISPLAY.Main = function() {

    /**
     * @private
     * @property {THREE.Camera} camera The camera for the display.
     */
    var camera;

    /**
     * @private
     * @property {Map} cellTowers The cell towers for the display.
     */
    var cellTowers;

    /**
     * @private
     * @property {Ext.container.Container} container The container where this display is located.
     */
    var container;

    /**
     * @private
     * @property {THREE.Control} controls The controls for the display.
     */
    var controls;

    /**
     * @private
     * @property {Map} detectors The detectors for the display.
     */
    var detectors;

    /**
     * @private
     * @property {THREEDISPLAY.HudDisplay} hudDisplay The HUD for the display.
     */
    var hudDisplay;

    /**
     * @private
     * @property {Map} lanes The lanes for the display.
     */
    var lanes;

    /**
     * @private
     * @property {THREEDISPLAY.MessageManager} messageManager The manager to handle the messages for
     * the display.
     */
    var messageManager;

    /**
     * @private
     * @property {Ext.panel.Panel} panel The panel where this display is located.
     */
    var panel;

    /**
     * @private
     * @property {DOMHighResTimeStamp} The previous time. (Used for calculating the delta time)
     */
    var prevTime = performance.now();

    /**
     * @private
     * @property {THREE.WebGLRenderer} renderer The renderer for three.js.
     */
    var renderer;

    /**
     * @private
     * @property {THREE.Scene} scene The scene for the display.
     */
    var scene;

    /**
     * @private
     * @property {Map} standaloneDevices The devices for the display.
     */
    var standaloneDevices;

    /**
     * @private
     * @property {Map} topographyFeatures The topography features for the display.
     */
    var topographyFeatures;

    /**
     * @private
     * @property {Map} vehicles The vehicles for the display.
     */
    var vehicles;

    /**
     * @private
     * @property {THREEDISPLAY.FloatingIds} floatingIds The floating IDs for the display.
     */
    var floatingIds;

    var obuMap = new Map();

    /**
     * @method init
     * 
     * The initializer for the display.
     * 
     * @public
     * @param {Ext.container.Container} initContainer The container for the display.
     * @param {Ext.panel.Panel} initPanel The panel the display will be in.
     */
    this.init = function(initContainer, initPanel) {

        container = initContainer;

        panel = initPanel;

        createScene();
        createLights();

        floatingIds = new THREEDISPLAY.FloatingIds(scene);
        hudDisplay = new THREEDISPLAY.HudDisplay(camera);
        messageManager = new THREEDISPLAY.MessageManager(scene);

        cellTowers = new THREEDISPLAY.Map(scene);
        detectors = new THREEDISPLAY.Map(scene);
        lanes = new THREEDISPLAY.Map(scene);
        standaloneDevices = new THREEDISPLAY.Map(scene);
        topographyFeatures = new THREEDISPLAY.Map(scene);

        vehicles = new THREEDISPLAY.Map(scene);

        createBackground();

        createGround();

        loop();

        /**
         * @method loop
         * 
         * Handles the animation in the display.
         * 
         * @private
         */
        function loop() {

            var time = performance.now();
            var delta = (time - prevTime);

            floatingIds.animate(camera);
            cellTowers.animate(camera);
            detectors.animate(camera);
            lanes.animate(camera);
            standaloneDevices.animate(camera);
            topographyFeatures.animate(camera);
            vehicles.animate(camera);
            hudDisplay.animate(controls);

            renderer.render(scene, camera);
            controls.update(delta);
            requestAnimationFrame(loop);

            prevTime = time;
        }

        /**
         * @method createLights
         * 
         * Creates the lights for the display.
         * 
         * @private
         */
        function createLights() {

            var light = new THREE.HemisphereLight();

            light.intensity = 0.7;
            light.name = 'Sky_Light';

            var sun = new THREE.DirectionalLight(16774373, 1);

            sun.position.set(1, 1, 1).normalize();
            sun.name = 'Sun_Light';

            scene.add(light);
            scene.add(sun);
        }

        /**
         * @method createScene
         * 
         * Creates the scene for the display.
         * 
         * @private
         */
        function createScene() {

            var panelBox = panel.getBox();

            scene = new THREE.Scene();

            // Create the camera
            var aspectRatio = panelBox.width / panelBox.height;
            var fieldOf = 60;
            var nearPlane = 1;
            var farPlane = 10000000;
            camera = new THREE.PerspectiveCamera(fieldOf, aspectRatio, nearPlane, farPlane);

            // Updates the camera to Z-UP
            camera.up = new THREE.Vector3(0, 0, 1);

            renderer = new THREE.WebGLRenderer({

                // Activate the anti-aliasing; this is less performant
                antialias : true,

                logarithmicDepthBuffer : true
            });
            renderer.setClearColor(0xffffff, 1);

            renderer.setSize(panelBox.width, panelBox.height);

            // controls
            controls = new THREE.EtexasControls(camera, renderer.domElement);
            controls.zMin = 200;
            var obj = controls.getObject();
            obj.position.set(0, 10000, 20000);
            scene.add(obj);

            var threeDisplayElement = document.getElementById('ThreeDisplayElement');

            threeDisplayElement.appendChild(renderer.domElement);

            panel.addListener('resize', handlePanelResize);

            renderer.domElement.addEventListener('click', handleMouseClick, false);
        }

        /**
         * @method createGround
         * 
         * Creates the ground for the display
         * 
         * @private
         */
        function createGround() {

            var geomGround = new THREE.PlaneGeometry(1000000, 1000000, 100, 100);

            var matGround = new THREE.MeshBasicMaterial({
                // grass green
                color : new THREE.Color(0x006400)
            });

            scene.add(new THREE.Mesh(geomGround, matGround));
        }

        /**
         * @method handlePanelResize
         * 
         * Handles the panel resize.
         * 
         * @private
         * @param {Ext.panel.Panel} panel The panel that has been resized.
         */
        function handlePanelResize(panel, info, eOpts) {

            // update height and width of the renderer and the camera
            var Box = panel.getBox();
            renderer.setSize(Box.width, Box.height);
            camera.aspect = Box.width / Box.height;
            camera.updateProjectionMatrix();
        }

        /**
         * @method createBackground
         * 
         * Creates the background for the display.
         * 
         * @private
         */
        function createBackground() {

            var geomBackground = new THREE.SphereGeometry(10000000, 32, 32);

            var materialBackground = new THREE.MeshBasicMaterial({
                // sky blue
                color : new THREE.Color(0x7EC0EE),
                side : THREE.BackSide
            });

            scene.add(new THREE.Mesh(geomBackground, materialBackground));
        }
    };

    /**
     * @method addBroadcastMessage
     * 
     * Adds a broadcast message based on the passed in MAC addresse.
     * 
     * @public
     * @param {Number} macAddress The mac address.
     * @param {String} channel The channel that the message is using.
     */
    this.addBroadcastMessage = function(macAddress, channel) {

        var txObject = standaloneDevices.get(macAddress);

        if (!txObject) {

            txObject = vehicles.get(obuMap.get(macAddress));
        }

        messageManager.addBroadcastMessage(txObject.mesh, channel);
    };

    /**
     * @method addTopographyFeatures
     * 
     * Adds the topography features to the display.
     * 
     * @param {Object[]} featureData The list of feature data.
     */
    this.addTopographyFeatures = function(featureData) {

        for (var i = 0; i < featureData.length; i++) {

            var feature = featureData[i];

            if (!topographyFeatures.has(feature.id)) {

                topographyFeatures.add(new THREEDISPLAY.TopographyFeature(feature));
            }
        }
    };

    /**
     * @method addCellTowers
     * 
     * Adds the cell towers to the display.
     * 
     * @param {Object[]} cellTowerData The list of cell tower data.
     */
    this.addCellTowers = function(cellTowerData) {

        for (var i = 0; i < cellTowerData.length; i++) {

            var cellTower = cellTowerData[i];

            if (!cellTowers.has(cellTower.id)) {

                cellTowers.add(new THREEDISPLAY.CellTower(cellTower));
            }
        }
    };

    /**
     * @method addDetectors
     * 
     * Adds the detectors to the display.
     * 
     * @public
     * @param {Object[]} detectorData The list of detector data.
     */
    this.addDetectors = function(detectorData) {

        for (var i = 0; i < detectorData.length; i++) {

            var detector = detectorData[i];

            if (!detectors.has(detector.detectorID)) {

                detectors.add(new THREEDISPLAY.Detector(detector));
            }
        }
    };

    /**
     * @method addLanes
     * 
     * Adds the lanes to the display.
     * 
     * @public
     * @param {Object[]} laneData The list of lane data.
     */
    this.addLanes = function(laneData) {

        for (var i = 0; i < laneData.length; i++) {

            var lane = laneData[i];

            if (!lanes.has(THREEDISPLAY.Lane.getProperId(lane))) {

                lanes.add(new THREEDISPLAY.Lane(lane));
            }
        }
    };

    /**
     * @method addSignals
     * 
     * Adds the signals to the display.
     * 
     * @public
     * @param {Object[]} signalData The list of signal data.
     */
    this.addSignals = function(signalData) {

        var signalMap = new Map();

        for (var i = 0; i < signalData.length; i++) {

            var signal = signalData[i];

            if (signalMap.has(THREEDISPLAY.Lane.getProperId(signal))) {

                var signalArray = signalMap.get(THREEDISPLAY.Lane.getProperId(signal));
                signalArray.push(signal);

                signalMap.set(THREEDISPLAY.Lane.getProperId(signal), signalArray);
            }
            else {

                signalMap.set(THREEDISPLAY.Lane.getProperId(signal), [ signal ]);
            }
        }

        signalMap.forEach(function(value, key, map) {

            var lane = lanes.get(key);
            lane.addSignal(value);
        });
    };

    /**
     * @method addStandaloneDevices
     * 
     * Adds the standalone devices to the display.
     * 
     * @public
     * @param {Object[]} standaloneDeviceData The list of standalone device data.
     */
    this.addStandaloneDevices = function(standaloneDeviceData) {

        for (var i = 0; i < standaloneDeviceData.length; i++) {

            var standaloneDevice = standaloneDeviceData[i];

            if (!standaloneDevices.has(standaloneDevice.deviceMac)) {

                standaloneDevices.add(new THREEDISPLAY.StandaloneDevice(standaloneDevice));
            }
        }
    };

    /**
     * @method addUnicastMessage
     * 
     * Adds a unicast message based on the passed in MAC addresses.
     * 
     * @public
     * @param {Number} txMacAddress The tx mac address.
     * @param {Number} rxMacAddress The rx mac address.
     * @param {String} channel The channel that the message is using.
     */
    this.addUnicastMessage = function(txMacAddress, rxMacAddress, channel) {

        var txObject = standaloneDevices.get(txMacAddress);
        var rxObject = standaloneDevices.get(rxMacAddress);

        if (!txObject) {

            txObject = vehicles.get(obuMap.get(txMacAddress));
        }
        if (!rxObject) {

            rxObject = vehicles.get(obuMap.get(rxMacAddress));
        }

        // It's possible for a message to be sent to a mac address which does not exist
        if (rxObject) {

            messageManager.addUnicastMessage(txObject.mesh, rxObject.mesh, channel);
        }
    };

    /**
     * @method changeCameraLocation
     * 
     * Changes the camera location.
     * 
     * @public
     * @param {Number} x The desired x coordinate target of the camera.
     * @param {Number} y The desired y coordinate target of the camera.
     */
    this.changeCameraLocation = function(x, y) {

        var obj = controls.getObject();
        obj.position.set(x + 2500, y, 20000);
        controls.resetRotation();
    };

    /**
     * @method clearMessages
     * 
     * Clears the messages from the display.
     * 
     * @public
     */
    this.clearMessages = function() {

        messageManager.clear();
    };

    /**
     * @method hideBuildings
     * 
     * Hides the buildings in the display.
     * 
     * @public
     */
    this.hideBuildings = function() {

        topographyFeatures.hide();
    };

    /**
     * @method hideCellTowers
     * 
     * Hides the cell towers in the display.
     * 
     * @public
     */
    this.hideCellTowers = function() {

        cellTowers.hide();
    };

    /**
     * @method hideDetectors
     * 
     * Hides the detectors in the display.
     * 
     * @public
     */
    this.hideDetectors = function() {

        detectors.hide();
    };

    /**
     * @method hideFloatingIds
     * 
     * Hides the floating IDs in the display.
     * 
     * @public
     */
    this.hideFloatingIds = function() {

        floatingIds.hide();
    };

    /**
     * @method hideHud
     * 
     * Hides the HUD in the display.
     * 
     * @public
     */
    this.hideHud = function() {

        hudDisplay.hide();
    };

    /**
     * @method hideLanes
     * 
     * Hides the lanes in the display.
     * 
     * @public
     */
    this.hideLanes = function() {

        lanes.hide();
    };

    /**
     * @method hideSignals
     * 
     * Hides the signals in the display.
     * 
     * @public
     */
    this.hideSignals = function() {

        THREEDISPLAY.Lane.signalIsVisible = false;
        lanes.forEach(function(value, key, map) {

            value.hideSignal();
        });
    };

    /**
     * @method hideStandaloneDevices
     * 
     * Hides the standalone devices in the display.
     * 
     * @public
     */
    this.hideStandaloneDevices = function() {

        standaloneDevices.hide();
    };

    /**
     * @method hideVehicles
     * 
     * Hides the vehicles in the display.
     * 
     * @public
     */
    this.hideVehicles = function() {

        vehicles.hide();
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

        messageManager.setPropagationLossModel(model);
    };

    /**
     * @method reset
     * 
     * Resets the state of the display.
     * 
     * @public
     */
    this.reset = function() {

        this.changeCameraLocation(0, 0);
        messageManager.clear();
        cellTowers.clear();
        detectors.clear();
        lanes.forEach(function(value, key, map) {

            value.clearSignal();
        });
        lanes.clear();
        standaloneDevices.clear();
        topographyFeatures.clear();
        obuMap.clear();
        vehicles.clear();
    };

    /**
     * @method showBuildings
     * 
     * Shows the buildings in the display.
     * 
     * @public
     */
    this.showBuildings = function() {

        topographyFeatures.show();
    };

    /**
     * @method showCellTowers
     * 
     * Shows the cell towers in the display.
     * 
     * @public
     */
    this.showCellTowers = function() {

        cellTowers.show();
    };

    /**
     * @method showDetectors
     * 
     * Shows the detectors in the display.
     * 
     * @public
     */
    this.showDetectors = function() {

        detectors.show();
    };

    /**
     * @method showFloatingIds
     * 
     * Shows the floating IDs in the display.
     * 
     * @public
     */
    this.showFloatingIds = function() {

        floatingIds.show();
    };

    /**
     * @method showHud
     * 
     * Shows the HUD in the display.
     * 
     * @public
     */
    this.showHud = function() {

        hudDisplay.show();
    };

    /**
     * @method showLanes
     * 
     * Shows the lanes in the display.
     * 
     * @public
     */
    this.showLanes = function() {

        lanes.show();
    };

    /**
     * @method showSignals
     * 
     * Shows the signals in the display.
     * 
     * @public
     */
    this.showSignals = function() {

        THREEDISPLAY.Lane.signalIsVisible = true;
        lanes.forEach(function(value, key, map) {

            value.showSignal();
        });
    };

    /**
     * @method showStandaloneDevices
     * 
     * Shows the standalone devices in the display.
     * 
     * @public
     */
    this.showStandaloneDevices = function() {

        standaloneDevices.show();
    };

    /**
     * @method showVehicles
     * 
     * Shows the vehicles in the display.
     * 
     * @public
     */
    this.showVehicles = function() {

        vehicles.show();
    };

    /**
     * @method updateVehicles
     * 
     * Updates the vehicles.
     * 
     * @public
     * @param {Object[]} vehicleData The list of vehicle data.
     */
    this.updateVehicles = function(vehicleData) {

        var newVehicles = [];
        var remainingVehicles = new Map();

        for (var i = 0; i < vehicleData.length; i++) {

            var vehicle = vehicleData[i];

            if (!vehicles.has(vehicle.vehicleID)) {

                newVehicles.push(vehicle);
                addToObuMap(vehicle.devices, vehicle.vehicleID);

            }
            else {

                remainingVehicles.set(vehicle.vehicleID, vehicle);
            }
        }

        vehicles.forEach(function(value, key, map) {

            if (remainingVehicles.has(value.id)) {

                var vehicleData = remainingVehicles.get(value.id);
                value.update(vehicleData);
            }
            else {

                vehicles.remove(value.id);
                removeFromObuMap(value.devices);
            }
        });

        for (i = 0; i < newVehicles.length; i++) {

            vehicles.add(new THREEDISPLAY.Vehicle(newVehicles[i]));
        }
    };

    /**
     * @method addToObuMap
     * 
     * Adds a list of OBUs to the obu map.
     * 
     * @private
     * @param {Object[]} obus The list of obus to add.
     * @param {String} vehicleId The vehicle ID that the OBUs are matched to.
     */
    function addToObuMap(obus, vehicleId) {

        if (!obus) {

            return;
        }

        for (var i = 0; i < obus.length; i++) {

            obuMap.set(obus[i].deviceMac, vehicleId);
        }
    }

    /**
     * @method handleMouseClick
     * 
     * Handles the mouse click event.
     * 
     * @private
     * @param {Object} event The event.
     */
    function handleMouseClick(event) {

        var box = panel.getBox();
        var mouseX = (event.offsetX / box.width) * 2 - 1;
        var mouseY = -(event.offsetY / box.height) * 2 + 1;

        var mouse = new THREE.Vector2(mouseX, mouseY);

        var raycaster = new THREE.Raycaster();
        raycaster.setFromCamera(mouse, camera);
        var intersects = raycaster.intersectObjects(scene.children, true);

        for (var i = 0; i < intersects.length; i++) {

            var object = THREEDISPLAY.Utils.getObjectWhichContainsUserData(intersects[i].object);

            if (object) {

                container.addDataToInfoPanel(object.userData.type, object.userData.simulationName, object.userData.id);
                break;
            }
        }
    }

    /**
     * @method removeFromObuMap
     * 
     * removes a list of OBUs from the obu map.
     * 
     * @private
     * @param {Object[]} obus The list of obus to remove.
     */
    function removeFromObuMap(obus) {

        for (var i = 0; i < obus.length; i++) {

            // used because map.delete(id) can get confused with the delete operator in javascript
            obuMap["delete"](obus[i].deviceMac);
        }
    }
};
