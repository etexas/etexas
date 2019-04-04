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
 * @class Signal
 * 
 * This class will handle the signal on a lane.
 * 
 * @author ttevendale
 * @param {Object} signals The object that contains the signal(s) information.
 * @param {Object} laneLocation The location of the lane that the signal(s) will be created on.
 */
THREEDISPLAY.Signal = function(signals, laneLocation) {

    /**
     * @private
     * @property {Number} size The general size for the signal.
     */
    var size = 100;

    /**
     * @private
     * @property {Object} location The location where the signal is created.
     */
    var location = laneLocation;

    /**
     * @private
     * @property {THREE.Group[]} currentSignals The current signals.
     */
    var currentSignals = [];

    /**
     * @property {THREE.Mesh} mesh The mesh of this signal.
     */
    this.mesh = _createSignalMesh(signals, location);

    /**
     * @method animate
     * 
     * Handles the animations associated with signals.
     * 
     * @param {THREE.Camera} camera The camera for the scene.
     */
    this.animate = function(camera) {

        this.mesh.lookAt(camera.getWorldPosition());
    };

    /**
     * @method update
     * 
     * Handles the updates associated with this signal.
     * 
     * @param {Object[]} signals The signals to be updated.
     */
    this.update = function(signals) {

        if (signals[0].typeIndication === 'STOP_SIGN' || signals[0].typeIndication === 'YIELD_SIGN') {

            return;
        }

        var numSignals = currentSignals.length >= signals.length ? currentSignals.length : signals.length;

        var newSignals = [];

        for (var i = 0; i < numSignals; i++) {

            var trafficLight = currentSignals.pop();

            var signal = signals[i];

            if (signal && trafficLight) {

                newSignals.push(trafficLight);
                trafficLight.userData.typeIndication = signal.typeIndication;
                var light = trafficLight.getObjectByName('backgroundLight');
                var arrow = trafficLight.getObjectByName('arrow');
                var uturnArrow = trafficLight.getObjectByName('uturnArrow');
                _handleTypeIndication(signal, light, arrow, uturnArrow);
            }
            else if (!signal) {

                THREEDISPLAY.Utils.dispose(trafficLight);
            }
            else if (!trafficLight) {

                var newTrafficLight = _createTrafficLightSection(signal);
                this.mesh.add(newTrafficLight);
                newSignals.push(newTrafficLight);
            }
        }

        currentSignals = newSignals;

        _handleSignalOrder(currentSignals);
    };

    /**
     * @method _createSignalMesh
     * 
     * General function for creating a signal mesh.
     * 
     * @private
     * @param {Object} signal The signal data which will be used to create the mesh.
     * @param {Object} location The location where the signal will be created.
     * @returns {THREE.Group} The created signal mesh.
     */
    function _createSignalMesh(signals, location, signalCount, showId) {

        var mesh = {};
        var signal = signals[0];
        var type = signal.typeIndication;

        if (type === 'UNCONTROLLED' || type === 'UNKNOWN') {

            return;
        }
        else if (type === 'STOP_SIGN') {

            mesh = _createStopSignMesh(location, signal.laneId);
        }
        else if (type === 'YIELD_SIGN') {

            mesh = _createYieldSignMesh(location, signal.laneId);
        }
        else {

            mesh = _createTrafficLightMesh(location, signals);
        }

        mesh.userData = {

            type : 'Signal',
            simulationName : signal.simulationName,
            id : signal.laneId
        };

        return mesh;

        /**
         * @method _createStopSignMesh
         * 
         * Creates a stop sign mesh.
         * 
         * @private
         * @param {Object} location The location where the signal will be created.
         * @param {Number} id The lane ID where the stop sign will be created.
         * @returns {THREE.Group} The created stop sign mesh.
         */
        function _createStopSignMesh(location, id) {

            var stopSignShape = new THREE.Shape();

            // The length of each line for the octagon to be size 1 from top to bottom and left to
            // right. This isn't exact since the full number is: 0.4142135623730950488016887242097,
            // but that's too large of a number for javascript
            var octagonLineLength = 0.41421356237309505;

            // since a 45 degree angle has the same sin and cos this only needs to be calculated for
            // one of them.
            var calculatedPoint = Math.cos(Math.PI / 4) * octagonLineLength;

            stopSignShape.moveTo(0, 0);
            stopSignShape.lineTo(octagonLineLength * size, 0);
            stopSignShape.lineTo((calculatedPoint + octagonLineLength) * size, calculatedPoint * size);
            stopSignShape.lineTo((calculatedPoint + octagonLineLength) * size, (calculatedPoint + octagonLineLength) * size);
            stopSignShape.lineTo(octagonLineLength * size, ((calculatedPoint * 2) + octagonLineLength) * size);
            stopSignShape.lineTo(0, ((calculatedPoint * 2) + octagonLineLength) * size);
            stopSignShape.lineTo(-calculatedPoint * size, (calculatedPoint + octagonLineLength) * size);
            stopSignShape.lineTo(-calculatedPoint * size, calculatedPoint * size);
            stopSignShape.lineTo(0, 0);

            var geomStopSign = new THREE.ExtrudeGeometry(stopSignShape, {
                steps : 1,
                amount : 10
            });

            geomStopSign.computeBoundingBox();
            geomStopSign.center();

            var matStopSign = new THREE.MeshBasicMaterial({
                color : new THREE.Color(0xB01C2E),
                side : THREE.FrontSide
            });

            var meshStopSign = new THREE.Mesh(geomStopSign, matStopSign);

            var groupedMesh = new THREE.Group();
            groupedMesh.add(meshStopSign);
            groupedMesh.position.set(location.x, location.y, location.z + size * 10);

            return groupedMesh;
        }

        /**
         * @method
         * 
         * Creates a yield sign mesh.
         * 
         * @private
         * @param {Object} location The location where the signal will be created.
         * @param {Number} id The lane ID where the yield sign will be created.
         * @returns {THREE.Group} The created yield sign mesh.
         */
        function _createYieldSignMesh(location, id) {

            var yieldSignShape = new THREE.Shape();

            yieldSignShape.moveTo(0, 0);
            yieldSignShape.lineTo(-size * 0.5, size);
            yieldSignShape.lineTo(size * 0.5, size);
            yieldSignShape.lineTo(0, 0);

            var geomTriangle = new THREE.ExtrudeGeometry(yieldSignShape, {
                steps : 1,
                amount : size * 0.1
            });

            geomTriangle.computeBoundingBox();
            geomTriangle.center();

            var matYieldSign = new THREE.MeshBasicMaterial({
                color : new THREE.Color(0xEED202),
                side : THREE.FrontSide
            });

            var meshYieldSign = new THREE.Mesh(geomTriangle, matYieldSign);

            var groupedMesh = new THREE.Group();
            groupedMesh.add(meshYieldSign);
            groupedMesh.position.set(location.x, location.y, location.z + size * 10);

            return groupedMesh;
        }

        /**
         * @method
         * 
         * Creates a traffic light mesh.
         * 
         * @private
         * @param {Object} location The location where the signal will be created.
         * @param {Object[]} signal The signal data which will be used to create the mesh.
         * @returns {THREE.Group} The created traffic light mesh.
         */
        function _createTrafficLightMesh(location, signals) {

            var groupedMesh = new THREE.Group();
            var trafficLightSections = [];

            for (var i = 0; i < signals.length; i++) {

                var trafficLightSection = _createTrafficLightSection(signals[i]);

                trafficLightSections.push(trafficLightSection);
                groupedMesh.add(trafficLightSection);
            }

            _handleSignalOrder(trafficLightSections);

            groupedMesh.position.set(location.x, location.y, location.z + size * 10);

            return groupedMesh;
        }
    }

    /**
     * @method _createTrafficLightSection
     * 
     * Creates a traffic light section mesh.
     * 
     * @private
     * @param {Object} signal The signal to create the traffic light section mesh with.
     * @return The created traffic light section mesh.
     */
    function _createTrafficLightSection(signal) {

        var light = _createLightMesh(signal);

        var matBase = new THREE.MeshBasicMaterial({
            color : new THREE.Color(0x000000),
            side : THREE.FrontSide
        });

        var geomBox = new THREE.BoxGeometry(size, size, size * 0.5);
        var trafficBox = new THREE.Mesh(geomBox, matBase);

        var trafficLight = new THREE.Group();

        trafficLight.add(trafficBox);
        trafficLight.add(light);
        trafficLight.userData = {
            typeIndication : signal.typeIndication,
            position : 0
        };

        currentSignals.push(trafficLight);

        return trafficLight;

        /**
         * @method _createLightMesh
         * 
         * Creates the light mesh.
         * 
         * @private
         * @param {Object} signal The signal to create the light mesh with.
         * @return The created light mesh.
         */
        function _createLightMesh(signal) {

            var matLight = new THREE.MeshBasicMaterial({
                color : _getLightColor(signal.colorIndication),
                side : THREE.FrontSide
            });

            var matArrow = new THREE.MeshBasicMaterial({
                color : _getLightColor(signal.colorIndication),
                side : THREE.FrontSide
            });

            var arrow = new THREE.Mesh(_createRegularArrow(), matArrow);
            arrow.userData.arrowDirection = 'LEFT_ARROW';
            arrow.name = 'arrow';

            var uturnArrow = new THREE.Mesh(_createUturnArrow(), matArrow);
            uturnArrow.name = 'uturnArrow';

            var geomCircle = new THREE.CylinderGeometry(size * 0.4, size * 0.4, size * 0.1, 32);
            geomCircle.rotateX(Math.PI * 0.5);
            var backgroundLight = new THREE.Mesh(geomCircle, matLight);
            backgroundLight.name = 'backgroundLight';

            _handleTypeIndication(signal, backgroundLight, arrow, uturnArrow);

            var light = new THREE.Group();
            light.add(backgroundLight);
            light.add(arrow);
            light.add(uturnArrow);
            light.position.z += size * 0.25;

            return light;

            /**
             * @method _createRegularArrow
             * 
             * Creates the regular arrow geometry (for the LEFT_ARROW, STRAIGHT_ARROW and
             * RIGHT_ARROW).
             * 
             * @private
             * @return The created arrow geometry.
             */
            function _createRegularArrow() {

                var arrowShape = new THREE.Shape();

                arrowShape.moveTo(size * 0.65, size * 0.4);
                arrowShape.lineTo(size * 0.25, size * 0.4);
                arrowShape.lineTo(size * 0.25, size * 0.6);
                arrowShape.lineTo(0, size * 0.3);

                arrowShape.lineTo(size * 0.25, size * 0);
                arrowShape.lineTo(size * 0.25, size * 0.2);
                arrowShape.lineTo(size * 0.65, size * 0.2);
                arrowShape.lineTo(size * 0.65, size * 0.4);

                var geomArrow = new THREE.ExtrudeGeometry(arrowShape, {
                    steps : 1,
                    amount : size * 0.01
                });

                geomArrow.computeBoundingBox();
                geomArrow.center();

                return geomArrow;
            }

            /**
             * @method _createUturnArrow
             * 
             * Creates the uturn arrow geometry.
             * 
             * @private
             * @return The created arrow geometry.
             */
            function _createUturnArrow() {

                var arrowShape = new THREE.Shape();

                arrowShape.moveTo(size * 0.5, 0);
                arrowShape.lineTo(size * 0.5, size * 0.45);
                arrowShape.quadraticCurveTo(size * 0.25, size * 0.6, size * 0.05, size * 0.45);
                arrowShape.lineTo(size * 0.05, size * 0.075);

                arrowShape.lineTo(0, size * 0.075);
                arrowShape.lineTo(size * 0.075, 0);
                arrowShape.lineTo(size * 0.15, size * 0.075);
                arrowShape.lineTo(size * 0.1, size * 0.075);

                arrowShape.lineTo(size * 0.1, size * 0.4);
                arrowShape.quadraticCurveTo(size * 0.25, size * 0.55, size * 0.45, size * 0.4);
                arrowShape.lineTo(size * 0.45, 0);
                arrowShape.lineTo(size * 0.50, 0);

                var geomArrow = new THREE.ExtrudeGeometry(arrowShape, {
                    steps : 1,
                    amount : size * 0.01
                });

                geomArrow.computeBoundingBox();
                geomArrow.center();
                geomArrow.translate(size * -0.025, size * 0.025, 0);

                return geomArrow;
            }

        }
    }

    /**
     * @method _handleTypeIndication
     * 
     * Handles the type of the signal.
     * 
     * @private
     * @param {Object} signal The signal to be handled.
     * @param {THREE.Mesh} backgroundLight The background light of the signal.
     * @param {THREE.Mesh} arrow The arrow attached to the signal.
     * @param {THREE.Mesh} uturnArrow The uturn arrow attached to the signal.
     */
    function _handleTypeIndication(signal, backgroundLight, arrow, uturnArrow) {

        var type = signal.typeIndication;

        if (type === 'BALL') {

            backgroundLight.material.color.set(_getLightColor(signal.colorIndication));
            arrow.visible = false;
            uturnArrow.visible = false;
        }
        else if (type === 'LEFT_ARROW' || type === 'RIGHT_ARROW' || type === 'STRAIGHT_ARROW') {

            backgroundLight.material.color.set(_getLightColor());
            arrow.material.color.set(_getLightColor(signal.colorIndication));
            _handleArrowDirection(arrow, type);
            arrow.visible = true;
            uturnArrow.visible = false;
        }
        else if (type === 'UTURN_ARROW') {

            backgroundLight.material.color.set(_getLightColor());
            uturnArrow.material.color.set(_getLightColor(signal.colorIndication));
            uturnArrow.visible = true;
            arrow.visible = false;
        }

        /**
         * @method _handleArrowDirection
         * 
         * Handles the direction of the arrow.
         * 
         * @private
         * @param {THREE.Mesh} arrow The arrow to handle the direction of.
         * @param {String} typeIndication The new direction of the arrow.
         */
        function _handleArrowDirection(arrow, typeIndication) {

            var currentArrow = arrow.userData.arrowDirection;

            if (currentArrow === 'LEFT_ARROW' && typeIndication === 'RIGHT_ARROW') {

                arrow.rotateZ(Math.PI);
            }
            else if (currentArrow === 'LEFT_ARROW' && typeIndication === 'STRAIGHT_ARROW') {

                arrow.rotateZ(Math.PI * 1.5);
            }
            else if (currentArrow === 'RIGHT_ARROW' && typeIndication === 'LEFT_ARROW') {

                arrow.rotateZ(Math.PI);
            }
            else if (currentArrow === 'RIGHT_ARROW' && typeIndication === 'STRAIGHT_ARROW') {

                arrow.rotateZ(Math.PI * 0.5);
            }
            else if (currentArrow === 'STRAIGHT_ARROW' && typeIndication === 'LEFT_ARROW') {

                arrow.rotateZ(Math.PI * 0.5);
            }
            else if (currentArrow === 'STRAIGHT_ARROW' && typeIndication === 'RIGHT_ARROW') {

                arrow.rotateZ(Math.PI * 1.5);
            }

            arrow.userData.arrowDirection = typeIndication;
        }
    }

    /**
     * @method _getLightColor
     * 
     * Gets the color of the traffic light based on the colorIndication.
     * 
     * @private
     * @param {String} color The color of the light.
     * @return {THREE.Color} The traffic light color.
     */
    function _getLightColor(color) {

        if (!color) {

            return new THREE.Color(0xA9A9A9);
        }
        else if (color === 'RED') {

            return new THREE.Color(0xC1121C);
        }
        else if (color === 'GREEN') {

            return new THREE.Color(0x008754);
        }
        else if (color === 'YELLOW') {

            return new THREE.Color(0xF0CA00);
        }
    }

    /**
     * @method _handleSignalOrder
     * 
     * Handles the location of the signals.
     * 
     * @private
     * @param {THREE.Mesh[]} trafficLightSections The list of trafficLightSections to organize.
     */
    function _handleSignalOrder(trafficLightSections) {

        var leftArrows = [];
        var straightArrows = [];
        var rightArrows = [];
        var uturnArrows = [];
        var remainingSignals = [];
        var i = 0;

        var numTrafficSections = trafficLightSections.length;

        for (i = 0; i < numTrafficSections; i++) {

            var lightSection = trafficLightSections[i];
            var type = lightSection.userData.typeIndication;

            if (type === 'LEFT_ARROW') {

                leftArrows.push(lightSection);
            }
            else if (type === 'STRAIGHT_ARROW') {

                straightArrows.push(lightSection);
            }
            else if (type === 'RIGHT_ARROW') {

                rightArrows.push(lightSection);
            }
            else if (type === 'UTURN_ARROW') {

                uturnArrows.push(lightSection);
            }
            else {

                remainingSignals.push(lightSection);
            }
        }

        var signalOrder = leftArrows.concat(straightArrows, rightArrows, uturnArrows, remainingSignals);

        for (i = 0; i < numTrafficSections; i++) {

            var trafficLightSection = signalOrder[i];
            var oldPosition = trafficLightSection.userData.position;

            trafficLightSection.translateY((i - oldPosition) * size);
            trafficLightSection.userData.position = i;
        }
    }
};
