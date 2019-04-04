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
 * @class EtexasControls
 * 
 * This class handles the controls for the etexas visualization.
 * 
 * @author ttevendale
 * @param {THREE.Camera} camera The camera to control.
 * @param {Object} domElement The DOM element that the canvas is attached to.
 */
THREE.EtexasControls = function(camera, domElement) {

    /**
     * @private
     * @property {THREE.EtexasControls} scope The scope object.
     */
    var scope = this;

    /**
     * @private
     * @property {THREE.Object3D} pitchObject The object which holds the pitch controls.
     */
    var pitchObject = new THREE.Object3D();
    pitchObject.add(camera);

    /**
     * @private
     * @property {THREE.Object3D} yawObject The object which holds the yaw controls.
     */
    var yawObject = new THREE.Object3D();
    yawObject.add(pitchObject);

    // Sets the controls to the correct direction (without this call the controls are wrong; also
    // this may only work with Z-UP coordinates)
    yawObject.lookAt(new THREE.Vector3(1, 0, 0));

    /**
     * @private
     * @property {Boolean} moveForward The boolean for moving forwards.
     */
    var moveForward = false;

    /**
     * @private
     * @property {Boolean} moveBackward The boolean for moving backwards.
     */
    var moveBackward = false;

    /**
     * @private
     * @property {Boolean} moveLeft The boolean for moving leftwards.
     */
    var moveLeft = false;

    /**
     * @private
     * @property {Boolean} moveRight The boolean for moving rightwards.
     */
    var moveRight = false;

    /**
     * @private
     * @property {Boolean} moveUp The boolean for moving upwards.
     */
    var moveUp = false;

    /**
     * @private
     * @property {Boolean} moveDown The boolean for moving downwards.
     */
    var moveDown = false;

    /**
     * @private
     * @property {Boolean} rotateUp The boolean for rotating upwards.
     */
    var rotateUp = false;

    /**
     * @private
     * @property {Boolean} rotateDown The boolean for rotating downwards.
     */
    var rotateDown = false;

    /**
     * @private
     * @property {Boolean} rotateLeft The boolean for rotating leftwards.
     */
    var rotateLeft = false;

    /**
     * @private
     * @property {Boolean} rotateRight The boolean for moving rightwards.
     */
    var rotateRight = false;

    /**
     * @private
     * @property {Number} movementSpeedMultiplier The number to multiple the movement speed by.
     */
    var movementSpeedMultiplier = 1;

    /**
     * @private
     * @property {Number} PI_2 The variable for holding the PI/2 Calculation (Used for performance).
     */
    var PI_2 = Math.PI / 2;

    /**
     * @property {Object} domElement The DOM element that the canvas is attached to.
     */
    this.domElement = (domElement !== undefined) ? domElement : document;
    if (domElement) {

        this.domElement.setAttribute('tabindex', -1);
    }

    /**
     * @property {Boolean} enabled The boolean for deciding even the controls are active.
     */
    this.enabled = true;

    /**
     * @property {Number} movementSpeed The movement speed for the controls.
     */
    this.movementSpeed = 10;

    /**
     * @property {Number} rotationSpeed The rotation speed for the controls.
     */
    this.rotationSpeed = 4;

    /**
     * @property {Number} xMin The x minimum for the controls.
     */
    this.xMin = -Infinity;

    /**
     * @property {Number} xMax The x maximum for the controls.
     */
    this.xMax = Infinity;

    /**
     * @property {Number} yMin The y minimum for the controls.
     */
    this.yMin = -Infinity;

    /**
     * @property {Number} yMax The y maximum for the controls.
     */
    this.yMax = Infinity;

    /**
     * @property {Number} zMin The z minimum for the controls.
     */
    this.zMin = -Infinity;

    /**
     * @property {Number} zMax The z maximum for the controls.
     */
    this.zMax = Infinity;

    /**
     * @method onKeyDown
     * 
     * Handles the event that is generated when a key is pressed down.
     * 
     * @private
     * @param {Object} event The key down event.
     */
    function onKeyDown(event) {

        switch (event.keyCode) {

            case 16: /* shift */
                movementSpeedMultiplier = 3;
                break;

            case 38: /* up */
                rotateUp = true;
                break;

            case 87: /* w */
                moveForward = true;
                break;

            case 37: /* left */
                rotateLeft = true;
                break;

            case 65: /* a */
                moveLeft = true;
                break;

            case 40: /* down */
                rotateDown = true;
                break;

            case 83: /* s */
                moveBackward = true;
                break;

            case 39: /* right */
                rotateRight = true;
                break;

            case 68: /* d */
                moveRight = true;
                break;

            case 82: /* r */
                moveUp = true;
                break;

            case 70: /* f */
                moveDown = true;
                break;
        }
    }

    /**
     * @method onKeyUp
     * 
     * Handles the event that is generated when a key goes up.
     * 
     * @private
     * @param {Object} event The key up event.
     */
    function onKeyUp(event) {

        switch (event.keyCode) {

            case 16: /* shift */
                movementSpeedMultiplier = 1;
                break;

            case 38: /* up */
                rotateUp = false;
                break;

            case 87: /* w */
                moveForward = false;
                break;

            case 37: /* left */
                rotateLeft = false;
                break;

            case 65: /* a */
                moveLeft = false;
                break;

            case 40: /* down */
                rotateDown = false;
                break;

            case 83: /* s */
                moveBackward = false;
                break;

            case 39: /* right */
                rotateRight = false;
                break;

            case 68: /* d */
                moveRight = false;
                break;

            case 82: /* R */
                moveUp = false;
                break;

            case 70: /* F */
                moveDown = false;
                break;
        }
    }

    /**
     * @method getObject
     * 
     * Gets the main object of the controls.
     * 
     * @private
     */
    this.getObject = function() {

        return yawObject;
    };

    /**
     * @method getDirection
     * 
     * Gets the direction vector that the camera is facing.
     * 
     * @private
     * @param {THREE.Vector3} vector The vector to contain the directionVector.
     */
    this.getDirection = function(vector) {

        pitchObject.getWorldDirection(vector);
    };

    /**
     * @method resetRotation
     * 
     * Resets the rotation of the controls.
     * 
     * @private
     */
    this.resetRotation = function() {

        pitchObject.rotation.x = -PI_2 + 0.1;
        yawObject.rotation.y = PI_2;
    };

    /**
     * @method update
     * 
     * Updates the controls. (This must be put into the animation or loop function for the controls
     * to work)
     * 
     * @private
     * @param {Number} delta The time since the last frame.
     */
    this.update = function(delta) {

        if (scope.enabled === false) {

            return;
        }

        var rotateSpeed = scope.rotationSpeed;
        var moveSpeed = scope.movementSpeed * movementSpeedMultiplier;

        delta *= movementSpeedMultiplier;

        if (rotateUp) {

            pitchObject.rotation.x += rotateSpeed * delta * 0.0005;
        }

        if (rotateDown) {

            pitchObject.rotation.x -= rotateSpeed * delta * 0.0005;
        }

        if (rotateLeft) {

            yawObject.rotation.y += rotateSpeed * delta * 0.0005;
        }

        if (rotateRight) {

            yawObject.rotation.y -= rotateSpeed * delta * 0.0005;
        }

        if (moveForward) {

            yawObject.translateZ(moveSpeed * -delta);
        }

        if (moveBackward) {

            yawObject.translateZ(moveSpeed * delta);
        }

        if (moveLeft) {

            yawObject.translateX(moveSpeed * -delta);
        }

        if (moveRight) {

            yawObject.translateX(moveSpeed * delta);
        }

        if (moveDown) {

            yawObject.translateY(moveSpeed * -delta);
        }

        if (moveUp) {

            yawObject.translateY(moveSpeed * delta);
        }

        pitchObject.rotation.x = Math.max(-PI_2 + 0.1, Math.min(PI_2 - 0.1, pitchObject.rotation.x));

        yawObject.position.x = Math.max(scope.xMin, Math.min(scope.xMax, yawObject.position.x));
        yawObject.position.y = Math.max(scope.yMin, Math.min(scope.yMax, yawObject.position.y));
        yawObject.position.z = Math.max(scope.zMin, Math.min(scope.zMax, yawObject.position.z));

    };

    this.domElement.addEventListener('keydown', onKeyDown, false);
    document.addEventListener('keyup', onKeyUp, false);
    this.resetRotation();
};
