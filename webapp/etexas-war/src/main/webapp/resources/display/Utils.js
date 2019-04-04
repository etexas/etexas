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
 * @class Utils
 * 
 * Contains utility functions for the display.
 * 
 * @author ttevendale
 */
THREEDISPLAY.Utils = function() {};

/**
 * @method dispose
 * 
 * Removes a three.js object and its children.
 * 
 * @static
 * @param {THREE.Object3D} object The object (and its children) to remove from the display.
 */
THREEDISPLAY.Utils.dispose = function(object) {

    var i = 0;

    var numChildren = object.children.length;
    for (i = numChildren - 1; i >= 0; i--) {

        THREEDISPLAY.Utils.dispose(object.children[i]);
    }

    if (object.material) {

        if (Array.isArray(object.material)) {

            var numMaterials = object.material.length;
            for (i = 0; i < numMaterials; i++) {

                object.material[i].dispose();
            }
        }
        else {

            object.material.dispose();
        }
    }

    if (object.geometry) {

        object.geometry.dispose();
    }

    if (object.parent) {

        object.parent.remove(object);
    }
};

/**
 * @method getObjectWhichContainsUserData
 * 
 * Retrieves the object that contains userData from either this object or it's ancestors.
 * 
 * @static
 * @param {THREE.Object3D} object The object (and its ancestors) to check for userData.
 * @return {THREE.Object3D} The object which contained userData or null if one didn't exist.
 */
THREEDISPLAY.Utils.getObjectWhichContainsUserData = function(object) {

    var done = false;

    while (object && !done) {

        if (object.userData.type) {

            done = true;
        }
        else {

            object = object.parent;
        }
    }

    return object;
};

/**
 * @method childLookAt
 * 
 * Gives a child object the ability to look at a camera. (the normal Object3D.lookAt in THREE.js
 * only works correctly with objects that are direct children of the Scene object.)
 * 
 * @static
 * @param {THREE.Object3D} childObject The object that wants to look at the camera.
 * @param {THREE.Camera} camera The camera to look at.
 */
THREEDISPLAY.Utils.childLookAt = function(childObject, camera) {

    var matrix = new THREE.Matrix4();
    var vector = new THREE.Vector3();

    var object = childObject;
    while (object) {

        vector.add(object.position);
        object = object.parent;
    }

    matrix.lookAt(camera.getWorldPosition(), vector, childObject.up);

    childObject.quaternion.setFromRotationMatrix(matrix);
};
