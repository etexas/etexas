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
 * @class FloatingIds
 * 
 * This class will handle the floating IDs.
 * 
 * @author ttevendale
 */
THREEDISPLAY.FloatingIds = function(scene) {

    /**
     * @private
     * @property {Boolean} The boolean for whether the IDs should be shown or not.
     */
    var showIds = true;

    /**
     * @method show
     * 
     * Shows the floating IDs in the display.
     * 
     * @public
     */
    this.show = function() {

        showIds = true;
    };

    /**
     * @method hide
     * 
     * Hides the floating IDs in the display.
     * 
     * @public
     */
    this.hide = function() {

        showIds = false;

        scene.traverseVisible(function(object) {

            if (object.userData.type && object.userData.type !== 'Lane') {

                if (object.getObjectByName('FloatingId')) {

                    object.getObjectByName('FloatingId').visible = false;
                }
            }
        });
    };

    /**
     * @method animate
     * 
     * Handles the animations associated with floating IDs.
     * 
     * @public
     * @param {THREE.Camera} camera The camera for the scene.
     */
    this.animate = function(camera) {

        if (showIds) {

            scene.traverseVisible(function(object) {

                if (object.userData.type && object.userData.type !== 'Lane') {

                    var floatingId = object.getObjectByName('FloatingId');
                    if (!floatingId) {

                        floatingId = _createFloatingIdMesh(object);
                    }
                    else {

                        object.getObjectByName('FloatingId').visible = true;
                    }

                    if (object.userData.type !== 'Signal') {

                        THREEDISPLAY.Utils.childLookAt(floatingId, camera);
                    }

                }
            });
        }
    };

    /**
     * @method _createFloatingIdMesh
     * 
     * Adds a floating ID to an object.
     * 
     * @private
     * @param {THREE.Group} object The object that will have the ID added to.
     * @return {THREE.Group} The ID mesh.
     */
    function _createFloatingIdMesh(object) {

        var geomText = new THREE.TextGeometry(object.userData.id.toString(), {
            font : THREEDISPLAY.Static.Models.getBasicFont(),
            size : 100,
            height : 10
        });

        var matText = new THREE.MeshBasicMaterial({
            color : new THREE.Color(0x000000),
            side : THREE.FrontSide
        });

        geomText.computeBoundingBox();
        geomText.center();

        var textMesh = new THREE.Mesh(geomText, matText);

        var textBox = new THREE.Vector3();
        textBox.subVectors(geomText.boundingBox.max, geomText.boundingBox.min);

        var geomTextBox = new THREE.BoxGeometry(textBox.x * 1.1, textBox.y * 1.1, textBox.z * 0.5);

        var matTextBox = new THREE.MeshBasicMaterial({
            color : new THREE.Color(0xFFFFFF),
            side : THREE.FrontSide
        });

        geomTextBox.translate(0, 0, -textBox.z * 0.25);

        var textBoxMesh = new THREE.Mesh(geomTextBox, matTextBox);

        var groupedMesh = new THREE.Group();
        groupedMesh.add(textMesh);
        groupedMesh.add(textBoxMesh);
        groupedMesh.name = 'FloatingId';

        var boundingBox = new THREE.Box3().setFromObject(object);
        if (object.userData.type === 'Signal') {

            groupedMesh.translateY((boundingBox.max.y - boundingBox.min.y) * 0.5 + 100);
        }
        else if (object.userData.objectType === 'Model') {

            groupedMesh.translateZ((boundingBox.max.z - boundingBox.min.z) + 100);
        }
        else {

            groupedMesh.translateZ((boundingBox.max.z - boundingBox.min.z) * 0.5 + 100);
        }

        object.add(groupedMesh);

        return groupedMesh;
    }
};
