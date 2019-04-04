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
 * @class Map
 * 
 * This class will handle the map logic for the display.
 * 
 * @author ttevendale
 * @param {THREE.Scene} The scene that will have objects added to/removed from.
 */
THREEDISPLAY.Map = function(scene) {

    /**
     * @private
     * @property {Map} map The map of objects.
     */
    var map = new Map();

    /**
     * @private
     * @property {Boolean} isVisible The current visibility of the objects.
     */
    var isVisible = true;

    /**
     * @method add
     * 
     * Adds an object to the map.
     * 
     * @public
     * @param {Object} object The object to be added.
     */
    this.add = function(object) {

        if (object.mesh) {

            object.mesh.visible = isVisible;
            scene.add(object.mesh);
            map.set(object.id, object);
        }
    };

    /**
     * @method remove
     * 
     * Removes an object from the map.
     * 
     * @public
     * @param {Number} id The ID of the object to remove.
     * @return {Object/false} the removed object or false if not found.
     */
    this.remove = function(id) {

        var object = map.get(id);

        if (object) {

            THREEDISPLAY.Utils.dispose(object.mesh);

            // used because map.delete(id) can get confused with the delete operator in javascript
            map["delete"](id);

            return object;
        }
        else {

            return false;
        }
    };

    /**
     * @method clear
     * 
     * Clears all of the objects from the map.
     * 
     * @public
     */
    this.clear = function() {

        map.forEach(function(value, key, map) {

            THREEDISPLAY.Utils.dispose(value.mesh);
        });

        map.clear();
    };

    /**
     * @method get
     * 
     * Gets an object from the map.
     * 
     * @public
     * @param {Number} id The ID of the object to get from the map.
     * @return {Object} The object that has the ID that was passed in.
     */
    this.get = function(id) {

        return map.get(id);
    };

    /**
     * @method has
     * 
     * Checks if an object is in the map.
     * 
     * @public
     * @param {Number} id The ID of the object to check.
     * @return {Boolean} True if the object with the ID exists, false otherwise.
     */
    this.has = function(id) {

        return map.has(id);
    };

    /**
     * @method forEach
     * 
     * Loops through each object in the map.
     * 
     * @public
     * @param {Function} fn The function to run.
     */
    this.forEach = function(fn) {

        map.forEach(fn);
    };

    /**
     * @method animate
     * 
     * Handles the animations associated with the objects.
     * 
     * @public
     * @param {THREE.Camera} camera The camera for the scene.
     */
    this.animate = function(camera) {

        map.forEach(function(value, key, map) {

            value.animate(camera);
        });
    };

    /**
     * @method hide
     * 
     * Hides the objects in the map.
     * 
     * @public
     */
    this.hide = function() {

        isVisible = false;
        map.forEach(function(value, key, map) {

            value.mesh.visible = false;
        });
    };

    /**
     * @method show
     * 
     * Shows the objects in the map.
     * 
     * @public
     */
    this.show = function() {

        isVisible = true;
        map.forEach(function(value, key, map) {

            value.mesh.visible = true;
        });
    };
};
