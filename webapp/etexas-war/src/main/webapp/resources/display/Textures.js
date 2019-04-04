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
 * @class Textures
 * 
 * This class will handle the textures.
 * 
 * @public
 */
THREEDISPLAY.Textures = function() {

    _init();

    /**
     * @private
     * @property {THREEDISPLAY.Textures.Lanes} laneTextures The lane textures class.
     */
    var laneTextures;

    /**
     * @method getBasicLane
     * 
     * Gets the basic lane texture.
     * 
     * @public
     * @return {THREE.Texture} The lane Texture.
     */
    this.getBasicLane = function() {

        return laneTextures.getBasicLane();
    };

    /**
     * @method _init
     * 
     * The init method for the textures class.
     * 
     * @private
     */
    function _init() {

        laneTextures = new THREEDISPLAY.Textures.Lanes();
        laneTextures.init();
    }
};

/**
 * @class Textures.Lanes
 * 
 * This class will handle the lane textures.
 * 
 * @public
 */
THREEDISPLAY.Textures.Lanes = function() {

    /**
     * @private
     * @property {THREE.Mesh} basicLane The basic lane texture.
     */
    var basicLane;

    /**
     * @method getBasicLane
     * 
     * Gets a basic lane texture.
     * 
     * @public
     * @return {THREE.Texture} The basic lane texture.
     */
    this.getBasicLane = function() {

        var basicLaneClone = basicLane.clone();
        basicLaneClone.needsUpdate = true;
        return basicLaneClone;
    };

    /**
     * @method init
     * 
     * The init method for the Textures.Lanes class.
     * 
     * @public
     */
    this.init = function() {

        basicLane = new THREE.TextureLoader().load('resources/display/images/basicRoad.png');
    };
};

/**
 * Creates the THREEDISPLAY.Static name space.
 */
THREEDISPLAY.Static = THREEDISPLAY.Static || {};

/**
 * Creates the THREEDISPLAY.Static.Textures instance.
 */
THREEDISPLAY.Static.Textures = new THREEDISPLAY.Textures();
