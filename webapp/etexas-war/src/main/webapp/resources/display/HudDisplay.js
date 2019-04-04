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
 * @class HudDisplay
 * 
 * This class will handle the HUD display.
 * 
 * @author ttevendale
 * @param {THREE.Camera} mainCamera The camera from the main scene.
 */
THREEDISPLAY.HudDisplay = function(mainCamera) {

    /**
     * @private
     * @property {THREE.AxisHelper} axes The positive axes for the HUD display.
     */
    var axes;

    /**
     * @private
     * @property {THREE.Camera} camera The camera for this simulation display.
     */
    var camera;

    /**
     * @private
     * @property {Number} CAM_DISTANCE The camera's distance from the center.
     * @readonly
     */
    var CAM_DISTANCE = 275;

    /**
     * @private
     * @property {Number} CANVA_HEIGHT The height of the HUD canvas.
     * @readonly
     */
    var CANVAS_HEIGHT = 200;

    /**
     * @private
     * @property {Number} CANVA_WIDTH The width of the HUD canvas.
     * @readonly
     */
    var CANVAS_WIDTH = 200;

    /**
     * @private
     * @property {THREE.Vector3} The direction vector where the camera is looking.
     */
    var directionVector = new THREE.Vector3();

    /**
     * @private
     * @property {Object} hudElement The HTML element where the HUD is located.
     */
    var hudElement;

    /**
     * @private
     * @property {THREE.WebGLRenderer} renderer The renderer for three.js.
     */
    var renderer;

    /**
     * @private
     * @property {THREE.Scene} scene The scene for this simulation display.
     */
    var scene;

    /**
     * @private
     * @property {THREE.Mesh} xMesh The x text mesh for the x axis.
     */
    var xMesh;

    /**
     * @private
     * @property {THREE.Mesh} yMesh The y text mesh for the y axis.
     */
    var yMesh;

    /**
     * @private
     * @property {THREE.Mesh} zMesh The z text mesh for the z axis.
     */
    var zMesh;

    _init();

    /**
     * @method _init
     * 
     * Handles the initialization of the HUD.
     * 
     * @private
     */
    function _init() {

        hudElement = document.createElement("div");
        hudElement.style.position = "absolute";
        hudElement.style.top = "0";
        hudElement.style.right = "0";

        document.getElementById('ThreeDisplayElement').appendChild(hudElement);

        renderer = new THREE.WebGLRenderer();
        renderer.setClearColor(0xf0f0f0, 1);
        renderer.setSize(CANVAS_WIDTH, CANVAS_HEIGHT);
        hudElement.appendChild(renderer.domElement);

        scene = new THREE.Scene();

        camera = new THREE.PerspectiveCamera(50, CANVAS_WIDTH / CANVAS_HEIGHT, 1, 1000);
        camera.up = mainCamera.up; // important!

        axes = new THREE.AxisHelper(100);

        scene.add(axes);

        var loader = new THREE.FontLoader();
        loader.load('/resources/display/fonts/helvetiker_regular.typeface.json', function(font) {

            THREEDISPLAY.HudDisplay.font = font;
            _addXYZText();
        });

        /**
         * @method _addXYZText
         * 
         * Addes the X, Y, and Z text to the axes.
         * 
         * @private
         */
        function _addXYZText() {

            xMesh = _createTextMesh("X");
            xMesh.translateX(100);

            yMesh = _createTextMesh("Y");
            yMesh.translateY(100);

            zMesh = _createTextMesh("Z");
            zMesh.translateZ(100);

            /**
             * @method createTextMesh
             * 
             * Creates the text mesh for the axes.
             * 
             * @private
             * @param {THREE.Mesh} text The text mesh.
             */
            function _createTextMesh(text) {

                var textGeom = new THREE.TextGeometry(text, {
                    font : THREEDISPLAY.HudDisplay.font,
                    size : 10,
                    height : 1
                });

                var matText = new THREE.MeshBasicMaterial({
                    color : new THREE.Color(0x000000),
                    side : THREE.FrontSide
                });

                var textMesh = new THREE.Mesh(textGeom, matText);

                axes.add(textMesh);

                return textMesh;
            }
        }
    }

    /**
     * @method animate
     * 
     * Handles the animations associated with this HUD.
     * 
     * @public
     * @param {THREE.Controls} controls The controls for the main scene.
     */
    this.animate = function(controls) {

        controls.getDirection(directionVector);

        camera.position.set(0, 0, 0);
        camera.position.add(directionVector);
        camera.position.setLength(CAM_DISTANCE);

        if (xMesh) {
            xMesh.lookAt(camera.position);
        }
        if (yMesh) {
            yMesh.lookAt(camera.position);
        }
        if (zMesh) {
            zMesh.lookAt(camera.position);
        }

        camera.lookAt(scene.position);
        renderer.render(scene, camera);
    };

    /**
     * @method show
     * 
     * Shows the HUD in the overall display.
     * 
     * @public
     */
    this.show = function() {

        hudElement.style.display = "block";
        hudElement.style.visibility = "visible";
    };

    /**
     * @method hide
     * 
     * Hides the HUD from the overall display
     * 
     * @public
     */
    this.hide = function() {

        hudElement.style.display = "none";
        hudElement.style.visibility = "hidden";
    };
};
