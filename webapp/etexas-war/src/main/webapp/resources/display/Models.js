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
 * @class Models
 * 
 * This class will handle the models.
 * 
 * @public
 */
THREEDISPLAY.Models = function() {

    _init();

    /**
     * @private
     * @property {THREEDISPLAY.Models.CellPhones} cellPhoneModels The cell phone models class.
     */
    var cellPhoneModels;

    /**
     * @private
     * @property {THREEDISPLAY.Models.CellTowers} cellTowerModels The cell tower models class.
     */
    var cellTowerModels;

    /**
     * @private
     * @property {THREEDISPLAY.Models.Fonts} fonts The fonts class.
     */
    var fonts;

    /**
     * @private
     * @property {THREEDISPLAY.Models.RSUs} rsuModels the RSU models class.
     */
    var rsuModels;

    /**
     * @private
     * @property {THREEDISPLAY.Models.Vehicles} vehicleModels The vehicle models class.
     */
    var vehicleModels;

    /**
     * @method getBasicFont
     * 
     * Gets the basic font.
     * 
     * @public
     * @return {THREE.Font} The text font.
     */
    this.getBasicFont = function() {

        return fonts.getBasicFont();
    };

    /**
     * @method getBasicVehicle
     * 
     * Gets a basic vehicle model.
     * 
     * @public
     * @return {THREEDISPLAY.Models.Vehicles.Vehicle} The basic vehicle model.
     */
    this.getBasicVehicle = function() {

        return vehicleModels.getBasicVehicle();
    };

    /**
     * @method getBus
     * 
     * Gets a bus model.
     * 
     * @public
     * @return {THREEDISPLAY.Models.Vehicles.Vehicle} The bus model.
     */
    this.getBus = function() {

        return vehicleModels.getBus();
    };

    /**
     * @method getCar
     * 
     * Gets a car model.
     * 
     * @public
     * @return {THREEDISPLAY.Models.Vehicles.Vehicle} The car model.
     */
    this.getCar = function() {

        return vehicleModels.getCar();
    };

    /**
     * @method getCellPhone
     * 
     * Gets a cell phone model.
     * 
     * @public
     * @return {THREE.Mesh} The cell phone model.
     */
    this.getCellPhone = function() {

        return cellPhoneModels.getCellPhone();
    };

    /**
     * @method getCellTower
     * 
     * Gets a cell tower model.
     * 
     * @public
     * @return {THREE.Mesh} The cell tower model.
     */
    this.getCellTower = function() {

        return cellTowerModels.getCellTower();
    };

    /**
     * @method getRSU
     * 
     * Gets a RSU model.
     * 
     * @public
     * @return {THREE.Mesh} The RSU model.
     */
    this.getRSU = function() {

        return rsuModels.getRSU();
    };

    /**
     * @method getSemiTruck
     * 
     * Gets a semi truck model.
     * 
     * @public
     * @return {THREEDISPLAY.Models.Vehicles.Vehicle} The semi truck model.
     */
    this.getSemiTruck = function() {

        return vehicleModels.getSemiTruck();
    };

    /**
     * @method _init
     * 
     * The init method for the Models class.
     * 
     * @private
     */
    function _init() {

        cellPhoneModels = new THREEDISPLAY.Models.CellPhones();
        cellTowerModels = new THREEDISPLAY.Models.CellTowers();
        fonts = new THREEDISPLAY.Models.Fonts();
        rsuModels = new THREEDISPLAY.Models.RSUs();
        vehicleModels = new THREEDISPLAY.Models.Vehicles();

        cellPhoneModels.init();
        cellTowerModels.init();
        fonts.init();
        rsuModels.init();
        vehicleModels.init();
    }
};

/**
 * @class Models.CellPhones
 * 
 * This class will handle the cell phone models.
 * 
 * @public
 */
THREEDISPLAY.Models.CellPhones = function() {

    /**
     * @private
     * @property {THREE.Mesh} cellPhone The cell phone model.
     */
    var cellPhone;

    /**
     * @method getCellPhone
     * 
     * Gets a cell phone model.
     * 
     * @public
     * @return {THREE.Mesh} The cell phone model.
     */
    this.getCellPhone = function() {

        return cellPhone.clone();
    };

    /**
     * @method init
     * 
     * The init method for the Models.CellPhones class.
     * 
     * @public
     */
    this.init = function() {

        createCellPhone();

        /**
         * @method createCellPhone
         * 
         * Creates the cell phone model.
         * 
         * @private
         */
        function createCellPhone() {

            var colladaLoader = new THREE.ColladaLoader();
            colladaLoader.load('resources/display/models/cellPhone/model.dae', function(collada) {

                var phone = collada.scene;
                phone.rotateX(Math.PI * -0.3);
                phone.position.z += 115;
                phone.position.y -= 100;
                phone.position.x -= 35;
                phone.scale.set(50, 50, 50);

                var phoneWrapper = new THREE.Group();
                phoneWrapper.add(phone);
                cellPhone = phoneWrapper;
            });
        }
    };
};

/**
 * @class Models.CellTowers
 * 
 * This class will handle the cell tower models.
 * 
 * @public
 */
THREEDISPLAY.Models.CellTowers = function() {

    /**
     * @private
     * @property {THREE.Mesh} cellTower The cell tower model.
     */
    var cellTower;

    /**
     * @method getCellTower
     * 
     * Gets a cell tower model.
     * 
     * @public
     * @return {THREE.Mesh} The cell tower model.
     */
    this.getCellTower = function() {

        return cellTower.clone();
    };

    /**
     * @method init
     * 
     * The init method for the Models.CellTowers class.
     * 
     * @public
     */
    this.init = function() {

        createCellTower();

        /**
         * @method createCellTower
         * 
         * Creates the cell tower model.
         * 
         * @private
         */
        function createCellTower() {

            var mtlLoader = new THREE.MTLLoader();
            mtlLoader.setPath('resources/display/models/cellTower/');
            mtlLoader.load('obj.mtl', function(materials) {

                materials.preload();

                var objLoader = new THREE.OBJLoader();
                objLoader.setMaterials(materials);
                objLoader.load('resources/display/models/cellTower/tinker.obj', function(object) {

                    object.scale.set(10, 10, 10);

                    var cellTowerWrapper = new THREE.Group();
                    cellTowerWrapper.add(object);
                    cellTowerWrapper.name = 'cellTower';

                    var groupedMesh = new THREE.Group();
                    groupedMesh.add(cellTowerWrapper);

                    cellTower = groupedMesh;
                });
            });
        }
    };
};

/**
 * @class Models.Fonts
 * 
 * This class will handle the fonts.
 * 
 * @public
 */
THREEDISPLAY.Models.Fonts = function() {

    /**
     * @private
     * @property {THREE.Font} basicFont The basic font.
     */
    var basicFont;

    /**
     * @method getBasicFont
     * 
     * Gets the basic font.
     * 
     * @public
     * @return {THREE.Font} The text font.
     */
    this.getBasicFont = function() {

        return basicFont;
    };

    /**
     * @method init
     * 
     * The init method for the Models.Fonts class.
     * 
     * @public
     */
    this.init = function() {

        var fontLoader = new THREE.FontLoader();
        fontLoader.load('/resources/display/fonts/helvetiker_regular.typeface.json', function(font) {

            basicFont = font;
        });
    };
};

/**
 * @class Models.RSUs
 * 
 * This class will handle the RSU models.
 * 
 * @public
 */
THREEDISPLAY.Models.RSUs = function() {

    /**
     * @private
     * @property {THREE.Mesh} rsu The RSU model.
     */
    var rsu;

    /**
     * @method getRSU
     * 
     * Gets a RSU model.
     * 
     * @public
     * @return {THREE.Mesh} The RSU model.
     */
    this.getRSU = function() {

        return rsu.clone();
    };

    /**
     * @method init
     * 
     * The init method for the Models.RSUs class.
     * 
     * @public
     */
    this.init = function() {

        createRSU();

        /**
         * @method createRSU
         * 
         * Creates the RSU model.
         * 
         * @private
         */
        function createRSU() {

            var mtlLoader = new THREE.MTLLoader();
            mtlLoader.setPath('resources/display/models/dsrcRSU/');
            mtlLoader.load('obj.mtl', function(materials) {

                materials.preload();

                var objLoader = new THREE.OBJLoader();
                objLoader.setMaterials(materials);
                objLoader.load('resources/display/models/dsrcRSU/tinker.obj', function(object) {

                    object.scale.set(6, 6, 6);

                    var rsuWrapper = new THREE.Group();
                    rsuWrapper.add(object);
                    rsuWrapper.name = 'RSU';

                    rsu = rsuWrapper;
                });
            });
        }
    };
};

/**
 * @class Models.Vehicles
 * 
 * This class will handle the vehicle models.
 * 
 * @public
 */
THREEDISPLAY.Models.Vehicles = function() {

    /**
     * @private
     * @property {THREEDISPLAY.Models.Vehicles.Vehicle} basicVehicle The basic vehicle model.
     */
    var basicVehicle;

    /**
     * @private
     * @property {THREEDISPLAY.Models.Vehicles.Vehicle} semiTruck The standard semi truck model.
     */
    var semiTruck;

    /**
     * @private
     * @property {THREEDISPLAY.Models.Vehicles.Vehicle} standardBus The standard bus model.
     */
    var standardBus;

    /**
     * @private
     * @property {THREEDISPLAY.Models.Vehicles.Vehicle} viperCar The viper car model.
     */
    var viperCar;

    /**
     * @method getBasicVehicle
     * 
     * Gets a basic vehicle model.
     * 
     * @public
     * @return {THREEDISPLAY.Models.Vehicles.Vehicle} The basic vehicle model.
     */
    this.getBasicVehicle = function() {

        var vehicle = basicVehicle.clone();
        return new Vehicle(vehicle, vehicle.getObjectByName('basicVehicle'));
    };

    /**
     * @method getBus
     * 
     * Gets a bus model.
     * 
     * @public
     * @return {THREEDISPLAY.Models.Vehicles.Vehicle} The bus model.
     */
    this.getBus = function() {

        var vehicle = standardBus.clone();
        return new Vehicle(vehicle, vehicle.getObjectByName('standardBus'));
    };

    /**
     * @method getCar
     * 
     * Gets a car model.
     * 
     * @public
     * @return {THREEDISPLAY.Models.Vehicles.Vehicle} The car model.
     */
    this.getCar = function() {

        var vehicle = viperCar.clone();
        return new Vehicle(vehicle, vehicle.getObjectByName('viperCar'));
    };

    /**
     * @method getSemiTruck
     * 
     * Gets a semi truck model.
     * 
     * @public
     * @return {THREEDISPLAY.Models.Vehicles.Vehicle} The semi truck model.
     */
    this.getSemiTruck = function() {

        var vehicle = semiTruck.clone();
        return new Vehicle(vehicle, vehicle.getObjectByName('semiTruck'));
    };

    /**
     * @method init
     * 
     * The init method for the Models.Vehicles class.
     * 
     * @public
     */
    this.init = function() {

        semiTruck = new THREE.Group();
        var semiTruckWrapper = new THREE.Group();
        semiTruckWrapper.name = 'semiTruck';
        semiTruck.add(semiTruckWrapper);

        createViperCar();
        createStandardBus();
        createBasicVehicle();
        createSemiTractor();
        createSemiTrailer();

        /**
         * @method createBasicVehicle
         * 
         * Creates the basic vehicle model.
         * 
         * @private
         */
        function createBasicVehicle() {

            var geomBox = new THREE.BoxGeometry(200, 200, 200);

            var matVehicle = new THREE.MeshBasicMaterial({
                color : new THREE.Color(0xB01C2E),
                side : THREE.FrontSide
            });

            var meshVehicle = new THREE.Mesh(geomBox, matVehicle);
            meshVehicle.translateZ(100);
            meshVehicle.name = 'basicVehicle';

            var groupedMesh = new THREE.Group();
            groupedMesh.add(meshVehicle);

            basicVehicle = groupedMesh;
        }

        /**
         * @method createSemiTractor
         * 
         * Creates the standard semi truck model's tractor.
         * 
         * @private
         */
        function createSemiTractor() {

            var mtlLoader = new THREE.MTLLoader();
            mtlLoader.setPath('resources/display/models/semiTruck/longnose/');
            mtlLoader.load('longnose_truck.mtl', function(materials) {

                materials.preload();

                var objLoader = new THREE.OBJLoader();
                objLoader.setMaterials(materials);
                objLoader.load('resources/display/models/semiTruck/longnose/longnose_truck.obj', function(object) {

                    object.rotateX(Math.PI * 0.5);
                    object.translateZ(350);
                    object.scale.set(2, 2, 2);

                    var semiTractorWrapper = new THREE.Group();
                    semiTractorWrapper.add(object);
                    semiTractorWrapper.name = 'semiTractor';

                    var semi = semiTruck.getObjectByName('semiTruck');
                    semi.add(semiTractorWrapper);
                });
            });
        }

        /**
         * @method createSemiTrailer
         * 
         * Creates the standard semi truck model's trailer.
         * 
         * @private
         */
        function createSemiTrailer() {

            var mtlLoader = new THREE.MTLLoader();
            mtlLoader.setPath('resources/display/models/semiTruck/longTrailer/');
            mtlLoader.load('obj.mtl', function(materials) {

                materials.preload();

                var objLoader = new THREE.OBJLoader();
                objLoader.setMaterials(materials);
                objLoader.load('resources/display/models/semiTruck/longTrailer/tinker.obj', function(object) {

                    object.translateY(-1168);
                    object.scale.set(2, 2, 2);

                    var semiTrailerWrapper = new THREE.Group();
                    semiTrailerWrapper.add(object);
                    semiTrailerWrapper.name = 'semiTrailer';

                    var semi = semiTruck.getObjectByName('semiTruck');
                    semi.add(semiTrailerWrapper);
                });
            });
        }

        /**
         * @method createStandardBus
         * 
         * Creates the standard bus model.
         * 
         * @private
         */
        function createStandardBus() {

            var mtlLoader = new THREE.MTLLoader();
            mtlLoader.setPath('resources/display/models/Senior-Midi/');
            mtlLoader.load('Senior%20Midi.mtl', function(materials) {

                materials.preload();

                var objLoader = new THREE.OBJLoader();
                objLoader.setMaterials(materials);
                objLoader.load('resources/display/models/Senior-Midi/Senior%20Midi.obj', function(object) {

                    object.rotateX(Math.PI * 1);
                    object.rotateY(Math.PI * 0.5);
                    object.rotateZ(Math.PI * -0.5);
                    object.translateX(-480);
                    object.scale.set(140, 140, 140);

                    var busWrapper = new THREE.Group();
                    busWrapper.add(object);
                    busWrapper.name = 'standardBus';

                    var groupedMesh = new THREE.Group();
                    groupedMesh.add(busWrapper);

                    standardBus = groupedMesh;
                });
            });
        }

        /**
         * @method createViperCar
         * 
         * Creates the viper car model.
         * 
         * @private
         */
        function createViperCar() {

            var objectLoader = new THREE.ObjectLoader();
            objectLoader.load('resources/display/models/dodge-viper-gts-threejs/viper.json', function(object) {

                var car = object.getObjectByName('car');
                car.rotateX(-Math.PI * 0.5);
                car.translateY(230);
                car.scale.set(28, 28, 28);

                object.getObjectByName('Body').material.color.setHex(0x888888);

                var carWrapper = new THREE.Group();
                carWrapper.add(object);
                carWrapper.name = 'viperCar';

                var groupedMesh = new THREE.Group();
                groupedMesh.add(carWrapper);

                viperCar = groupedMesh;
            });
        }
    };

    /**
     * @class Vehicle
     * 
     * The class that will handle the individual vehicle.
     * 
     * @public
     * @param {THREE.Mesh} gropuedMesh The grouped vehicle wrapper mesh.
     * @param {THREE.Mesh} vehicleMesh The vehicle mesh.
     */
    function Vehicle(groupedMesh, vehicleMesh) {

        var vehicle = vehicleMesh;

        this.mesh = groupedMesh;

        this.update = function(vehicleData, prevHeading) {

            if (prevHeading) {

                vehicle.rotateZ((prevHeading - vehicleData.heading) * (Math.PI / 180));
            }
            else {

                vehicle.rotateZ(-vehicleData.heading * (Math.PI / 180));
            }

            this.mesh.position.set(vehicleData.x, vehicleData.y, vehicleData.z);
        };
    }
};

/**
 * Creates the THREEDISPLAY.Static name space.
 */
THREEDISPLAY.Static = THREEDISPLAY.Static || {};

/**
 * Creates the THREEDISPLAY.Static.Models instance.
 */
THREEDISPLAY.Static.Models = new THREEDISPLAY.Models();
