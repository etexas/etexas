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
 * @class ETexas.view.display.DisplayContainerController
 * @extends Ext.app.ViewController
 * 
 * The {@link ETexas.view.display.DisplayContainer} controller. TODO ttevendale 3/1/2018 look into
 * Ext.data.NodeInterface methods (appendChild, etc...) and consider converting this class into
 * using them.
 * 
 * @author ttevendale
 */
Ext.define('ETexas.view.display.DisplayContainerController', {
    extend : 'Ext.app.ViewController',
    alias : 'controller.displaycontainer',

    /** @inheritdoc */
    init : function(view) {

        var me = this;
        var viewModel = this.getViewModel();

        /*
         * TODO: emyers - It is uncertain why the call to notify is necessary, but chained stores
         * are not loading without it at the moment. This should be revisited and, hopefully,
         * removed once the issue is resolved.
         */

        viewModel.notify();

        viewModel.get('cellTowerStore').on('datachanged', this._onCellTowerDataChanged, this);
        viewModel.get('detectorStore').on('datachanged', this._onDetectorDataChanged, this);
        viewModel.get('laneStore').on('datachanged', this._onLaneDataChanged, this);
        viewModel.get('signalStore').on('datachanged', this._onSignalStoreDataChanged, this);
        viewModel.get('standaloneDeviceStore').on('datachanged', this._onStandaloneDeviceDataChanged, this);
        viewModel.get('topographyFeatureStore').on('datachanged', this._onTopographyFeatureDataChanged, this);
        viewModel.get('vehicleStore').on('datachanged', this._onVehicleDataChanged, this);

        viewModel.bind('{selectedComposite}', function(selectedComposite) {

            me._updateCameraView(selectedComposite);

            if (selectedComposite) {

                viewModel.get('deviceProfileStore').load();
            }
            else {

                viewModel.get('deviceProfileStore').removeAll();
            }
        });

        viewModel.bind('{selectedExecution}', function(selectedExecution) {

            me._updatePropagationLossModel(viewModel.get('selectedComposite'));
        });

        this.callParent([ view ]);
    },

    /**
     * @method addDataToInfoPanel
     * 
     * Adds the data to the info panel based on the type and ID.
     * 
     * @public
     * @param {String} type The type of object to add to the info panel.
     * @param {String} simulationName the name of the simulation that the object is attached to (if
     * there is one).
     * @param {Number} id The ID of the object to add to the info panel.
     */
    addDataToInfoPanel : function(type, simulationName, id) {

        var infoBoxData = this.getViewModel().get('_infoBoxData');
        infoBoxData.type = type;
        infoBoxData.simulationName = simulationName;
        infoBoxData.id = id;

        if (type === 'Topography Feature') {

            this._addBuildingDataToInfoPanel(id);
        }
        else if (type === 'Cell Tower') {

            this._addCellTowerDataToInfoPanel(id);
        }
        else if (type === 'Detector') {

            this._addDetectorDataToInfoPanel(id);
        }
        else if (type === 'Lane') {

            this._addLaneDataToInfoPanel(simulationName, id);
        }
        else if (type === 'Signal') {

            this._addSignalDataToInfoPanel(simulationName, id);
        }
        else if (type === 'Standalone Device') {

            this._addStandaloneDeviceDataToInfoPanel(id);
        }
        else if (type === 'Vehicle') {

            this._addVehicleDataToInfoPanel(id);
        }
    },

    /**
     * @method onBuildingsCheckChange
     * 
     * Handles the event that is generated when the check box's state changes.
     * 
     * @protected
     * @param {Ext.menu.CheckItem} checkItem The menu check box.
     * @param {Boolean} checked True if the item is checked, false otherwise.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onBuildingsCheckChange : function(checkItem, checked, eOpts) {

        if (checked) {

            this.getViewModel().get('_ThreeDisplay').showBuildings();
        }
        else {

            this.getViewModel().get('_ThreeDisplay').hideBuildings();
        }
    },

    /**
     * @method onCellTowersCheckChange
     * 
     * Handles the event that is generated when the check box's state changes.
     * 
     * @protected
     * @param {Ext.menu.CheckItem} checkItem The menu check box.
     * @param {Boolean} checked True if the item is checked, false otherwise.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onCellTowersCheckChange : function(checkItem, checked, eOpts) {

        if (checked) {

            this.getViewModel().get('_ThreeDisplay').showCellTowers();
        }
        else {

            this.getViewModel().get('_ThreeDisplay').hideCellTowers();
        }
    },

    /**
     * @method onDetectorsCheckChange
     * 
     * Handles the event that is generated when the check box's state changes.
     * 
     * @protected
     * @param {Ext.menu.CheckItem} checkItem The menu check box.
     * @param {Boolean} checked True if the item is checked, false otherwise.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onDetectorsCheckChange : function(checkItem, checked, eOpts) {

        if (checked) {

            this.getViewModel().get('_ThreeDisplay').showDetectors();
        }
        else {

            this.getViewModel().get('_ThreeDisplay').hideDetectors();
        }
    },

    /**
     * @method onFloatingIdsCheckChange
     * 
     * Handles the event that is generated when the check box's state changes.
     * 
     * @protected
     * @param {Ext.menu.CheckItem} checkItem The menu check box.
     * @param {Boolean} checked True if the item is checked, false otherwise.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onFloatingIdsCheckChange : function(checkItem, checked, eOpts) {

        if (checked) {
            this.getViewModel().get('_ThreeDisplay').showFloatingIds();
        }
        else {
            this.getViewModel().get('_ThreeDisplay').hideFloatingIds();
        }
    },

    /**
     * @method onHudDisplayCheckChange
     * 
     * Handles the event that is generated when the check box's state changes.
     * 
     * @protected
     * @param {Ext.menu.CheckItem} checkItem The menu check box.
     * @param {Boolean} checked True if the item is checked, false otherwise.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onHudDisplayCheckChange : function(checkItem, checked, eOpts) {

        if (checked) {

            this.getViewModel().get('_ThreeDisplay').showHud();
        }
        else {

            this.getViewModel().get('_ThreeDisplay').hideHud();
        }
    },

    /**
     * @method onLanesCheckChange
     * 
     * Handles the event that is generated when the check box's state changes.
     * 
     * @protected
     * @param {Ext.menu.CheckItem} checkItem The menu check box.
     * @param {Boolean} checked True if the item is checked, false otherwise.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onLanesCheckChange : function(checkItem, checked, eOpts) {

        if (checked) {

            this.getViewModel().get('_ThreeDisplay').showLanes();
        }
        else {

            this.getViewModel().get('_ThreeDisplay').hideLanes();
        }
    },

    /**
     * @method onSignalsCheckChange
     * 
     * Handles the event that is generated when the check box's state changes.
     * 
     * @protected
     * @param {Ext.menu.CheckItem} checkItem The menu check box.
     * @param {Boolean} checked True if the item is checked, false otherwise.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onSignalsCheckChange : function(checkItem, checked, eOpts) {

        if (checked) {

            this.getViewModel().get('_ThreeDisplay').showSignals();
        }
        else {

            this.getViewModel().get('_ThreeDisplay').hideSignals();
        }
    },

    /**
     * @method onStandaloneDevicesCheckChange
     * 
     * Handles the event that is generated when the check box's state changes.
     * 
     * @protected
     * @param {Ext.menu.CheckItem} checkItem The menu check box.
     * @param {Boolean} checked True if the item is checked, false otherwise.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onStandaloneDevicesCheckChange : function(checkItem, checked, eOpts) {

        if (checked) {

            this.getViewModel().get('_ThreeDisplay').showStandaloneDevices();
        }
        else {

            this.getViewModel().get('_ThreeDisplay').hideStandaloneDevices();
        }
    },

    /**
     * @method onTreePanelItemClick
     * 
     * Handles the event that is generated when the tree panel's item is clicked.
     * 
     * @protected
     * @param {Ext.view.View} view The tree view that the event was fired from.
     * @param {Ext.data.Model} record The record that belongs to the item.
     * @param {HTMLElement} item The item's element.
     * @param {Number} index The item's index.
     * @param {Ext.event.Event} e The raw event object.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onTreePanelItemClick : function(view, record, item, index, e, eOpts) {

        var node = record;

        while (node.parentNode) {

            if (node.data.nodeType === 'Message') {

                this.getViewModel().get('_ThreeDisplay').clearMessages();

                if (node.data.ownerDeviceAddress === node.data.originAddress) {
                    if (node.data.destinationAddress === 'Broadcast') {

                        this.getViewModel().get('_ThreeDisplay').addBroadcastMessage(node.data.originAddress, node.data.dsrcChannel);
                    }
                    else {

                        this.getViewModel().get('_ThreeDisplay').addUnicastMessage(node.data.originAddress, node.data.destinationAddress, node.data.dsrcChannel);
                    }
                }
                else {

                    this.getViewModel().get('_ThreeDisplay').addUnicastMessage(node.data.originAddress, node.data.ownerDeviceAddress, node.data.dsrcChannel);
                }
            }

            node = node.parentNode;
        }
    },

    /**
     * @method onVehiclesCheckChange
     * 
     * Handles the event that is generated when the check box's state changes.
     * 
     * @protected
     * @param {Ext.menu.CheckItem} checkItem The menu check box.
     * @param {Boolean} checked True if the item is checked, false otherwise.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onVehiclesCheckChange : function(checkItem, checked, eOpts) {

        if (checked) {

            this.getViewModel().get('_ThreeDisplay').showVehicles();
        }
        else {

            this.getViewModel().get('_ThreeDisplay').hideVehicles();
        }
    },

    /**
     * @method onViewRendered
     * 
     * Handles the event that is generated when the application renders the view.
     * 
     * @protected
     * @param {Ext.component.Component} component The view that rendered.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onViewRendered : function(component, eOpts) {

        this.getViewModel().get('_ThreeDisplay').init(component, component.lookupReference('displayPanel'));
    },

    /**
     * @method resetThreeJS
     * 
     * Resets the ThreeJS instance.
     * 
     * @public
     */
    resetThreeJS : function() {

        this.getViewModel().get('_ThreeDisplay').reset();
    },

    /**
     * @method _onCellTowerDataChanged
     * 
     * Handles the event that is generated when the store's data changes.
     * 
     * @private
     * @param {Ext.store.Store} store The store that had its data changed.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    _onCellTowerDataChanged : function(store, eOpts) {

        var viewModel = this.getViewModel();
        var cellTowerData = [];

        store.each(function(record) {

            cellTowerData.push(record.data);
        });

        viewModel.get('_ThreeDisplay').addCellTowers(cellTowerData);

        var infoBoxData = viewModel.get('_infoBoxData');
        if (infoBoxData.type === 'Cell Tower') {

            this._addCellTowerDataToInfoPanel(infoBoxData.id);
        }
    },

    /**
     * @method _onDetectorDataChanged
     * 
     * Handles the event that is generated when the store's data changes.
     * 
     * @private
     * @param {Ext.store.Store} store The store that had its data changed.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    _onDetectorDataChanged : function(store, eOpts) {

        var viewModel = this.getViewModel();
        var detectorData = [];

        store.each(function(record) {

            detectorData.push(record.data);
        });

        viewModel.get('_ThreeDisplay').addDetectors(detectorData);

        var infoBoxData = viewModel.get('_infoBoxData');
        if (infoBoxData.type === 'Detector') {

            this._addDetectorDataToInfoPanel(infoBoxData.id);
        }
    },

    /**
     * @method _onLaneDataChanged
     * 
     * Handles the event that is generated when the store's data changes.
     * 
     * @private
     * @param {Ext.store.Store} store The store that had its data changed.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    _onLaneDataChanged : function(store, eOpts) {

        var viewModel = this.getViewModel();
        var laneData = [];

        store.each(function(record) {

            laneData.push(record.data);
        });

        viewModel.get('_ThreeDisplay').addLanes(laneData);

        var infoBoxData = viewModel.get('_infoBoxData');
        if (infoBoxData.type === 'Lane') {

            this._addLaneDataToInfoPanel(infoBoxData.simulationName, infoBoxData.id);
        }
    },

    /**
     * @method _onSignalStoreDataChanged
     * 
     * Handles the event that is generated when the store's data changes.
     * 
     * @private
     * @param {Ext.store.Store} store The store that had its data changed.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    _onSignalStoreDataChanged : function(store, eOpts) {

        var viewModel = this.getViewModel();
        var signalData = [];

        store.each(function(record) {

            signalData.push(record.data);
        });

        viewModel.get('_ThreeDisplay').addSignals(signalData);

        var infoBoxData = viewModel.get('_infoBoxData');
        if (infoBoxData.type === 'Signal') {

            this._addSignalDataToInfoPanel(infoBoxData.simulationName, infoBoxData.id);
        }
    },

    /**
     * @method _onStandaloneDeviceDataChanged
     * 
     * Handles the event that is generated when the store's data changes.
     * 
     * @private
     * @param {Ext.store.Store} store The store that had its data changed.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    _onStandaloneDeviceDataChanged : function(store, eOpts) {

        var viewModel = this.getViewModel();
        var standaloneDeviceData = [];

        store.each(function(record) {

            standaloneDeviceData.push(record.data);
        });

        viewModel.get('_ThreeDisplay').addStandaloneDevices(standaloneDeviceData);

        var infoBoxData = viewModel.get('_infoBoxData');
        if (infoBoxData.type === 'Standalone Device') {

            this._addStandaloneDeviceDataToInfoPanel(infoBoxData.id);
        }
    },

    /**
     * @method _onTopographyFeatureDataChanged
     * 
     * Handles the event that is generated when the store's data changes.
     * 
     * @private
     * @param {Ext.store.Store} store The store that had its data changed.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    _onTopographyFeatureDataChanged : function(store, eOpts) {

        var viewModel = this.getViewModel();
        var featureData = [];

        store.each(function(record) {

            featureData.push(record.data);
        });

        viewModel.get('_ThreeDisplay').addTopographyFeatures(featureData);

        var infoBoxData = viewModel.get('_infoBoxData');
        if (infoBoxData.type === 'Topography Feature') {

            this._addBuildingDataToInfoPanel(infoBoxData.id);
        }
    },

    /**
     * @method _onVehicleDataChanged
     * 
     * Handles the event that is generated when the store's data changes.
     * 
     * @private
     * @param {Ext.store.Store} store The store that had its data changed.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    _onVehicleDataChanged : function(store, eOpts) {

        var viewModel = this.getViewModel();
        var vehicleData = [];

        store.each(function(record) {

            vehicleData.push(record.data);
        });

        viewModel.get('_ThreeDisplay').updateVehicles(vehicleData);
        viewModel.get('_ThreeDisplay').clearMessages();

        var infoBoxData = viewModel.get('_infoBoxData');
        if (infoBoxData.type === 'Vehicle') {

            this._addVehicleDataToInfoPanel(infoBoxData.id);
        }
    },

    /**
     * @method _addBuildingDataToInfoPanel
     * 
     * Adds building data to the info panel (currently replaces everything else in the info panel).
     * 
     * @private
     * @param {Number} buildingId The ID of the building to add to the info panel.
     */
    _addBuildingDataToInfoPanel : function(buildingId) {

        var topographyFeatureStore = this.getViewModel().get('topographyFeatureStore');
        var building = topographyFeatureStore.findRecord('id', buildingId, false, false, true);

        if (!building) {

            this._clearInfoBox();
            return;
        }

        building = building.getData();

        this.getView().lookupReference('displayInfoPanel').setRootNode({
            expanded : true,
            children : [ {
                text : 'BUILDING ' + building.id,
                expanded : true,
                children : [ {
                    text : 'Name : ' + building.name,
                    leaf : true
                }, {
                    text : 'X (cm) : ' + building.x,
                    leaf : true
                }, {
                    text : 'Y (cm): ' + building.y,
                    leaf : true
                }, {
                    text : 'Width : ' + building.width,
                    leaf : true
                }, {
                    text : 'Length : ' + building.length,
                    leaf : true
                }, {
                    text : 'Height : ' + building.height,
                    leaf : true
                } ]
            } ]
        });
    },

    /**
     * @method _addCellTowerDataToInfoPanel
     * 
     * Adds cell tower data to the info panel (currently replaces everything else in the info
     * panel).
     * 
     * @private
     * @param {Number} cellTowerId The ID of the cell tower to add to the info panel.
     */
    _addCellTowerDataToInfoPanel : function(cellTowerId) {

        var cellTowerStore = this.getViewModel().get('cellTowerStore');
        var cellTower = cellTowerStore.findRecord('id', cellTowerId, false, false, true);

        if (!cellTower) {

            this._clearInfoBox();
            return;
        }

        cellTower = cellTower.getData();

        this.getView().lookupReference('displayInfoPanel').setRootNode({
            expanded : true,
            children : [ {
                text : 'CELL TOWER ' + cellTower.id,
                expanded : true,
                children : [ {
                    text : 'Provider : ' + cellTower.provider,
                    leaf : true
                }, {
                    text : 'X (cm): ' + cellTower.x,
                    leaf : true
                }, {
                    text : 'Y (cm): ' + cellTower.y,
                    leaf : true
                }, {
                    text : 'Z (cm): ' + cellTower.z,
                    leaf : true
                } ]
            } ]
        });
    },

    /**
     * @method _addDetectorDataToInfoPanel
     * 
     * Adds detector data to the info panel (currently replaces everything else in the info panel).
     * 
     * @private
     * @param {Number} detectorId The ID of the detector to add to the info panel.
     */
    _addDetectorDataToInfoPanel : function(detectorId) {

        var i = 0;
        var detectorStore = this.getViewModel().get('detectorStore');
        var detector = detectorStore.findRecord('detectorID', detectorId, false, false, true);

        if (!detector) {

            this._clearInfoBox();
            return;
        }

        detector = detector.getData();

        var laneIdNodes = [];

        for (i = 0; i < detector.laneIDs.length; i++) {

            laneIdNodes.push({

                text : detector.laneIDs[i],
                leaf : true
            });
        }

        var capabilityNodes = [];

        if (detector.lengthDetectCap) {

            capabilityNodes.push({

                text : 'Length Detection',
                leaf : true
            });
        }
        else if (detector.presenceDetectCap) {

            capabilityNodes.push({

                text : 'Presence Detection',
                leaf : true
            });
        }
        else if (detector.pulseDetectCap) {

            capabilityNodes.push({

                text : 'Pulse Detection',
                leaf : true
            });
        }
        else if (detector.speedDetectCap) {

            capabilityNodes.push({

                text : 'Speed Detection',
                leaf : true
            });
        }

        var areaNodes = [];

        for (i = 0; i < detector.area.n; i++) {

            areaNodes.push({

                text : '(' + detector.area.x[i] + ', ' + detector.area.y[i] + ')',
                leaf : true
            });
        }

        this.getView().lookupReference('displayInfoPanel').setRootNode({
            expanded : true,
            children : [ {
                text : 'DETECTOR ' + detector.detectorID,
                expanded : true,
                children : [ {
                    text : 'Lanes',
                    expanded : true,
                    children : laneIdNodes
                }, {
                    text : 'Capabilities',
                    expanded : true,
                    children : capabilityNodes
                }, {
                    text : 'Boundary Points (x, y) (cm)',
                    expanded : true,
                    children : areaNodes
                } ]
            } ]
        });
    },

    /**
     * @method _addLaneDataToInfoPanel
     * 
     * Adds lane data to the info panel (currently replaces everything else in the info panel).
     * 
     * @private
     * @param {String} simulationName the name of the simulation that the lane is attached to.
     * @param {Number} laneId The ID of the lane to add to the info panel.
     */
    _addLaneDataToInfoPanel : function(simulationName, laneId) {

        var i = 0;
        var laneStore = this.getViewModel().get('laneStore');

        // queryBy returns a list, but there should only be one match
        var lane = laneStore.queryBy(function(record, id) {

            var data = record.getData();
            return data.simulationName === simulationName && data.laneId === laneId;
        });

        if (lane.length === 1) {

            lane = lane.items[0];
        }
        else {

            this._clearInfoBox();
            return;
        }

        lane = lane.getData();

        var movements = lane.laneMovements.entry;

        var movementNodes = [];

        for (i = 0; i < movements.length; i++) {

            movementNodes.push({

                text : movements[i].value.movement,
                leaf : true
            });
        }

        var geometry = lane.laneGeomList;

        var laneNodes = [];

        for (i = 0; i < geometry.length; i++) {

            var node = geometry[i];
            laneNodes.push({

                text : '(' + node.x + ', ' + node.y + ', ' + node.z + ', ' + node.width + ')',
                leaf : true
            });
        }

        this.getView().lookupReference('displayInfoPanel').setRootNode({
            expanded : true,
            children : [ {
                text : 'LANE ' + lane.laneId,
                expanded : true,
                children : [ {
                    text : 'Approach : ' + lane.approachId,
                    leaf : true
                }, {
                    text : 'Type : ' + lane.type,
                    leaf : true
                }, {
                    text : 'Movements',
                    expanded : true,
                    children : movementNodes
                }, {
                    text : 'Speed Limit (m/s) : ' + Ext.util.Format.number(lane.speedLimitInMetersPerSecond, '###,##0.0#'),
                    leaf : true
                }, {
                    text : 'Nodes (x, y, z, w) (cm)',
                    expanded : true,
                    children : laneNodes
                } ]

            } ]
        });
    },

    /**
     * @method _addSignalDataToInfoPanel
     * 
     * Adds signal data to the info panel (currently replaces everything else in the info panel).
     * 
     * @private
     * @param {String} simulationName the name of the simulation that the lane is attached to.
     * @param {Number} laneId The ID of the lane that the signals will be retrieved from to add to
     * the info panel.
     */
    _addSignalDataToInfoPanel : function(simulationName, laneId) {

        var signalStore = this.getViewModel().get('signalStore');
        var signals = signalStore.queryBy(function(record, id) {

            var data = record.getData();
            return data.simulationName === simulationName && data.laneId === laneId;
        });

        var signalNodes = [];

        for (var i = 0; i < signals.count(); i++) {

            var signal = signals.getAt(i).getData();

            signalNodes.push({
                text : 'SIGNAL',
                expanded : true,
                children : [ {
                    text : 'Lane : ' + signal.laneId,
                    leaf : true
                }, {
                    text : 'Color : ' + signal.colorIndication,
                    leaf : true
                }, {
                    text : 'State : ' + signal.stateIndication,
                    leaf : true
                }, {
                    text : 'Type : ' + signal.typeIndication,
                    leaf : true
                }, {
                    text : 'Time to Change (s) : ' + signal.timeToChange,
                    leaf : true
                } ]
            });
        }

        this.getView().lookupReference('displayInfoPanel').setRootNode({
            expanded : true,
            children : signalNodes
        });
    },

    /**
     * @method _addStandaloneDeviceDataToInfoPanel
     * 
     * Adds standalone device data to the info panel (currently replaces everything else in the info
     * panel).
     * 
     * @private
     * @param {Number} deviceId The ID of the device to add to the info panel.
     */
    _addStandaloneDeviceDataToInfoPanel : function(deviceId) {

        var me = this;
        var standaloneDeviceStore = this.getViewModel().get('standaloneDeviceStore');
        var device = standaloneDeviceStore.findRecord('deviceMac', deviceId, false, false, true);

        if (!device) {

            this._clearInfoBox();
            return;
        }

        var selectedComposite = this.getViewModel().get('selectedComposite');

        var applicationStore = Ext.create('ETexas.store.ApplicationStore', {
            proxy : {
                type : 'ajax',
                url : ETexas.util.UrlProvider.getDeviceApplicationsUrl(selectedComposite, device)
            }
        });

        device = device.getData();

        applicationStore.load({
            scope : this,
            callback : function(records, operation, success) {

                var txMessageNodes = [];
                var rxMessageNodes = [];
                me._getMessageNodes(device, txMessageNodes, rxMessageNodes);

                this.getView().lookupReference('displayInfoPanel').setRootNode({
                    expanded : true,
                    children : [ {
                        text : 'DEVICE ' + device.deviceMac,
                        expanded : true,
                        children : [ {
                            text : 'Type : ' + device.deviceType,
                            leaf : true
                        }, {
                            text : 'X (cm): ' + device.x,
                            leaf : true
                        }, {
                            text : 'Y (cm): ' + device.y,
                            leaf : true
                        }, {
                            text : 'Z (cm): ' + device.z,
                            leaf : true
                        }, {
                            text : 'Applications',
                            expanded : true,
                            children : me._getApplicationNodes(getApplications(records))
                        }, {
                            text : 'Tx Messages',
                            expanded : true,
                            children : txMessageNodes
                        }, {
                            text : 'Rx Messages',
                            expanded : true,
                            children : rxMessageNodes
                        } ]
                    } ]

                });
            }
        });

        /**
         * Gets an array of applications from records.
         * 
         * @param records The records that contain the applications
         * @returns {Object[]} The applications array.
         */
        function getApplications(records) {

            var applications = [];
            for (var i = 0; i < records.length; i++) {

                applications.push(records[i].getData());
            }

            return applications;
        }
    },

    /**
     * @method _addVehicleDataToInfoPanel
     * 
     * Adds vehicle data to the info panel (currently replaces everything else in the info panel).
     * 
     * @private
     * @param {Number} vehicleId The ID of the vehicle to add to the info panel.
     */
    _addVehicleDataToInfoPanel : function(vehicleId) {

        var me = this;
        var vehicleStore = this.getViewModel().get('vehicleStore');

        var vehicleIndex = vehicleStore.findBy(function(record, id) {

            var data = record.getData();
            return data.vehicleID === vehicleId;
        });

        var vehicle = vehicleStore.getAt(vehicleIndex);
        if (vehicleIndex === -1) {

            this._clearInfoBox();
            return;
        }

        vehicle = vehicle.getData();

        this.getView().lookupReference('displayInfoPanel').setRootNode({
            expanded : true,
            children : [ {
                text : 'VEHICLE ' + vehicle.vehicleID,
                expanded : true,
                children : [ {
                    text : 'Lane : ' + vehicle.laneID,
                    leaf : true
                }, {
                    text : 'Type : ' + vehicle.type,
                    leaf : true
                }, {
                    text : 'X (cm) : ' + vehicle.x,
                    leaf : true
                }, {
                    text : 'Y (cm) : ' + vehicle.y,
                    leaf : true
                }, {
                    text : 'Speed (m/s) : ' + vehicle.speed,
                    leaf : true
                }, {
                    text : 'Devices',
                    expanded : true,
                    children : me._getDeviceNodes(vehicle.devices)
                } ]
            } ]
        });
    },

    /**
     * @method _getDeviceNodes
     * 
     * Gets device nodes from device data, which will be used with the tree panel.
     * 
     * @private
     * @param {Ext.data.Model} devices The device data to convert into nodes.
     * @returns {Ext.data.NodeInterface} The application nodes.
     */
    _getDeviceNodes : function(devices) {

        var me = this;
        var deviceNodes = [];

        if (!devices) {

            return deviceNodes;
        }

        for (var i = 0; i < devices.length; i++) {

            var device = devices[i];
            var txMessageNodes = [];
            var rxMessageNodes = [];

            this._getMessageNodes(device, txMessageNodes, rxMessageNodes);

            deviceNodes.push({
                text : 'DEVICE ' + device.deviceMac,
                expanded : true,
                children : [ {
                    text : 'Device Profile ID : ' + device.deviceRuleId,
                    leaf : true
                }, {
                    text : 'Type : ' + device.deviceType,
                    leaf : true
                }, {
                    text : 'Applications',
                    expanded : true,
                    children : this._getApplicationNodes(getApplications(device))
                }, {
                    text : 'Tx Messages',
                    expanded : true,
                    children : txMessageNodes
                }, {
                    text : 'Rx Messages',
                    expanded : true,
                    children : rxMessageNodes
                } ]
            });
        }

        return deviceNodes;

        /**
         * Gets an array of applications from a device.
         * 
         * @param device The device that contains the applications
         * @returns {Object[]} The applications array.
         */
        function getApplications(device) {

            var deviceProfile;
            var deviceProfileStore = me.getViewModel().get('deviceProfileStore');
            deviceProfileStore.each(function(record) {

                if (device.deviceRuleId === record.getData().id) {

                    deviceProfile = record.getData();
                    return false;
                }
            });

            if (deviceProfile) {

                return deviceProfile.applications;
            }
            else {

                return [];
            }
        }
    },

    /**
     * @method _getApplicationNodes
     * 
     * Gets application nodes from application data, which will be used with the tree panel.
     * 
     * @private
     * @param {Ext.data.Model} applications The application data to convert into nodes.
     * @returns {Ext.data.NodeInterface} The application nodes.
     */
    _getApplicationNodes : function(applications) {

        var applicationNodes = [];

        if (!applications) {

            return applicationNodes;
        }

        for (var i = 0; i < applications.length; i++) {

            var application = applications[i];

            applicationNodes.push({
                text : application.name,
                expanded : false,
                children : [ {
                    text : 'Type : ' + application.type,
                    leaf : true
                } ]
            });
        }

        return applicationNodes;

    },

    /**
     * @method _getMessageNodes
     * 
     * Gets message nodes from message data, which will be used with the tree panel.
     * 
     * @private
     * @param {Ext.data.Model} messages The message data to convert into nodes.
     * @param {Ext.data.NodeInterface[]} txMessageNodes The Tx nodes array that will be populated by
     * this method.
     * @param {Ext.data.NodeInterface[]} rxMessageNodes The Rx nodes array that will be populated by
     * this method.
     */
    _getMessageNodes : function(device, txMessageNodes, rxMessageNodes) {

        var messages = [];
        var messageStore = this.getViewModel().get('messageStore');
        messageStore.each(function(record) {

            if (device.deviceMac === record.getData().ownerDeviceAddress) {

                messages.push(record.getData());
            }
        });

        for (var i = 0; i < messages.length; i++) {

            var message = messages[i];
            if (message.type === 'Tx') {

                txMessageNodes.push(createMessageNode(message));
            }
            else if (message.type === 'Rx') {

                rxMessageNodes.push(createMessageNode(message));
            }
        }

        /**
         * Creates an individual message node.
         * 
         * @param message The message data to convert into a message node.
         * @returns {Ext.data.NodeInterface} The message node.
         */
        function createMessageNode(message) {

            return {
                text : 'MESSAGE ' + message.messageId,
                nodeType : 'Message',
                ownerDeviceAddress : message.ownerDeviceAddress,
                originAddress : message.originAddress,
                destinationAddress : message.destinationAddress,
                dsrcChannel : message.channel,
                expanded : false,
                children : [ {
                    text : 'Origin Address : ' + message.originAddress,
                    leaf : true
                }, {
                    text : 'Destination Address : ' + message.destinationAddress,
                    leaf : true
                }, {
                    text : 'Channel : ' + (message.channel || 'N/A'),
                    leaf : true
                }, {
                    text : 'Type : ' + message.messageType,
                    leaf : true
                }, {
                    text : 'Data',
                    expanded : false,
                    children : [ {
                        text : message.message,
                        leaf : true
                    } ]
                } ]
            };
        }
    },

    /**
     * @method _clearInfoBox
     * 
     * Clears the info box and sets the displayInfoPanel to show nothing.
     * 
     * @private
     */
    _clearInfoBox : function() {

        var infoBoxData = this.getViewModel().get('_infoBoxData');
        infoBoxData.type = null;
        infoBoxData.simulationName = null;
        infoBoxData.id = null;

        this.getView().lookupReference('displayInfoPanel').setRootNode({});
    },

    /**
     * @method _updateCameraView
     * 
     * Updates the locations that the camera can view.
     * 
     * @private
     * @param {ETexas.model.CompositeModel} [selectedComposite] The selected composite.
     */
    _updateCameraView : function(selectedComposite) {

        if (!selectedComposite) {

            return;
        }

        var view = this.getView();
        var viewModel = this.getViewModel();
        var simulations = selectedComposite.getSimulations();

        var cameraViewButton = view.lookupReference('cameraViewButton');
        var cameraViewMenu = [];
        var idNum = 1;

        simulations.each(function(record) {

            var data = record.getData();
            cameraViewMenu.push({
                reference : 'simulationItem' + idNum,
                id : Ext.id(null, 'simulation-item-' + idNum + '-'),
                handler : function() {

                    viewModel.get('_ThreeDisplay').changeCameraLocation(data.x, data.y);
                },
                text : data.name
            });
            idNum++;
        });

        cameraViewButton.setMenu(cameraViewMenu);
    },

    /**
     * @method _updatePropagationLossModel
     * 
     * Retrieves and updates the propagation loss model for the composite.
     * 
     * @private
     * @param {ETexas.model.CompositeModel} [selectedComposite] The selected composite.
     */
    _updatePropagationLossModel : function(selectedComposite) {

        if (!selectedComposite) {

            return;
        }
        var me = this;

        Ext.Ajax.request({
            scope : this,
            method : 'GET',
            url : ETexas.util.UrlProvider.getCompositeOptionsUrl(selectedComposite),
            success : function(response, opts) {

                var compositeOptions = Ext.JSON.decode(response.responseText);
                me.getViewModel().get('_ThreeDisplay').setPropagationLossModel(compositeOptions.propagationLossModel);
            }
        });
    }
});
