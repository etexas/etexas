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
 * @class ETexas.view.lanemapping.LaneMappingFormController
 * @extends ETexas.view.model.ModelFormController
 * 
 * The {@link ETexas.view.lanemapping.LaneMappingForm} controller.
 * 
 * @abstract
 * @author emyers
 */
Ext.define('ETexas.view.lanemapping.LaneMappingFormController', {
    extend : 'ETexas.view.model.ModelFormController',

    requires : [ 'ETexas.view.lane.LaneGrid', 'ETexas.view.window.BasicWindow' ],

    /** @inheritdoc */
    init : function(view) {

        var laneStore = this.getViewModel().get('laneStore');
        var selectedComposite = this.getViewModel().get('selectedComposite');

        Ext.Ajax.request({
            scope : this,
            method : 'GET',
            url : ETexas.util.UrlProvider.getLanesUrl(selectedComposite),
            success : function(response, opts) {

                var lanes = [];
                var laneMap = Ext.JSON.decode(response.responseText);

                Ext.Object.each(laneMap, function(key, value, object) {

                    var simulation = selectedComposite.getSimulations().getById(key);

                    Ext.Array.forEach(value, function(item, index, allItems) {

                        item.simulationName = simulation.get('name');
                        lanes.push(item);

                    }, this);
                });

                laneStore.setData(lanes);
            }
        });

        view.lookupReference('sourceLaneBox').validator = Ext.bind(this.validateSourceLaneField, this);
        view.lookupReference('sourceSimulationBox').validator = Ext.bind(this.validateSourceSimulationField, this);
        view.lookupReference('targetLaneBox').validator = Ext.bind(this.validateTargetLaneField, this);
        view.lookupReference('targetSimulationBox').validator = Ext.bind(this.validateTargetSimulationField, this);
    },

    /**
     * @method loadSourceLanes
     * 
     * Loads the source lanes for the specified simulation.
     * 
     * @protected
     * @param {ETexas.model.SimulationModel} [simulation] The target simulation.
     */
    loadSourceLanes : function(simulation) {

        this.lookupReference('sourceLaneBox').clearValue();
        var sourceLaneStore = this.getViewModel().get('sourceLaneStore');
        sourceLaneStore.removeAll();

        if (simulation) {

            var laneStore = this.getViewModel().get('laneStore');
            laneStore.each(function(lane) {

                if (lane.get('simulationName') === simulation.get('name') && lane.get('type') === 'OUTBOUND') {

                    sourceLaneStore.add(lane);
                }
            });
        }
    },

    /**
     * @method loadTargetLanes
     * 
     * Loads the target lanes for the specified simulation.
     * 
     * @protected
     * @param {ETexas.model.SimulationModel} [simulation] The target simulation.
     */
    loadTargetLanes : function(simulation) {

        this.lookupReference('targetLaneBox').clearValue();
        var targetLaneStore = this.getViewModel().get('targetLaneStore');
        targetLaneStore.removeAll();

        if (simulation) {

            var laneStore = this.getViewModel().get('laneStore');
            laneStore.each(function(lane) {

                if (lane.get('simulationName') === simulation.get('name') && lane.get('type') === 'INBOUND') {

                    targetLaneStore.add(lane);
                }
            });
        }
    },

    /**
     * @method onSourceLaneButtonClicked
     * 
     * Handles the event that is generated when the user clicks the source Show Lanes button.
     * 
     * @protected
     * @param {Ext.button.Button} button The clicked button.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onSourceLaneButtonClicked : function(button, e, eOpts) {

        var window = this.getView().add({
            xtype : 'basicwindow',
            reference : 'sourceLanesWindow',
            id : Ext.id(null, 'source-lanes-window-'),
            title : 'Source Lane Information',
            layout : 'fit',
            width : 756,
            height : 512,
            items : [ {
                xtype : 'lanegrid',
                reference : 'sourceLaneGrid',
                id : Ext.id(null, 'source-lane-grid-'),
                bind : {
                    store : '{sourceLaneStore}',
                    selection : '{selectedSourceLane}'
                }
            } ]

        });

        window.show();
    },

    /**
     * @method onSourceSimulationChange
     * 
     * Handles the event that is generated when the user selects a source simulation.
     * 
     * @protected
     * @param {Ext.form.field.Field} field The changed field.
     * @param {Object} newValue The new value.
     * @param {Object} oldValue The original value.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onSourceSimulationChange : function(field, newValue, oldValue, eOpts) {

        this.loadSourceLanes(field.getSelection());
    },

    /**
     * @method onTargetLaneButtonClicked
     * 
     * Handles the event that is generated when the user clicks the taret Show Lanes button.
     * 
     * @protected
     * @param {Ext.button.Button} button The clicked button.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onTargetLaneButtonClicked : function(button, e, eOpts) {

        var window = this.getView().add({
            xtype : 'basicwindow',
            reference : 'targetLanesWindow',
            id : Ext.id(null, 'target-lanes-window-'),
            title : 'Target Lane Information',
            layout : 'fit',
            width : 756,
            height : 512,
            items : [ {
                xtype : 'lanegrid',
                reference : 'targetLaneGrid',
                id : Ext.id(null, 'target-lane-grid-'),
                bind : {
                    store : '{targetLaneStore}',
                    selection : '{selectedTargetLane}'
                }
            } ]

        });

        window.show();
    },

    /**
     * @method onTargetSimulationChange
     * 
     * Handles the event that is generated when the user selects a target simulation.
     * 
     * @protected
     * @param {Ext.form.field.Field} field The changed field.
     * @param {Object} newValue The new value.
     * @param {Object} oldValue The original value.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onTargetSimulationChange : function(field, newValue, oldValue, eOpts) {

        this.loadTargetLanes(field.getSelection());
    },

    /**
     * @method validateSourceLaneField
     * 
     * Validates the current value of the source lane field.
     * 
     * @protected
     * @param {String} value The current field value.
     * @param {ETexas.model.LaneMappingModel} [laneMapping] The selected lane mapping.
     * @return {Boolean/String} True if the current field value is valid, otherwise a string
     * validation message is returned.
     */
    validateSourceLaneField : function(value, laneMapping) {

        var sourceLane = this.lookupReference('sourceLaneBox').getValue();

        if (!sourceLane) {

            return 'A valid source lane is required.';
        }

        var sourceSimulation = this.lookupReference('sourceSimulationBox').getValue();
        var mappingStore = this.getViewModel().get('laneMappingStore');
        var matches = mappingStore.queryBy(function(record, id) {

            if (!laneMapping || laneMapping !== record) {

                return record.get('sourceSimulation') === sourceSimulation && record.get('sourceLane') === sourceLane;
            }

        }, this);

        if (matches.length !== 0) {

            return 'A lane mapping from the selected source simulation and lane already exists.';
        }

        return true;
    },

    /**
     * @method validateSourceSimulationField
     * 
     * Validates the current value of the source simulation field.
     * 
     * @protected
     * @param {String} value The current field value.
     * @return {Boolean/String} True if the current field value is valid, otherwise a string
     * validation message is returned.
     */
    validateSourceSimulationField : function(value) {

        var source = this.lookupReference('sourceSimulationBox').getValue();

        if (!source) {

            return 'A valid source simulation is required.';
        }

        var targetBox = this.lookupReference('targetSimulationBox');

        if (targetBox.getValue()) {

            if (targetBox.getValue() === source) {

                return 'The source and target simulations for each lane mapping must be unique.';
            }

            targetBox.clearInvalid();
        }

        return true;
    },

    /**
     * @method validateTargetLaneField
     * 
     * Validates the current value of the target lane field.
     * 
     * @protected
     * @param {String} value The current field value.
     * @param {ETexas.model.LaneMappingModel} [laneMapping] The selected lane mapping.
     * @return {Boolean/String} True if the current field value is valid, otherwise a string
     * validation message is returned.
     */
    validateTargetLaneField : function(value, laneMapping) {

        var targetLane = this.lookupReference('targetLaneBox').getValue();

        if (!targetLane) {

            return 'A valid target lane is required.';
        }

        var targetSimulation = this.lookupReference('targetSimulationBox').getValue();
        var mappingStore = this.getViewModel().get('laneMappingStore');
        var matches = mappingStore.queryBy(function(record, id) {

            if (!laneMapping || laneMapping !== record) {

                return record.get('targetSimulation') === targetSimulation && record.get('targetLane') === targetLane;
            }

        }, this);

        if (matches.length !== 0) {

            return 'A lane mapping to the selected target simulation and lane already exists.';
        }

        return true;
    },

    /**
     * @method validateTargetSimulationField
     * 
     * Validates the current value of the target simulation field.
     * 
     * @protected
     * @param {String} value The current field value.
     * @return {Boolean/String} True if the current field value is valid, otherwise a string
     * validation message is returned.
     */
    validateTargetSimulationField : function(value) {

        var target = this.lookupReference('targetSimulationBox').getValue();

        if (!target) {

            return 'A valid target simulation is required.';
        }

        var sourceBox = this.lookupReference('sourceSimulationBox');

        if (sourceBox.getValue()) {

            if (sourceBox.getValue() === target) {

                return 'The source and target simulations for each lane mapping must be unique.';
            }

            sourceBox.validate();
        }

        return true;
    }
});
