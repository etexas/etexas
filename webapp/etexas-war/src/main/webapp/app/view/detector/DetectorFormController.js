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
 * @class ETexas.view.detector.DetectorFormController
 * @extends ETexas.view.model.ModelFormController
 * 
 * The {@link ETexas.view.detector.DetectorForm} controller.
 * 
 * @abstract
 * @author emyers
 * @author ttevendale
 */
Ext.define('ETexas.view.detector.DetectorFormController', {
    extend : 'ETexas.view.model.ModelFormController',

    requires : [ 'ETexas.view.lane.LaneGrid', 'ETexas.view.window.BasicWindow' ],

    /** @inheritdoc */
    init : function(view) {

        view.lookupReference('distanceField').validator = Ext.bind(this.validateDistanceField, this);
        this.callParent([ view ]);
    },

    /**
     * @method onLaneButtonClicked
     * 
     * Handles the event that is generated when the user clicks the Show Lanes button.
     * 
     * @protected
     * @param {Ext.button.Button} button The clicked button.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onLaneButtonClicked : function(button, e, eOpts) {

        var window = this.getView().add({
            xtype : 'basicwindow',
            reference : 'lanesWindow',
            id : Ext.id(null, 'lanes-window-'),
            title : 'Lane Information',
            layout : 'fit',
            width : 756,
            height : 512,
            items : [ {
                xtype : 'lanegrid',
                reference : 'laneGrid',
                id : Ext.id(null, 'lane-grid-'),
                bind : {
                    store : '{laneStore}',
                    selection : '{selectedLane}'
                }
            } ]

        });

        window.show();
    },

    /**
     * @method onLaneComboBoxSelected
     * 
     * Handles the event that is generated when the user selects a lane in the combo box.
     * 
     * @protected
     * @param {Ext.form.field.ComboBox} combo The combo box.
     * @param {Ext.data.Model} record The selected record.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onLaneComboBoxSelected : function(combo, record, eOpts) {

        this.getView().lookupReference('distanceField').validate();
    },

    /**
     * @method validateDistanceField
     * 
     * Validates the current value of the detector's distance field.
     * 
     * @protected
     * @param {String} value The current field value.
     * @return {Boolean/String} True if the current field value is valid, otherwise a string
     * validation message is returned.
     */
    validateDistanceField : function(value) {

        var laneId = this.getView().lookupReference('laneBox').getValue();
        var laneStore = this.getViewModel().get('laneStore');

        if (!laneStore) {

            this.getViewModel().notify();
            laneStore = this.getViewModel().get('laneStore');
        }

        var lane = laneStore.findRecord('laneId', laneId);

        if (lane) {

            var laneNodes = lane.get('laneGeomList');
            var totalDistance = 0;

            for (var i = 0; i < laneNodes.length; i++) {

                if (i !== 0) {

                    var previousNode = laneNodes[i - 1];
                    var laneNode = laneNodes[i];
                    totalDistance += Math.sqrt(Math.pow(laneNode.x - previousNode.x, 2) + Math.pow(laneNode.y - previousNode.y, 2) + Math.pow(laneNode.z - previousNode.z, 2));
                }
            }

            totalDistance = Math.floor(totalDistance * 100) / 100;

            if (totalDistance < value) {

                return "The detector's distance from the current lane's stop line is longer than the lane's total length (" + totalDistance + " cm).";
            }
        }

        return true;
    }
});
