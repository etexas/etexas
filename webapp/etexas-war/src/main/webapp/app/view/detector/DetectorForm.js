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
 * @class ETexas.view.detector.DetectorForm
 * @extends ETexas.view.model.ModelForm
 * 
 * An abstract form panel for {@link ETexas.model.DetectorModel} operations.
 * 
 * @abstract
 * @author emyers
 */
Ext.define('ETexas.view.detector.DetectorForm', {
    extend : 'ETexas.view.model.ModelForm',

    requires : [ 'ETexas.view.detector.DetectorFormModel' ],

    viewModel : {

        type : 'detectorform'
    },

    /** @inheritdoc */
    initComponent : function() {

        this.items = this.buildItems();
        this.callParent();
    },

    /**
     * @method buildItems
     * 
     * Builds the items for this form panel.
     * 
     * @template
     * @protected
     * @return {Object[]} The items for this form panel.
     */
    buildItems : function() {

        var widthMsg = 'Detector widths must be in the range of 1 cm to 400 cm.';
        var heightMsg = 'Detector heights must be in the range of 1 cm to 800 cm.';
        var minDistanceMsg = 'Distances from the stop line must be greater than or equal to 0 cm.';

        return [ {
            xtype : 'panel',
            reference : 'lanePanel',
            id : Ext.id(null, 'lane-panel-'),
            margin : '0 0 5 0',
            layout : {
                type : 'hbox',
                align : 'middle'
            },
            items : [ {
                xtype : 'combobox',
                reference : 'laneBox',
                id : Ext.id(null, 'lane-box-'),
                name : 'lane',
                fieldLabel : 'Lane',
                displayField : 'laneId',
                queryMode : 'local',
                editable : false,
                listeners : {
                    select : 'onLaneComboBoxSelected'
                },
                bind : {
                    store : '{laneStore}',
                    selection : '{selectedLane}'
                },
                allowBlank : false,
                blankText : 'A valid lane number is required.'
            }, {
                xtype : 'button',
                reference : 'laneButton',
                id : Ext.id(null, 'lane-button-'),
                margin : '0 0 0 5',
                handler : 'onLaneButtonClicked',
                text : 'Show Lanes'
            } ]
        }, {
            xtype : 'numberfield',
            reference : 'widthField',
            id : Ext.id(null, 'width-field-'),
            name : 'width',
            fieldLabel : 'Width (cm)',
            allowBlank : false,
            blankText : 'A valid detector width is required.',
            allowExponential : false,
            minValue : 1,
            maxValue : 400,
            minText : widthMsg,
            maxText : widthMsg
        }, {
            xtype : 'numberfield',
            reference : 'heightField',
            id : Ext.id(null, 'height-field-'),
            name : 'height',
            fieldLabel : 'Height (cm)',
            allowBlank : false,
            blankText : 'A valid detector height is required.',
            allowExponential : false,
            minValue : 1,
            maxValue : 800,
            minText : heightMsg,
            maxText : heightMsg
        }, {
            xtype : 'numberfield',
            reference : 'distanceField',
            id : Ext.id(null, 'distance-field-'),
            name : 'distance',
            fieldLabel : 'Distance (cm)',
            allowBlank : false,
            blankText : 'A valid distance from the stop line is required.',
            allowExponential : false,
            minValue : 0,
            negativeText : minDistanceMsg
        } ];
    }
});
