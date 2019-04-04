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
 * @class ETexas.view.composite.EditCompositeOptionsForm
 * @extends ETexas.view.model.ModelForm
 * 
 * A form panel to edit the options for an existing {@link ETexas.model.CompositeModel}.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.composite.EditCompositeOptionsForm', {
    extend : 'ETexas.view.model.ModelForm',
    xtype : 'editcompositeoptionsform',

    requires : [ 'ETexas.view.composite.EditCompositeOptionsFormController' ],

    controller : 'editcompositeoptionsform',

    title : 'Edit Options',

    /** @inheritdoc */
    initComponent : function() {

        this.items = this.buildItems();
        this.callParent();
    },

    /** @inheritdoc */
    buildButtons : function() {

        return Ext.Array.insert(this.callParent(), 0, [ {
            reference : 'updateButton',
            id : Ext.id(null, 'update-button-'),
            handler : 'onUpdateButtonClicked',
            text : 'Update'
        } ]);
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

        var labelWidth = new Ext.util.TextMetrics().getWidth('Propagation Loss Model:');
        var latitudeMsg = 'Latitude values must be in the range of -90 DD to 90 DD.';
        var longitudeMsg = 'Longitude values must be in the range of -180 DD to 180 DD.';

        return [ {
            xtype : 'numberfield',
            reference : 'latitudeField',
            id : Ext.id(null, 'latitude-field-'),
            name : 'latitude',
            fieldLabel : 'Latitude (DD)',
            labelWidth : labelWidth,
            decimalPrecision : 7,
            allowBlank : false,
            blankText : 'A valid latitude is required.',
            allowExponential : false,
            minValue : -90,
            maxValue : 90,
            minText : latitudeMsg,
            maxText : latitudeMsg
        }, {
            xtype : 'numberfield',
            reference : 'longitudeField',
            id : Ext.id(null, 'longitude-field-'),
            name : 'longitude',
            fieldLabel : 'Longitude (DD)',
            labelWidth : labelWidth,
            decimalPrecision : 7,
            allowBlank : false,
            blankText : 'A valid longitude is required.',
            allowExponential : false,
            minValue : -180,
            maxValue : 180,
            minText : longitudeMsg,
            maxText : longitudeMsg
        }, {
            xtype : 'combobox',
            reference : 'geographicCalculatorBox',
            id : Ext.id(null, 'geographic-calculator-box-'),
            name : 'geographicCalculator',
            fieldLabel : 'Geographic Calculator',
            labelWidth : labelWidth,
            displayField : 'calculator',
            queryMode : 'local',
            editable : false,
            allowBlank : false,
            blankText : 'A valid geographic calculator is required.',
            bind : {
                store : '{calculatorStore}'
            }
        }, {
            xtype : 'combobox',
            reference : 'propagationLossModelBox',
            id : Ext.id(null, 'propagation-loss-model-box-'),
            name : 'propagationLossModel',
            fieldLabel : 'Propagation Loss Model',
            labelWidth : labelWidth,
            displayField : 'propagationLossModel',
            queryMode : 'local',
            editable : false,
            allowBlank : false,
            blankText : 'A valid propagation loss model is required.',
            bind : {
                store : '{propagationLossModelStore}'
            }
        }, {
            xtype : 'combobox',
            reference : 'communicationsModelBox',
            id : Ext.id(null, 'communications-model-box-'),
            name : 'communicationsModel',
            fieldLabel : 'Communications Model',
            labelWidth : labelWidth,
            displayField : 'communicationsModel',
            queryMode : 'local',
            editable : false,
            allowBlank : false,
            blankText : 'A valid communications model is required.',
            bind : {
                store : '{communicationsStore}'
            }
        } ];
    },

    /** @inheritdoc */
    buildTools : function() {

        return Ext.Array.insert(this.callParent(), 0, [ {
            type : 'help',
            reference : 'helpTool',
            id : Ext.id(null, 'help-tool-'),
            callback : 'onHelpToolClicked',
            tooltip : 'Edit Options Help'
        } ]);
    }
});
