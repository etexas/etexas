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
 * @class ETexas.view.cellularconfiguration.EditCellularConfigurationForm
 * @extends ETexas.view.model.ModelForm
 * 
 * A form panel to edit the cellular configuration for an existing
 * {@link ETexas.model.CompositeModel}.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.cellularconfiguration.EditCellularConfigurationForm', {
    extend : 'ETexas.view.model.ModelForm',
    xtype : 'editcellularconfigurationform',

    requires : [ 'ETexas.view.cellularconfiguration.EditCellularConfigurationFormController', 'ETexas.view.cellularconfiguration.EditCellularConfigurationFormModel' ],

    controller : 'editcellularconfigurationform',

    viewModel : {
        type : 'editcellularconfigurationform'
    },

    title : 'Edit Cellular Configuration',

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

        var labelWidth = new Ext.util.TextMetrics().getWidth('Cellular Device Power (dBm):');
        var uplinkCarrierFrequencyMsg = 'Uplink carrier frequencies must be in the range of 18,000 to 24,599.';
        var downlinkCarrierFrequencyMsg = 'Downlink carrier frequencies must be in the range of 0 to 6,599.';
        var cellTowerNoiseMsg = 'Cell tower noise must be in the range of 0 dB to 194 dB.';
        var cellTowerPowerMsg = 'Cell tower power must be in the range of -200 dBm to 200 dBm.';
        var cellularDeviceNoiseMsg = 'Cellular device noise must be in the range of 0 dB to 194 dB.';
        var cellularDevicePowerMsg = 'Cellular device power must be in the range of -200 dBm to 200 dBm.';

        return [ {
            xtype : 'combobox',
            reference : 'uplinkBandwidthBox',
            id : Ext.id(null, 'uplink-bandwidth-box-'),
            name : 'uplinkBandwidth',
            fieldLabel : 'Uplink Bandwidth',
            labelWidth : labelWidth,
            displayField : 'bandwidth',
            queryMode : 'local',
            editable : false,
            bind : {
                store : '{bandwidthStore}'
            },
            allowBlank : false,
            blankText : 'A valid uplink bandwidth is required.'
        }, {
            xtype : 'combobox',
            reference : 'downlinkBandwidthBox',
            id : Ext.id(null, 'downlink-bandwidth-box-'),
            name : 'downlinkBandwidth',
            fieldLabel : 'Downlink Bandwidth',
            labelWidth : labelWidth,
            displayField : 'bandwidth',
            queryMode : 'local',
            editable : false,
            bind : {
                store : '{bandwidthStore}'
            },
            allowBlank : false,
            blankText : 'A valid downlink bandwidth is required.'
        }, {
            xtype : 'numberfield',
            reference : 'uplinkCarrierFrequencyField',
            id : Ext.id(null, 'uplink-carrier-frequency-field-'),
            name : 'uplinkCarrierFrequency',
            fieldLabel : 'Uplink Carrier Frequency',
            labelWidth : labelWidth,
            allowBlank : false,
            blankText : 'A valid uplink carrier frequency is required.',
            allowDecimals : false,
            allowExponential : false,
            minValue : 18000,
            maxValue : 24599,
            minText : uplinkCarrierFrequencyMsg,
            maxText : uplinkCarrierFrequencyMsg
        }, {
            xtype : 'numberfield',
            reference : 'downlinkCarrierFrequencyField',
            id : Ext.id(null, 'downlink-carrier-frequency-field-'),
            name : 'downlinkCarrierFrequency',
            fieldLabel : 'Downlink Carrier Frequency',
            labelWidth : labelWidth,
            allowBlank : false,
            blankText : 'A valid downlink carrier frequency is required.',
            allowDecimals : false,
            allowExponential : false,
            minValue : 0,
            maxValue : 6599,
            maxText : downlinkCarrierFrequencyMsg,
            negativeText : downlinkCarrierFrequencyMsg
        }, {
            xtype : 'numberfield',
            reference : 'cellTowerNoiseField',
            id : Ext.id(null, 'cell-tower-noise-field-'),
            name : 'cellTowerNoise',
            fieldLabel : 'Cell Tower Noise (dB)',
            labelWidth : labelWidth,
            allowBlank : false,
            blankText : 'A valid cell tower noise is required.',
            allowExponential : false,
            minValue : 0,
            maxValue : 194,
            maxText : cellTowerNoiseMsg,
            negativeText : cellTowerNoiseMsg
        }, {
            xtype : 'numberfield',
            reference : 'cellTowerPowerField',
            id : Ext.id(null, 'cell-tower-power-field-'),
            name : 'cellTowerPower',
            fieldLabel : 'Cell Tower Power (dBm)',
            labelWidth : labelWidth,
            allowBlank : false,
            blankText : 'A valid cell tower power is required.',
            allowExponential : false,
            minValue : -200,
            maxValue : 200,
            minText : cellTowerPowerMsg,
            maxText : cellTowerPowerMsg
        }, {
            xtype : 'numberfield',
            reference : 'cellularDeviceNoiseField',
            id : Ext.id(null, 'cellular-device-noise-field-'),
            name : 'cellularDeviceNoise',
            fieldLabel : 'Cellular Device Noise (dB)',
            labelWidth : labelWidth,
            allowBlank : false,
            blankText : 'A valid cellular device noise is required.',
            allowExponential : false,
            minValue : 0,
            maxValue : 194,
            maxText : cellularDeviceNoiseMsg,
            negativeText : cellularDeviceNoiseMsg
        }, {
            xtype : 'numberfield',
            reference : 'cellularDevicePowerField',
            id : Ext.id(null, 'cellular-device-power-field-'),
            name : 'cellularDevicePower',
            fieldLabel : 'Cellular Device Power (dBm)',
            labelWidth : labelWidth,
            allowBlank : false,
            blankText : 'A validate cellular device power is required.',
            allowExponential : false,
            minValue : -200,
            maxValue : 200,
            minText : cellularDevicePowerMsg,
            maxText : cellularDevicePowerMsg
        } ];
    },

    /** @inheritdoc */
    buildTools : function() {

        return Ext.Array.insert(this.callParent(), 0, [ {
            type : 'help',
            reference : 'helpTool',
            id : Ext.id(null, 'help-tool-'),
            callback : 'onHelpToolClicked',
            tooltip : 'Edit Cellular Configuration Help'
        } ]);
    }
});
