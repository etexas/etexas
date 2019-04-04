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
 * @class ETexas.view.composite.CompositeOptionsPanel
 * @extends Ext.panel.Panel
 * 
 * A panel to view the options for an existing {@link ETexas.model.CompositeModel}.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.composite.CompositeOptionsPanel', {
    extend : 'Ext.panel.Panel',
    xtype : 'compositeoptionspanel',

    requires : [ 'ETexas.view.composite.CompositeOptionsPanelController' ],

    controller : 'compositeoptionspanel',

    layout : {
        type : 'vbox',
        align : 'stretch'
    },

    /** @inheritdoc */
    initComponent : function() {

        this.items = this.buildItems();
        this.callParent();
    },

    /**
     * @method buildItems
     * 
     * Builds the items for this panel.
     * 
     * @template
     * @protected
     * @return {Object[]} The items for this panel.
     */
    buildItems : function() {

        return [ this._buildCoordinatesPanel(), this._buildCommunicationsPanel() ];
    },

    /**
     * @method _buildCoordinatesPanel
     * 
     * Builds the coordinates panel for this panel.
     * 
     * @private
     * @return {Ext.panel.Panel} The coordinates panel for this panel.
     */
    _buildCoordinatesPanel : function() {

        var labelWidth = new Ext.util.TextMetrics().getWidth('Propagation Loss Model:');

        return {
            xtype : 'panel',
            reference : 'coordinatesPanel',
            id : Ext.id(null, 'coordinates-panel-'),
            title : 'Coordinates',
            margin : '0 5 0 5',
            bodyPadding : 10,
            layout : {
                type : 'vbox',
                align : 'stretch'
            },
            defaults : {
                xtype : 'displayfield',
                labelWidth : labelWidth
            },
            items : [ {
                reference : 'latitudeField',
                id : Ext.id(null, 'latitude-field-'),
                fieldLabel : 'Latitude (DD)',
                renderer : Ext.util.Format.numberRenderer('0.0000000')
            }, {
                reference : 'longitudeField',
                id : Ext.id(null, 'longitude-field-'),
                fieldLabel : 'Longitude (DD)',
                renderer : Ext.util.Format.numberRenderer('0.0000000')
            }, {
                reference : 'geographicCalculatorField',
                id : Ext.id(null, 'geographic-calculator-field-'),
                fieldLabel : 'Geographic Calculator'
            }, {
                reference : 'propagationLossModelField',
                id : Ext.id(null, 'propagation-loss-model-field-'),
                fieldLabel : 'Propagation Loss Model'
            } ]
        };
    },

    /**
     * @method _buildCommunicationsPanel
     * 
     * Builds the communications panel for this panel.
     * 
     * @private
     * @return {Ext.panel.Panel} The communications panel for this panel.
     */
    _buildCommunicationsPanel : function() {

        var labelWidth = new Ext.util.TextMetrics().getWidth('Propagation Loss Model:');

        return {
            xtype : 'panel',
            reference : 'communicationsPanel',
            id : Ext.id(null, 'communications-panel-'),
            title : 'Communications',
            margin : '0 5 0 5',
            bodyPadding : 10,
            layout : {
                type : 'vbox',
                align : 'stretch'
            },
            items : [ {
                xtype : 'displayfield',
                reference : 'communicationsModelField',
                id : Ext.id(null, 'communications-model-field-'),
                fieldLabel : 'Communications Model',
                labelWidth : labelWidth
            }, {
                xtype : 'panel',
                reference : 'cellularConfigurationPanel',
                id : Ext.id(null, 'cellular-configuration-panel-'),
                layout : 'hbox',
                items : [ {
                    xtype : 'button',
                    reference : 'cellularConfigurationButton',
                    id : Ext.id(null, 'cellular-configuration-button-'),
                    handler : 'onCellularConfigurationButtonClicked',
                    margin : '10 0 0 ' + labelWidth,
                    text : 'Cellular Configuration'
                } ]
            }, {
                xtype : 'displayfield',
                reference : 'noteField',
                id : Ext.id(null, 'note-field-'),
                margin : '10 0 0 ' + labelWidth,
                value : 'NOTE: the cellular configuration may be changed at any time, but will only be in effect during executions when a cellular communications model (i.e., NS3) is selected.'
            } ]
        };
    }
});
