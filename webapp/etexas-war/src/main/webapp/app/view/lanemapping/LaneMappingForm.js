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
 * @class ETexas.view.lanemapping.LaneMappingForm
 * @extends ETexas.view.model.ModelForm
 * 
 * An abstract form panel for {@link ETexas.model.LaneMappingModel} operations.
 * 
 * @abstract
 * @author emyers
 */
Ext.define('ETexas.view.lanemapping.LaneMappingForm', {
    extend : 'ETexas.view.model.ModelForm',

    requires : [ 'ETexas.view.lanemapping.LaneMappingFormModel' ],

    viewModel : {

        type : 'lanemappingform'
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

        var labelWidth = new Ext.util.TextMetrics().getWidth('Source Simulation:');

        return [ {
            xtype : 'combobox',
            reference : 'sourceSimulationBox',
            id : Ext.id(null, 'source-simulation-box-'),
            name : 'sourceSimulation',
            fieldLabel : 'Source Simulation',
            labelWidth : labelWidth,
            displayField : 'name',
            valueField : 'id',
            queryMode : 'local',
            editable : false,
            listeners : {
                change : 'onSourceSimulationChange'
            },
            bind : {
                store : '{simulationStore}',
                selection : '{selectedSourceSimulation}'
            }
        }, {
            xtype : 'panel',
            reference : 'sourceLanePanel',
            id : Ext.id(null, 'source-lane-panel-'),
            margin : '0 0 5 0',
            layout : 'hbox',
            items : [ {
                xtype : 'combobox',
                reference : 'sourceLaneBox',
                id : Ext.id(null, 'source-lane-box-'),
                name : 'sourceLane',
                fieldLabel : 'Source Lane',
                labelWidth : labelWidth,
                displayField : 'laneId',
                queryMode : 'local',
                editable : false,
                bind : {
                    store : '{sourceLaneStore}',
                    selection : '{selectedSourceLane}'
                }
            }, {
                xtype : 'button',
                reference : 'sourceLaneButton',
                id : Ext.id(null, 'source-lane-button-'),
                margin : '0 0 0 5',
                text : 'Show Lanes',
                handler : 'onSourceLaneButtonClicked',
                bind : {
                    disabled : '{!selectedSourceSimulation}'
                }
            } ]
        }, {
            xtype : 'combobox',
            reference : 'targetSimulationBox',
            id : Ext.id(null, 'target-simulation-box-'),
            name : 'targetSimulation',
            fieldLabel : 'Target Simulation',
            labelWidth : labelWidth,
            displayField : 'name',
            valueField : 'id',
            queryMode : 'local',
            editable : false,
            listeners : {
                change : 'onTargetSimulationChange'
            },
            bind : {
                store : '{simulationStore}',
                selection : '{selectedTargetSimulation}'
            }
        }, {
            xtype : 'panel',
            reference : 'targetLanePanel',
            id : Ext.id(null, 'target-lane-panel-'),
            layout : 'hbox',
            items : [ {
                xtype : 'combobox',
                reference : 'targetLaneBox',
                id : Ext.id(null, 'target-lane-box-'),
                name : 'targetLane',
                fieldLabel : 'Target Lane',
                labelWidth : labelWidth,
                displayField : 'laneId',
                queryMode : 'local',
                editable : false,
                bind : {
                    store : '{targetLaneStore}',
                    selection : '{selectedTargetLane}'
                }
            }, {
                xtype : 'button',
                reference : 'targetLaneButton',
                id : Ext.id(null, 'target-lane-button-'),
                margin : '0 0 0 5',
                text : 'Show Lanes',
                handler : 'onTargetLaneButtonClicked',
                bind : {
                    disabled : '{!selectedTargetSimulation}'
                }
            } ]
        } ];
    }
});
