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
 * @class ETexas.view.display.DisplayContainer
 * @extends Ext.container.Container
 * 
 * A container to display the visualization.
 * 
 * @author ttevendale
 */
Ext.define('ETexas.view.display.DisplayContainer', {

    extend : 'Ext.container.Container',

    xtype : 'displaycontainer',

    requires : [ 'ETexas.view.display.DisplayContainerController', 'ETexas.view.display.DisplayContainerModel' ],

    controller : 'displaycontainer',

    viewModel : {
        type : 'displaycontainer'
    },

    layout : 'border',

    listeners : {
        afterrender : 'onViewRendered'
    },

    /** @inheritdoc */
    initComponent : function() {

        this.items = [ this._buildVisualizationPanel(), this._buildInfoPanel() ];
        this.callParent();
    },

    /**
     * @method _buildInfoPanel
     * 
     * Builds the info panel.
     * 
     * @private
     * @return {Ext.tree.Panel} The info panel
     */
    _buildInfoPanel : function() {

        return {
            region : 'east',
            xtype : 'treepanel',
            reference : 'displayInfoPanel',
            id : Ext.id(null, 'display-info-panel-'),
            split : true,
            collapsible : true,
            cls : 'tree_wrap',
            title : 'Display Info',
            rootVisible : false,
            store : {
                root : {}
            },
            width : '30%',
            listeners : {
                itemclick : 'onTreePanelItemClick'
            }
        };
    },

    /**
     * @method _buildVisualizationPanel
     * 
     * Builds the visualization panel which contains Three.js.
     * 
     * @private
     * @return {Ext.panel.Panel} The visualization panel
     */
    _buildVisualizationPanel : function() {

        return {
            region : 'center',
            xtype : 'panel',
            title : 'Display',
            layout : 'fit',
            dockedItems : [ this._buildVisualizationToolbar() ],
            reference : 'displayPanel',
            id : Ext.id(null, 'display-panel-'),
            html : '<div class="ThreeDisplayElement" id="ThreeDisplayElement"></div>'
        };
    },

    /**
     * @method _buildVisualizationToolbar
     * 
     * Builds the visualization tool bar.
     * 
     * @private
     * @return {Ext.toolbar.Toolbar} The visualization tool bar
     */
    _buildVisualizationToolbar : function() {

        var toolbar = {
            xtype : 'toolbar',
            docked : 'top',
            reference : 'displayToolbar',
            items : [ {
                xtype : 'button',
                reference : 'viewButton',
                id : Ext.id(null, 'view-button-'),
                text : 'View',
                menu : [ {
                    xtype : 'menucheckitem',
                    reference : 'buildingsCheckBox',
                    id : Ext.id(null, 'buildings-check-box-'),
                    text : 'Buildings',
                    checked : true,
                    listeners : {
                        checkchange : 'onBuildingsCheckChange'
                    }
                }, {
                    xtype : 'menucheckitem',
                    reference : 'cellTowersCheckBox',
                    id : Ext.id(null, 'cell-towers-check-box-'),
                    text : 'Cell Towers',
                    checked : true,
                    listeners : {
                        checkchange : 'onCellTowersCheckChange'
                    }
                }, {
                    xtype : 'menucheckitem',
                    reference : 'detectorsCheckBox',
                    id : Ext.id(null, 'detectors-check-box-'),
                    text : 'Detectors',
                    checked : true,
                    listeners : {
                        checkchange : 'onDetectorsCheckChange'
                    }
                }, {
                    xtype : 'menucheckitem',
                    reference : 'floatingIdsCheckBox',
                    id : Ext.id(null, 'floating-ids-check-box-'),
                    text : 'Floating IDs',
                    checked : true,
                    listeners : {
                        checkchange : 'onFloatingIdsCheckChange'
                    }
                }, {
                    xtype : 'menucheckitem',
                    reference : 'hudDisplayCheckBox',
                    id : Ext.id(null, 'hud-display-check-box-'),
                    text : 'Hud Display',
                    checked : true,
                    listeners : {
                        checkchange : 'onHudDisplayCheckChange'
                    }
                }, {
                    xtype : 'menucheckitem',
                    reference : 'lanesCheckBox',
                    id : Ext.id(null, 'lanes-check-box-'),
                    text : 'Lanes',
                    checked : true,
                    listeners : {
                        checkchange : 'onLanesCheckChange'
                    }
                }, {
                    xtype : 'menucheckitem',
                    reference : 'signalsCheckBox',
                    id : Ext.id(null, 'signals-check-box-'),
                    text : 'Signals',
                    checked : true,
                    listeners : {
                        checkchange : 'onSignalsCheckChange'
                    }
                }, {
                    xtype : 'menucheckitem',
                    reference : 'standaloneDevicesCheckBox',
                    id : Ext.id(null, 'standalone-devices-check-box-'),
                    text : 'Standalone Devices',
                    checked : true,
                    listeners : {
                        checkchange : 'onStandaloneDevicesCheckChange'
                    }
                }, {
                    xtype : 'menucheckitem',
                    reference : 'vehiclesCheckBox',
                    id : Ext.id(null, 'vehicles-check-box-'),
                    text : 'Vehicles',
                    checked : true,
                    listeners : {
                        checkchange : 'onVehiclesCheckChange'
                    }
                } ]
            }, {
                xtype : 'button',
                reference : 'cameraViewButton',
                id : Ext.id(null, 'camera-view-button-'),
                text : 'Camera View'
            } ],
            id : Ext.id(null, 'display-toolbar-')
        };

        return toolbar;
    },

    /**
     * @method addDataToInfoBox
     * 
     * Calls the Controller to add data to the info panel based on the type and ID.
     * 
     * @public
     * @param {String} type The type of object to add to the info panel.
     * @param {String} simulationName the name of the simulation that the object is attached to (if
     * there is one).
     * @param {Number} id The ID of the object to add to the info panel.
     */
    addDataToInfoPanel : function(type, simulationName, id) {

        this.getController().addDataToInfoPanel(type, simulationName, id);
    },

    /**
     * @method reset
     * 
     * Calls the Controller to reset ThreeJS.
     * 
     * @public
     */
    reset : function() {

        this.getController().resetThreeJS();
    }
});
