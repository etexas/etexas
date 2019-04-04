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
 * @class ETexas.view.composite.CompositeSettingsWindow
 * @extends ETexas.view.window.BasicWindow
 * 
 * A window to view the settings for an existing {@link ETexas.model.CompositeModel}.
 * 
 * @author emyers
 * @author ttevendale
 */
Ext.define('ETexas.view.composite.CompositeSettingsWindow', {
    extend : 'ETexas.view.window.BasicWindow',
    xtype : 'compositesettingswindow',

    requires : [ 'ETexas.view.application.ApplicationHostToolbar', 'ETexas.view.celltower.CellTowerGrid', 'ETexas.view.composite.CompositeOptionsPanel',
            'ETexas.view.composite.CompositeOptionsToolbar', 'ETexas.view.composite.CompositeSettingsWindowController', 'ETexas.view.composite.CompositeSettingsWindowModel',
            'ETexas.view.device.DeviceGrid', 'ETexas.view.device.FixedCellularDeviceGrid', 'ETexas.view.deviceprofile.CellularDeviceProfileGrid', 'ETexas.view.deviceprofile.DeviceProfileGrid',
            'ETexas.view.lanemapping.LaneMappingGrid', 'ETexas.view.model.ModelToolbar', 'ETexas.view.topographyfeature.BuildingGrid' ],

    controller : 'compositesettingswindow',

    viewModel : {
        type : 'compositesettingswindow'
    },

    title : 'Composite Settings',

    layout : 'fit',
    width : 756,
    height : 512,

    /** @inheritdoc */
    initComponent : function() {

        this.items = this.buildItems();
        this.callParent();
    },

    /**
     * @method buildItems
     * 
     * Builds the items for this window.
     * 
     * @template
     * @protected
     * @return {Object[]} The items for this window.
     */
    buildItems : function() {

        return [ {
            xtype : 'tabpanel',
            reference : 'tabPanel',
            id : Ext.id(null, 'tab-panel-'),
            items : [ this._buildBuildingGrid(), this._buildCellTowerGrid(), this._buildCellularDeviceProfileGrid(), this._buildFixedCellularDeviceGrid(), this._buildLaneMappingGrid(),
                    this._buildObuDeviceProfileGrid(), this._buildOptionsPanel(), this._buildRseDeviceGrid() ]
        } ];
    },

    /**
     * @method _buildBuildingGrid
     * 
     * Builds the building grid for this window.
     * 
     * @private
     * @return {ETexas.view.topographyfeature.BuildingGrid} The building grid for this window.
     */
    _buildBuildingGrid : function() {

        return {
            xtype : 'buildinggrid',
            reference : 'buildingGrid',
            id : Ext.id(null, 'building-grid-'),
            title : 'Buildings',
            dockedItems : [ {
                xtype : 'modeltoolbar',
                reference : 'buildingToolbar',
                id : Ext.id(null, 'building-toolbar-'),
                dock : 'top'
            } ],
            selModel : {
                selType : 'rowmodel',
                mode : 'MULTI'
            },
            bind : {
                store : '{buildingStore}'
            },
            listeners : {
                createmodel : 'showCreateBuildingForm',
                deletemodel : 'deleteBuildings',
                editmodel : 'showEditBuildingForm',
                selectionchange : 'onBuildingSelectionChange'
            }
        };
    },

    /**
     * @method _buildCellTowerGrid
     * 
     * Builds the cell tower grid for this window.
     * 
     * @private
     * @return {ETexas.view.celltower.CellTowerGrid} The cell tower grid for this window.
     */
    _buildCellTowerGrid : function() {

        return {
            xtype : 'celltowergrid',
            reference : 'cellTowerGrid',
            id : Ext.id(null, 'cell-tower-grid-'),
            title : 'Cell Towers',
            dockedItems : [ {
                xtype : 'modeltoolbar',
                reference : 'cellTowerToolbar',
                id : Ext.id(null, 'cell-tower-toolbar-'),
                dock : 'top'
            } ],
            bind : {
                store : '{cellTowerStore}'
            },
            selModel : {
                selType : 'rowmodel',
                mode : 'MULTI'
            },
            listeners : {
                createmodel : 'showCreateCellTowerForm',
                deletemodel : 'deleteCellTowers',
                editmodel : 'showEditCellTowerForm',
                selectionchange : 'onCellTowerSelectionChange'
            }
        };
    },

    /**
     * @method _buildCellularDeviceProfileGrid
     * 
     * Builds the cellular device profile grid for this window.
     * 
     * @private
     * @return {ETexas.view.cellulardeviceprofile.CellularDeviceProfileGrid} The cellular device
     * profile grid for this window.
     */
    _buildCellularDeviceProfileGrid : function() {

        return {
            xtype : 'cellulardeviceprofilegrid',
            reference : 'cellularDeviceProfileGrid',
            id : Ext.id(null, 'cellular-device-profile-grid-'),
            title : 'Cellular Device Profiles',
            dockedItems : [ {
                xtype : 'applicationhosttoolbar',
                reference : 'cellularDeviceProfileToolbar',
                id : Ext.id(null, 'cellular-device-profile-toolbar-'),
                dock : 'top'
            } ],
            selModel : {
                selType : 'rowmodel',
                mode : 'MULTI'
            },
            bind : {
                store : '{cellularDeviceProfileStore}'
            },
            listeners : {
                createmodel : 'showCreateCellularDeviceProfileForm',
                deletemodel : 'deleteCellularDeviceProfiles',
                editmodel : 'showEditCellularDeviceProfileForm',
                hostapplications : 'showCellularDeviceProfileApplicationsWindow',
                selectionchange : 'onCellularDeviceProfileSelectionChange'
            }
        };
    },

    /**
     * @method _buildFixedCellularDeviceGrid
     * 
     * Builds the fixed cellular device grid for this window.
     * 
     * @private
     * @return {ETexas.view.device.FixedCellularDeviceGrid} The fixed cellular device grid for this
     * window.
     */
    _buildFixedCellularDeviceGrid : function() {

        return {
            xtype : 'fixedcellulardevicegrid',
            reference : 'fixedCellularDeviceGrid',
            id : Ext.id(null, 'fixed-cellular-device-grid-'),
            title : 'Fixed Cellular Devices',
            dockedItems : [ {
                xtype : 'applicationhosttoolbar',
                reference : 'fixedCellularDeviceToolbar',
                id : Ext.id(null, 'fixed-cellular-device-toolbar-'),
                dock : 'top'
            } ],
            selModel : {
                selType : 'rowmodel',
                mode : 'MULTI'
            },
            bind : {
                store : '{fixedCellularDeviceStore}'
            },
            listeners : {
                createmodel : 'showCreateFixedCellularDeviceForm',
                deletemodel : 'deleteFixedCellularDevices',
                editmodel : 'showEditFixedCellularDeviceForm',
                hostapplications : 'showFixedCellularDeviceApplicationsWindow',
                selectionchange : 'onFixedCellularDeviceSelectionChange'
            }
        };
    },

    /**
     * @method _buildLaneMappingGrid
     * 
     * Builds the lane mapping grid for this window.
     * 
     * @private
     * @return {ETexas.view.lanemapping.LaneMappingGrid} The lane mapping grid for this window.
     */
    _buildLaneMappingGrid : function() {

        return {
            xtype : 'lanemappinggrid',
            reference : 'laneMappingGrid',
            id : Ext.id(null, 'lane-mapping-grid-'),
            title : 'Lane Mappings',
            dockedItems : [ {
                xtype : 'modeltoolbar',
                reference : 'laneMappingToolbar',
                id : Ext.id(null, 'lane-mapping-toolbar-'),
                dock : 'top'
            } ],
            selModel : {
                selType : 'rowmodel',
                mode : 'MULTI'
            },
            bind : {
                store : '{laneMappingStore}'
            },
            listeners : {
                createmodel : 'showCreateLaneMappingForm',
                deletemodel : 'deleteLaneMappings',
                editmodel : 'showEditLaneMappingForm',
                selectionchange : 'onLaneMappingSelectionChange'
            }
        };
    },

    /**
     * @method _buildObuDeviceProfileGrid
     * 
     * Builds the OBU device profile grid for this window.
     * 
     * @private
     * @return {ETexas.view.obudeviceprofile.ObuDeviceProfileGrid} The OBU device profile grid for
     * this window.
     */
    _buildObuDeviceProfileGrid : function() {

        return {
            xtype : 'deviceprofilegrid',
            reference : 'obuDeviceProfileGrid',
            id : Ext.id(null, 'obu-device-profile-grid-'),
            title : 'OBU Device Profiles',
            dockedItems : [ {
                xtype : 'applicationhosttoolbar',
                reference : 'obuDeviceProfileToolbar',
                id : Ext.id(null, 'obu-device-profile-toolbar-'),
                dock : 'top'
            } ],
            selModel : {
                selType : 'rowmodel',
                mode : 'MULTI'
            },
            bind : {
                store : '{obuDeviceProfileStore}'
            },
            listeners : {
                createmodel : 'showCreateObuDeviceProfileForm',
                deletemodel : 'deleteObuDeviceProfiles',
                editmodel : 'showEditObuDeviceProfileForm',
                hostapplications : 'showObuDeviceProfileApplicationsWindow',
                selectionchange : 'onObuDeviceProfileSelectionChange'
            }
        };
    },

    /**
     * @method _buildOptionsPanel
     * 
     * Builds the options panel for this window.
     * 
     * @private
     * @return {ETexas.view.composite.CompositeOptionsPanel} The options panel for this window.
     */
    _buildOptionsPanel : function() {

        return {
            xtype : 'compositeoptionspanel',
            reference : 'compositeOptionsPanel',
            id : Ext.id(null, 'composite-options-panel-'),
            title : 'Options',
            dockedItems : [ {
                xtype : 'compositeoptionstoolbar',
                reference : 'compositeOptionsToolbar',
                id : Ext.id(null, 'composite-options-toolbar-'),
                dock : 'top'
            } ],
            listeners : {
                editcellularconfiguration : 'showEditCellularConfigurationForm',
                editmodel : 'showEditCompositeOptionsForm'
            }
        };
    },

    /**
     * @method _buildRseDeviceGrid
     * 
     * Builds the RSE device grid for this window.
     * 
     * @private
     * @return {ETexas.view.device.DeviceGrid} The RSE device grid for this window.
     */
    _buildRseDeviceGrid : function() {

        return {
            xtype : 'devicegrid',
            reference : 'rseDeviceGrid',
            id : Ext.id(null, 'rse-device-grid-'),
            title : 'RSE Devices',
            dockedItems : [ {
                xtype : 'applicationhosttoolbar',
                reference : 'rseDeviceToolbar',
                id : Ext.id(null, 'rse-device-toolbar-'),
                dock : 'top'
            } ],
            selModel : {
                selType : 'rowmodel',
                mode : 'MULTI'
            },
            bind : {
                store : '{rseDeviceStore}'
            },
            listeners : {
                createmodel : 'showCreateRseDeviceForm',
                deletemodel : 'deleteRseDevices',
                editmodel : 'showEditRseDeviceForm',
                hostapplications : 'showRseDeviceApplicationsWindow',
                selectionchange : 'onRseDeviceSelectionChange'
            }
        };
    },

    /** @inheritdoc */
    buildTools : function() {

        return Ext.Array.insert(this.callParent(), 0, [ {
            type : 'help',
            reference : 'helpTool',
            id : Ext.id(null, 'help-tool-'),
            callback : 'onHelpToolClicked',
            tooltip : 'Composite Settings Help'
        } ]);
    }
});
