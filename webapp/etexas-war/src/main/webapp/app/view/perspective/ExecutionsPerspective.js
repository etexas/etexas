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
 * @class ETexas.view.perspective.ExecutionsPerspective
 * @extends Ext.container.Container
 * 
 * A container to view execution content.
 * 
 * @author emyers
 * @author ttevendale
 */
Ext.define('ETexas.view.perspective.ExecutionsPerspective', {
    extend : 'Ext.container.Container',
    xtype : 'executionsperspective',

    requires : [ 'ETexas.view.celltower.CellTowerGrid', 'ETexas.view.command.CommandGrid', 'ETexas.view.detector.DeployedDetectorGrid', 'ETexas.view.device.StandaloneDeviceGrid',
            'ETexas.view.execution.ExecutionGrid', 'ETexas.view.execution.ExecutionToolbar', 'ETexas.view.lane.LaneGrid', 'ETexas.view.log.LogContainer', 'ETexas.view.model.ModelToolbar',
            'ETexas.view.perspective.ExecutionsPerspectiveController', 'ETexas.view.perspective.ExecutionsPerspectiveModel', 'ETexas.view.signal.SignalGrid',
            'ETexas.view.topographyfeature.BuildingGrid', 'ETexas.view.vehicle.VehicleGrid', 'ETexas.view.display.DisplayContainer', 'Ext.grid.feature.Grouping' ],

    controller : 'executionsperspective',

    viewModel : {
        type : 'executionsperspective'
    },

    layout : 'border',

    /** @inheritdoc */
    initComponent : function() {

        this.items = this.buildItems();
        this.callParent();
    },

    /**
     * @method buildItems
     * 
     * Builds the items for this container.
     * 
     * @template
     * @protected
     * @return {Object[]} The items for this container.
     */
    buildItems : function() {

        return [ this._buildExecutionsPanel(), this._buildDetailsPanel() ];
    },

    /**
     * @method _buildExecutionsPanel
     * 
     * Builds the executions panel for this container.
     * 
     * @private
     * @return {Ext.panel.Panel} The executions panel for this container.
     */
    _buildExecutionsPanel : function() {

        return {
            region : 'west',
            xtype : 'panel',
            reference : 'executionsPanel',
            id : Ext.id(null, 'executions-panel-'),
            title : 'Executions',
            width : '25%',
            split : true,
            frame : true,
            collapsible : true,
            collapseToolText : 'Collapse',
            layout : {
                type : 'accordion',
                multi : true,
                fill : false
            },
            items : [ this._buildCompositesPanel(), this._buildExecutionGrid() ],
            tools : [ {
                type : 'help',
                reference : 'executionsHelpTool',
                id : Ext.id(null, 'executions-help-tool-'),
                callback : 'onExecutionsHelpToolClicked',
                tooltip : 'Executions Help'
            } ]
        };
    },

    /**
     * @method _buildCompositesPanel
     * 
     * Builds the composites panel for this container.
     * 
     * @private
     * @return {Ext.panel.Panel} The composites panel for this container.
     */
    _buildCompositesPanel : function() {

        return {
            xtype : 'panel',
            reference : 'compositesPanel',
            id : Ext.id(null, 'composites-panel-'),
            title : 'Select Composite',
            layout : 'fit',
            items : [ {
                xtype : 'combobox',
                reference : 'compositeBox',
                id : Ext.id(null, 'composite-box-'),
                displayField : 'name',
                queryMode : 'local',
                editable : false,
                bind : {
                    store : '{compositeStore}',
                    selection : '{selectedComposite}'
                }
            } ]
        };
    },

    /**
     * @method _buildExecutionGrid
     * 
     * Builds the execution grid for this container.
     * 
     * @private
     * @return {ETexas.view.execution.ExecutionGrid} The execution grid for this container.
     */
    _buildExecutionGrid : function() {

        return {
            xtype : 'executiongrid',
            reference : 'executionGrid',
            id : Ext.id(null, 'execution-grid-'),
            title : 'Composite Executions',
            dockedItems : [ {
                xtype : 'executiontoolbar',
                reference : 'executionToolbar',
                id : Ext.id(null, 'execution-toolbar-'),
                dock : 'top'
            } ],
            selModel : {
                selType : 'rowmodel',
                mode : 'MULTI'
            },
            bind : {
                store : '{executionStore}'
            },
            listeners : {
                createmodel : 'showCreateExecutionForm',
                deletemodel : 'deleteExecutions',
                editmodel : 'showEditExecutionForm',
                selectionchange : 'onExecutionSelectionChange',
                showexecutionmessages : 'showExecutionMessages'
            }
        };
    },

    /**
     * @method _buildDetailsPanel
     * 
     * Builds the details panel for this container.
     * 
     * @private
     * @return {Ext.panel.Panel} The details panel for this container.
     */
    _buildDetailsPanel : function() {

        return {
            region : 'center',
            xtype : 'panel',
            reference : 'detailsPanel',
            id : Ext.id(null, 'details-panel-'),
            title : 'Execution Details',
            layout : 'fit',
            frame : true,
            items : [ this._buildDetailsContainer() ]
        };
    },

    /**
     * @method _buildDetailsContainer
     * 
     * Builds the details container for this container.
     * 
     * @private
     * @return {Ext.container.Container} The details container for this container.
     */
    _buildDetailsContainer : function() {

        var textMargin = '0 20 0 0';
        var boldMetrics = new Ext.util.TextMetrics(Ext.getBody().createChild().applyStyles('font-eight:bold'));

        return {
            xtype : 'container',
            reference : 'detailsContainer',
            id : Ext.id(null, 'details-container-'),
            layout : 'border',
            items : [ {
                region : 'north',
                xtype : 'toolbar',
                reference : 'detailsToolbar',
                id : Ext.id(null, 'details-toolbar-'),
                items : [ {
                    xtype : 'displayfield',
                    reference : 'timeField',
                    id : Ext.id(null, 'time-field-'),
                    fieldLabel : 'Time',
                    labelStyle : 'font-weight:bold',
                    labelWidth : boldMetrics.getWidth('Time:'),
                    bind : '{simulationTime}',
                    margin : textMargin,
                    renderer : Ext.util.Format.numberRenderer('###,##0.0#')
                }, {
                    xtype : 'numberfield',
                    reference : 'stepsField',
                    id : Ext.id(null, 'steps-field-'),
                    fieldLabel : 'Steps',
                    labelStyle : 'font-weight:bold',
                    labelWidth : boldMetrics.getWidth('Steps:'),
                    margin : textMargin,
                    maxWidth : 150,
                    allowDecimals : false,
                    allowExponential : false,
                    allowBlank : false,
                    blankText : 'A valid number of steps is required to advance the execution.',
                    minValue : 1,
                    minText : 'A minimum of one step is required to advance the execution.',
                    value : 1
                }, {
                    xtype : 'displayfield',
                    reference : 'remainingField',
                    id : Ext.id(null, 'remaining-field-'),
                    fieldLabel : 'Remaining',
                    labelStyle : 'font-weight:bold',
                    labelWidth : boldMetrics.getWidth('Remaining::'),
                    bind : '{remainingSteps}',
                    margin : textMargin
                }, {
                    xtype : 'button',
                    reference : 'advanceButton',
                    id : Ext.id(null, 'advance-button-'),
                    handler : 'onAdvanceButtonClicked',
                    text : 'Advance'
                }, {
                    xtype : 'button',
                    reference : 'commandButton',
                    id : Ext.id(null, 'command-button-'),
                    text : 'Command',
                    menu : [ {
                        reference : 'changeLaneItem',
                        id : Ext.id(null, 'change-lane-item-'),
                        handler : 'showLaneCommandForm',
                        text : 'Change Lane'
                    }, {
                        reference : 'changeSignalItem',
                        id : Ext.id(null, 'change-signal-item-'),
                        handler : 'showSignalCommandForm',
                        text : 'Change Signal'
                    }, {
                        reference : 'changeSpeedItem',
                        id : Ext.id(null, 'change-speed-item-'),
                        handler : 'showSpeedCommandForm',
                        text : 'Change Speed'
                    }, '-', {
                        reference : 'injectVehicleItem',
                        id : Ext.id(null, 'inject-vehicle-item-'),
                        handler : 'showInjectionCommandForm',
                        text : 'Inject Vehicle'
                    } ]
                }, {
                    xtype : 'button',
                    reference : 'finishButton',
                    id : Ext.id(null, 'finish-button-'),
                    handler : 'onFinishButtonClicked',
                    text : 'Finish'
                }, '-', {
                    xtype : 'button',
                    reference : 'viewButton',
                    id : Ext.id(null, 'view-button-'),
                    text : 'View',
                    menu : [ {
                        reference : 'textItem',
                        id : Ext.id(null, 'text-item-'),
                        handler : 'onTextItemClicked',
                        text : 'Text'
                    }, {
                        reference : 'visualizationItem',
                        id : Ext.id(null, 'visualization-item-'),
                        handler : 'onVisualizationItemClicked',
                        text : 'Visualization'
                    } ]
                } ]

            }, this._buildInformationContainer() ]
        };
    },

    /**
     * @method _buildInformationContainer
     * 
     * Builds the information container for this container.
     * 
     * @private
     * @return {Ext.container.Container} The information container for this container.
     */
    _buildInformationContainer : function() {

        return {
            region : 'center',
            xtype : 'container',
            reference : 'informationContainer',
            id : Ext.id(null, 'information-container-'),
            layout : 'border',
            items : [ this._buildDisplayContainer() ]
        };
    },

    /**
     * @method _buildDisplayContainer
     * 
     * Builds the display container..
     * 
     * @private
     * @return {Ext.container.Container} The display container.
     */
    _buildDisplayContainer : function() {

        return {
            region : 'center',
            xtype : 'container',
            reference : 'displayContainer',
            id : Ext.id(null, 'display-contaniner-'),
            layout : 'card',
            items : [ this._buildInformationTabPanel(), this._buildVisualizationContainer() ]
        };
    },
    /**
     * @method _buildInformationTabPanel
     * 
     * Builds the information tab panel for this container.
     * 
     * @private
     * @return {Ext.tab.Panel} The information tab panel for this container.
     */
    _buildInformationTabPanel : function() {

        var viewConfig = {
            deferEmptyText : false,
            bind : {
                emptyText : '{emptyText}'
            }
        };

        return {
            region : 'center',
            xtype : 'tabpanel',
            reference : 'informationTabPanel',
            id : Ext.id(null, 'information-tab-panel-'),
            items : [ {
                xtype : 'buildinggrid',
                reference : 'buildingGrid',
                id : Ext.id(null, 'building-grid-'),
                title : 'Buildings',
                bind : '{buildingStore}'
            }, {
                xtype : 'celltowergrid',
                reference : 'cellTowerGrid',
                id : Ext.id(null, 'cell-tower-grid-'),
                title : 'Cell Towers',
                bind : '{cellTowerStore}'
            }, {
                xtype : 'commandgrid',
                reference : 'commandGrid',
                id : Ext.id(null, 'command-grid-'),
                title : 'Commands',
                bind : '{commandStore}',
                features : [ {
                    ftype : 'grouping',
                    groupHeaderTpl : '{name}'
                } ]
            }, {
                xtype : 'deployeddetectorgrid',
                reference : 'detectorGrid',
                id : Ext.id(null, 'detector-grid-'),
                title : 'Detectors',
                bind : '{detectorStore}',
                viewConfig : viewConfig,
                features : [ {
                    ftype : 'grouping',
                    groupHeaderTpl : '{name}'
                } ]
            }, {
                xtype : 'lanegrid',
                reference : 'laneGrid',
                id : Ext.id(null, 'lane-grid-'),
                title : 'Lanes',
                bind : '{laneStore}',
                features : [ {
                    ftype : 'grouping',
                    groupHeaderTpl : '{name}'
                } ]
            }, {
                xtype : 'logcontainer',
                reference : 'logContainer',
                id : Ext.id(null, 'log-container-'),
                title : 'Logs'
            }, {
                xtype : 'signalgrid',
                reference : 'signalGrid',
                id : Ext.id(null, 'signal-grid-'),
                title : 'Signals',
                bind : '{signalStore}',
                viewConfig : viewConfig,
                features : [ {
                    ftype : 'grouping',
                    groupHeaderTpl : '{name}'
                } ]
            }, {
                xtype : 'standalonedevicegrid',
                reference : 'standaloneDeviceGrid',
                id : Ext.id(null, 'standalone-device-grid-'),
                title : 'Standalone Devices',
                bind : '{standaloneDeviceStore}',
                viewConfig : viewConfig,
                listeners : {
                    showapplications : 'showApplications',
                    showmessages : 'showMessages'
                }
            }, {
                xtype : 'vehiclegrid',
                reference : 'vehicleGrid',
                id : Ext.id(null, 'vehicle-grid-'),
                title : 'Vehicles',
                bind : '{vehicleStore}',
                viewConfig : viewConfig,
                features : [ {
                    ftype : 'grouping',
                    groupHeaderTpl : '{name}'
                } ],
                listeners : {
                    showOnBoardDevices : 'showOnBoardDevices'
                }
            } ]
        };
    },

    /**
     * @method _buildVisualizationContainer
     * 
     * Builds the display container which contains three.js.
     * 
     * @private
     * @return {ETexas.view.display.DisplayContainer} The display container which contains three.js
     */
    _buildVisualizationContainer : function() {

        var panel = {
            region : 'center',
            xtype : 'displaycontainer',
            reference : 'visualizationContainer',
            id : Ext.id(null, 'visualization-container-')
        };

        return panel;
    }
});
