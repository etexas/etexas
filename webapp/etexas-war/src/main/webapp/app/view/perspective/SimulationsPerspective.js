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
 * @class ETexas.view.perspective.SimulationsPerspective
 * @extends Ext.container.Container
 * 
 * A container to view composite and simulation content.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.perspective.SimulationsPerspective', {
    extend : 'Ext.container.Container',
    xtype : 'simulationsperspective',

    requires : [ 'ETexas.view.composite.CompositeGrid', 'ETexas.view.composite.CompositeToolbar', 'ETexas.view.perspective.SimulationsPerspectiveController',
            'ETexas.view.perspective.SimulationsPerspectiveModel', 'ETexas.view.simulation.SimulationGrid', 'ETexas.view.simulation.SimulationToolbar' ],

    controller : 'simulationsperspective',

    viewModel : {
        type : 'simulationsperspective'
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

        return [ this._buildCompositeGrid(), this._buildSimulationGrid() ];
    },

    /**
     * @method _buildCompositeGrid
     * 
     * Builds the composite grid for this container.
     * 
     * @private
     * @return {ETexas.view.composite.CompositeGrid} The composite grid for this container.
     */
    _buildCompositeGrid : function() {

        return {
            region : 'west',
            xtype : 'compositegrid',
            reference : 'compositeGrid',
            id : Ext.id(null, 'composite-grid-'),
            title : 'Composites',
            frame : true,
            width : '35%',
            split : true,
            dockedItems : [ {
                xtype : 'compositetoolbar',
                reference : 'compositeToolbar',
                id : Ext.id(null, 'composite-toolbar-'),
                dock : 'top'
            } ],
            selModel : {
                selType : 'rowmodel',
                mode : 'MULTI'
            },
            tools : [ {
                type : 'help',
                reference : 'compositeHelpTool',
                id : Ext.id(null, 'composite-help-tool-'),
                callback : 'onCompositeHelpToolClicked',
                tooltip : 'Composites Help'
            } ],
            bind : {
                store : '{compositeStore}'
            },
            listeners : {
                compositereporting : 'showCompositeReportingWindow',
                compositesettings : 'showCompositeSettingsWindow',
                copycomposite : 'showCopyCompositeForm',
                createmodel : 'showCreateCompositeForm',
                deletemodel : 'deleteComposites',
                exportcomposite : 'exportComposite',
                renamecomposite : 'showRenameCompositeForm',
                selectionchange : 'onCompositeSelectionChange'
            }
        };
    },

    /**
     * @method _buildSimulationGrid
     * 
     * Builds the simulation grid for this container.
     * 
     * @private
     * @return {ETexas.view.simulation.SimulationGrid} The simulation grid for this container.
     */
    _buildSimulationGrid : function() {

        return {
            region : 'center',
            xtype : 'simulationgrid',
            reference : 'simulationGrid',
            id : Ext.id(null, 'simulation-grid-'),
            title : 'Simulations',
            frame : true,
            dockedItems : [ {
                xtype : 'simulationtoolbar',
                reference : 'simulationToolbar',
                id : Ext.id(null, 'simulation-toolbar-'),
                dock : 'top'
            } ],
            selModel : {
                selType : 'rowmodel',
                mode : 'MULTI'
            },
            tools : [ {
                type : 'help',
                reference : 'simulationHelpTool',
                id : Ext.id(null, 'simulation-help-tool-'),
                callback : 'onSimulationHelpToolClicked',
                tooltip : 'Simulations Help'
            } ],
            bind : {
                store : '{simulationStore}'
            },
            listeners : {
                copysimulation : 'showCopySimulationForm',
                createsimulation : 'showCreateSimulationForm',
                deletemodel : 'deleteSimulations',
                detectors : 'showDetectorsWindow',
                exportsimulation : 'exportSimulation',
                updatesimulation : 'showUpdateSimulationForm',
                selectionchange : 'onSimulationSelectionChange',
                simulationsource : 'editSourceFiles',
                uploadsimulation : 'showUploadSimulationForm'
            }
        };
    }
});
