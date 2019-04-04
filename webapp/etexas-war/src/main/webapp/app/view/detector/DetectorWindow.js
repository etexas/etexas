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
 * @class ETexas.view.detector.DetectorWindow
 * @extends ETexas.view.window.BasicWindow
 * 
 * A window to view the detectors for an existing {@link ETexas.model.SimulationModel}.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.detector.DetectorWindow', {
    extend : 'ETexas.view.window.BasicWindow',
    xtype : 'detectorwindow',

    requires : [ 'ETexas.view.detector.DetectorGrid', 'ETexas.view.detector.DetectorWindowController', 'ETexas.view.detector.DetectorWindowModel', 'ETexas.view.model.ModelToolbar' ],

    controller : 'detectorwindow',

    viewModel : {
        type : 'detectorwindow'
    },

    title : 'Detectors',

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

        return [ this._buildDetectorGrid() ];
    },

    /**
     * @method _buildDetectorGrid
     * 
     * Builds the detector grid for this window.
     * 
     * @private
     * @return {ETexas.view.detector.DetectorGrid} The detector grid for this window.
     */
    _buildDetectorGrid : function() {

        return {
            xtype : 'detectorgrid',
            reference : 'detectorGrid',
            id : Ext.id(null, 'detector-grid-'),
            dockedItems : [ {
                xtype : 'modeltoolbar',
                reference : 'detectorToolbar',
                id : Ext.id(null, 'detector-toolbar-'),
                dock : 'top'
            } ],
            selModel : {
                selType : 'rowmodel',
                mode : 'MULTI'
            },
            bind : {
                store : '{detectorStore}'
            },
            listeners : {
                createmodel : 'showCreateDetectorForm',
                deletemodel : 'deleteDetectors',
                editmodel : 'showEditDetectorForm',
                selectionchange : 'onDetectorSelectionChange'
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
            tooltip : 'Detectors Help'
        } ]);
    }
});
