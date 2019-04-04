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
 * @class ETexas.view.log.LogGrid
 * @extends ETexas.view.model.ModelGrid
 * 
 * A grid panel to view {@link ETexas.model.LogModel} data.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.log.LogGrid', {
    extend : 'ETexas.view.model.ModelGrid',
    xtype : 'loggrid',

    requires : [ 'ETexas.view.log.LogGridController', 'ETexas.view.log.LogGridModel', 'Ext.toolbar.Paging' ],

    controller : 'loggrid',

    viewModel : {
        type : 'loggrid'
    },

    /** @inheritdoc */
    initComponent : function() {

        this.dockedItems = this.buildDockedItems();
        this.callParent();
    },

    /** @inheritdoc */
    buildColumns : function() {

        return Ext.Array.push(this.callParent(), {
            reference : 'hostColumn',
            id : Ext.id(null, 'host-column-'),
            dataIndex : 'deviceId',
            text : 'Host Device',
            flex : 1
        }, {
            reference : 'applicationColumn',
            id : Ext.id(null, 'application-column-'),
            dataIndex : 'applicationName',
            text : 'Application',
            flex : 2
        }, {
            reference : 'keyColumn',
            id : Ext.id(null, 'key-column-'),
            dataIndex : 'applicationKey',
            text : 'Key',
            flex : 2
        }, {
            reference : 'messageColumn',
            id : Ext.id(null, 'message-column-'),
            dataIndex : 'applicationMessage',
            text : 'Message',
            flex : 3
        }, {
            reference : 'timeColumn',
            id : Ext.id(null, 'time-column-'),
            dataIndex : 'simulationTime',
            text : 'Time',
            flex : 1
        });
    },

    /**
     * @method buildDockedItems
     * 
     * Builds the docked items for this grid panel.
     * 
     * @template
     * @protected
     * @return {Object[]} The docked items for this grid panel.
     */
    buildDockedItems : function() {

        return [ {
            xtype : 'pagingtoolbar',
            reference : 'pagingToolbar',
            id : Ext.id(null, 'paging-toolbar-'),
            dock : 'top',
            displayInfo : true,
            items : [ '-', {
                xtype : 'combobox',
                reference : 'resultsBox',
                id : Ext.id(null, 'results-box-'),
                fieldLabel : 'Results per Page',
                labelWidth : new Ext.util.TextMetrics().getWidth('Results per Page:'),
                displayField : 'count',
                queryMode : 'local',
                editable : false,
                value : 25,
                bind : {
                    store : '{resultCountStore}'
                }
            }, '-', {
                xtype : 'button',
                reference : 'searchButton',
                id : Ext.id(null, 'search-button-'),
                text : 'Search',
                handler : 'onSearchButtonClicked'
            }, {
                xtype : 'button',
                reference : 'resetButton',
                id : Ext.id(null, 'reset-button-'),
                text : 'Reset',
                handler : 'onResetButtonClicked'
            }, {
                xtype : 'button',
                reference : 'exportButton',
                id : Ext.id(null, 'export-button-'),
                text : 'Export',
                handler : 'onExportButtonClicked',
                disabled : true
            } ]
        } ];
    }
});
