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
 * @class ETexas.view.log.LogContainer
 * @extends Ext.container.Container
 * 
 * A container to view {@link ETexas.model.LogModel} data.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.log.LogContainer', {
    extend : 'Ext.container.Container',
    xtype : 'logcontainer',

    requires : [ 'ETexas.view.log.LogContainerController', 'ETexas.view.log.LogContainerModel', 'ETexas.view.log.LogGrid' ],

    controller : 'logcontainer',

    viewModel : {
        type : 'logcontainer'
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

        return [ this._buildLogGrid(), this._buildFilterPanel() ];
    },

    /**
     * @method _buildFilterPanel
     * 
     * Builds the filter panel for this container.
     * 
     * @private
     * @return {Ext.panel.Panel} The filter panel for this container.
     */
    _buildFilterPanel : function() {

        return {
            region : 'north',
            xtype : 'panel',
            reference : 'filterPanel',
            id : Ext.id(null, 'filter-panel-'),
            bodyPadding : '10 10 0 10',
            layout : 'fit',
            items : [ {
                xtype : 'fieldset',
                reference : 'filterSet',
                id : Ext.id(null, 'filter-set-'),
                title : 'Search Filters',
                collapsible : true,
                padding : 10,
                layout : 'hbox',
                items : [ this._buildHostFilterContainer(), this._buildApplicationFilterContainer(), this._buildKeyFilterContainer(), this._buildTimeFilterContainer() ]
            } ]
        };
    },

    /**
     * @method _buildHostFilterContainer
     * 
     * Builds the host device filter container for this container.
     * 
     * @private
     * @return {Ext.container.Container} The host device filter container for this container.
     */
    _buildHostFilterContainer : function() {

        return {
            xtype : 'container',
            reference : 'hostFilterContainer',
            id : Ext.id(null, 'host-filter-container-'),
            layout : 'vbox',
            items : [ {
                xtype : 'label',
                reference : 'hostFilterLabel',
                id : Ext.id(null, 'host-filter-label-'),
                text : 'Host Device',
                style : {
                    fontWeight : 'bold'
                }
            }, {
                xtype : 'grid',
                reference : 'hostFilterGrid',
                id : Ext.id(null, 'host-filter-grid-'),
                height : 150,
                width : '95%',
                hideHeaders : true,
                columns : [ {
                    reference : 'hostColumn',
                    id : Ext.id(null, 'host-column-'),
                    dataIndex : 'value',
                    flex : 1
                } ],
                selModel : {
                    selType : 'rowmodel',
                    mode : 'MULTI'
                },
                viewConfig : {
                    emptyText : 'None',
                    deferEmptyText : false
                },
                listeners : {
                    selectionchange : 'onHostFiltersChanged'
                },
                bind : {
                    store : '{hostFilterStore}'
                }
            } ],
            flex : 0.25
        };
    },

    /**
     * @method _buildApplicationFilterContainer
     * 
     * Builds the application filter container for this container.
     * 
     * @private
     * @return {Ext.container.Container} The application filter container for this container.
     */
    _buildApplicationFilterContainer : function() {

        return {
            xtype : 'container',
            reference : 'applicationFilterContainer',
            id : Ext.id(null, 'application-filter-container-'),
            layout : 'vbox',
            items : [ {
                xtype : 'label',
                reference : 'applicationFilterLabel',
                id : Ext.id(null, 'application-filter-label-'),
                text : 'Application',
                style : {
                    fontWeight : 'bold'
                }
            }, {
                xtype : 'grid',
                reference : 'applicationFilterGrid',
                id : Ext.id(null, 'application-filter-grid-'),
                height : 150,
                width : '95%',
                hideHeaders : true,
                columns : [ {
                    reference : 'applicationColumn',
                    id : Ext.id(null, 'application-column-'),
                    dataIndex : 'value',
                    flex : 1
                } ],
                selModel : {
                    selType : 'rowmodel',
                    mode : 'MULTI'
                },
                viewConfig : {
                    emptyText : 'None',
                    deferEmptyText : false
                },
                listeners : {
                    selectionchange : 'onApplicationFiltersChanged'
                },
                bind : {
                    store : '{applicationFilterStore}'
                }
            } ],
            flex : 0.25
        };
    },

    /**
     * @method _buildKeyFilterContainer
     * 
     * Builds the key filter container for this container.
     * 
     * @private
     * @return {Ext.container.Container} The key filter container for this container.
     */
    _buildKeyFilterContainer : function() {

        return {
            xtype : 'container',
            reference : 'keyFilterContainer',
            id : Ext.id(null, 'key-filter-container-'),
            layout : 'vbox',
            items : [ {
                xtype : 'label',
                reference : 'keyFilterLabel',
                id : Ext.id(null, 'key-filter-label-'),
                text : 'Key',
                style : {
                    fontWeight : 'bold'
                }
            }, {
                xtype : 'grid',
                reference : 'keyFilterGrid',
                id : Ext.id(null, 'key-filter-grid-'),
                height : 150,
                width : '95%',
                hideHeaders : true,
                columns : [ {
                    reference : 'keyColumn',
                    id : Ext.id(null, 'key-column-'),
                    dataIndex : 'value',
                    flex : 1
                } ],
                selModel : {
                    selType : 'rowmodel',
                    mode : 'MULTI'
                },
                viewConfig : {
                    emptyText : 'None',
                    deferEmptyText : false
                },
                bind : {
                    store : '{keyFilterStore}'
                }
            } ],
            flex : 0.25
        };
    },

    /**
     * @method _buildTimeFilterContainer
     * 
     * Builds the time filter container for this container.
     * 
     * @private
     * @return {Ext.container.Container} The time filter container for this container.
     */
    _buildTimeFilterContainer : function() {

        return {
            xtype : 'container',
            reference : 'timeFilterContainer',
            id : Ext.id(null, 'time-filter-container-'),
            layout : 'vbox',
            items : [ {
                xtype : 'label',
                reference : 'timeFilterLabel',
                id : Ext.id(null, 'time-filter-label-'),
                text : 'Time',
                style : {
                    fontWeight : 'bold'
                }
            }, {
                xtype : 'numberfield',
                reference : 'minTimeField',
                id : Ext.id(null, 'min-time-field-'),
                fieldLabel : 'Min',
                allowExponential : false
            }, {
                xtype : 'numberfield',
                reference : 'maxTimeField',
                id : Ext.id(null, 'max-time-field-'),
                fieldLabel : 'Max',
                allowExponential : false
            } ]
        };
    },

    /**
     * @method _buildLogGrid
     * 
     * Builds the log grid for this container.
     * 
     * @private
     * @return {ETexas.view.log.LogGrid} The log grid for this container.
     */
    _buildLogGrid : function() {

        return {
            region : 'center',
            xtype : 'loggrid',
            reference : 'logGrid',
            id : Ext.id(null, 'log-grid-'),
            listeners : {
                exportlogs : 'exportLogs',
                resetlogs : 'resetLogs',
                searchlogs : 'searchLogs'
            },
            bind : {
                store : '{logStore}'
            }
        };
    }
});
