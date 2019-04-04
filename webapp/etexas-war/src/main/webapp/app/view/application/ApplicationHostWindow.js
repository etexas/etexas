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
 * @class ETexas.view.application.ApplicationHostWindow
 * @extends ETexas.view.window.BasicWindow
 * 
 * A window to view hosted {@link ETexas.model.ApplicationModel} data.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.application.ApplicationHostWindow', {
    extend : 'ETexas.view.window.BasicWindow',
    xtype : 'applicationhostwindow',

    requires : [ 'ETexas.view.application.ApplicationHostWindowController', 'ETexas.view.application.ApplicationHostWindowModel', 'ETexas.view.application.ApplicationParameterGrid',
            'ETexas.view.application.ApplicationParameterToolbar', 'ETexas.view.grid.column.NameColumn' ],

    controller : 'applicationhostwindow',

    viewModel : {
        type : 'applicationhostwindow'
    },

    title : 'Hosted Applications',
    width : 756,
    height : 512,

    layout : {
        type : 'hbox',
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
     * Builds the items for this window.
     * 
     * @template
     * @protected
     * @return {Object[]} The items for this window.
     */
    buildItems : function() {

        return [ this._buildAvailableGridPanel(), this._buildControlContainer(), this._buildHostedGridPanel(), this._buildParametersPanel() ];
    },

    /**
     * @method _buildAvailableGridPanel
     * 
     * Builds the available grid panel for this window.
     * 
     * @private
     * @return {Ext.grid.Panel} The available grid panel for this window.
     */
    _buildAvailableGridPanel : function() {

        return {
            xtype : 'grid',
            reference : 'availableGrid',
            id : Ext.id(null, 'available-grid-'),
            cls : 'border-gray',
            margin : '5',
            columns : [ {
                xtype : 'namecolumn',
                reference : 'availableColumn',
                id : Ext.id(null, 'available-column-'),
                dataIndex : 'name',
                text : 'Available',
                flex : 1
            } ],
            flex : 6,
            bind : {
                store : '{availableStore}',
                selection : '{selectedApplicationProfile}'
            }
        };
    },

    /**
     * @method _buildControlContainer
     * 
     * Builds the control container for this window.
     * 
     * @private
     * @return {Ext.container.Container} The control container for this window.
     */
    _buildControlContainer : function() {

        return {
            xtype : 'container',
            reference : 'controlContainer',
            id : Ext.id(null, 'control-container-'),
            layout : {
                type : 'vbox',
                align : 'center',
                pack : 'center'
            },
            items : [ {
                xtype : 'button',
                reference : 'addButton',
                id : Ext.id(null, 'add-button-'),
                handler : 'onAddButtonClicked',
                margin : '0 0 2 0',
                text : '>',
                bind : {
                    disabled : '{!selectedApplicationProfile}'
                }
            }, {
                xtype : 'button',
                reference : 'removeButton',
                id : Ext.id(null, 'remove-button-'),
                handler : 'onRemoveButtonClicked',
                margin : '2 0 0 0',
                text : '<'
            } ],
            flex : 1
        };
    },

    /**
     * @method _buildHostedGridPanel
     * 
     * Builds the hosted grid panel for this window.
     * 
     * @private
     * @return {Ext.grid.Panel} The hosted grid panel for this window.
     */
    _buildHostedGridPanel : function() {

        return {
            xtype : 'grid',
            reference : 'hostedGrid',
            id : Ext.id(null, 'hosted-grid-'),
            cls : 'border-gray',
            margin : '5',
            columns : [ {
                xtype : 'namecolumn',
                reference : 'hostedColumn',
                id : Ext.id(null, 'hosted-column-'),
                dataIndex : 'name',
                text : 'Hosted',
                flex : 1
            } ],
            flex : 6,
            selModel : {
                selType : 'rowmodel',
                mode : 'MULTI'
            },
            bind : {
                store : '{hostedStore}'
            },
            listeners : {
                selectionchange : 'onApplicationSelectionChange'
            }
        };
    },

    /**
     * @method _buildParametersPanel
     * 
     * Builds the parameters panel for this window.
     * 
     * @private
     * @return {Ext.panel.Panel} The parameters panel for this window.
     */
    _buildParametersPanel : function() {

        return {
            xtype : 'panel',
            reference : 'parametersPanel',
            id : Ext.id(null, 'parameters-panel-'),
            title : 'Parameters',
            cls : 'border-gray',
            margin : '5',
            flex : 8,
            items : [ {
                xtype : 'applicationparametergrid',
                reference : 'applicationParameterGrid',
                id : Ext.id(null, 'application-parameter-grid-'),
                bind : {
                    store : '{parameterStore}',
                    selection : '{selectedParameter}'
                }
            } ],
            dockedItems : [ {
                xtype : 'applicationparametertoolbar',
                reference : 'applicationParameterToolbar',
                id : Ext.id(null, 'application-parameter-toolbar-'),
                dock : 'top'
            } ],
            listeners : {
                editmodel : 'showEditApplicationParameterForm'
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
            tooltip : 'Hosted Applications'
        } ]);
    }
});
