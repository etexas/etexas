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
 * @class ETexas.view.message.MessageWindow
 * @extends ETexas.view.window.BasicWindow
 * 
 * A window to view the messages for a selected {@link ETexas.model.DeviceModel}.
 * 
 * @author ttevendale
 */
Ext.define('ETexas.view.message.MessageWindow', {
    extend : 'ETexas.view.window.BasicWindow',
    xtype : 'messagewindow',

    requires : [ 'ETexas.view.message.MessageGrid', 'ETexas.view.message.MessageWindowController', 'ETexas.view.message.MessageWindowModel', 'Ext.grid.feature.Grouping' ],

    controller : 'messagewindow',

    viewModel : {
        type : 'messagewindow'
    },

    title : 'Messages',

    layout : 'fit',
    width : 1024,
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
            xtype : 'messagegrid',
            reference : 'messageGrid',
            id : Ext.id(null, 'message-grid-'),
            bind : '{messageStore}',
            emptyText : 'There are no messages for this device.',
            features : [ {
                ftype : 'grouping',
                groupHeaderTpl : '{name}'
            } ]
        } ];
    },

    /** @inheritdoc */
    buildTools : function() {

        return Ext.Array.insert(this.callParent(), 0, [ {
            type : 'help',
            reference : 'helpTool',
            id : Ext.id(null, 'help-tool-'),
            callback : 'onHelpToolClicked',
            tooltip : 'Messages Help'
        } ]);
    }
});
