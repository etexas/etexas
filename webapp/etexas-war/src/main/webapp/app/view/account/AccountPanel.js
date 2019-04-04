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
 * @class ETexas.view.account.AccountPanel
 * @extends Ext.panel.Panel
 * 
 * A panel to manage account information.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.account.AccountPanel', {
    extend : 'Ext.panel.Panel',
    xtype : 'accountpanel',

    requires : [ 'ETexas.view.account.AccountPanelController', 'ETexas.view.account.ChangePasswordForm', 'ETexas.view.account.EditInformationForm' ],

    controller : 'accountpanel',

    layout : {
        type : 'vbox',
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
     * Builds the items for this form panel.
     * 
     * @template
     * @protected
     * @return {Object[]} The items for this form panel.
     */
    buildItems : function() {

        var labelWidth = new Ext.util.TextMetrics().getWidth('Confirm New Password:');

        return [ {
            xtype : 'panel',
            reference : 'informationPanel',
            id : Ext.id(null, 'information-panel-'),
            title : 'Account Information',
            margin : '0, 0, 5, 0',
            layout : 'vbox',
            items : [ {
                xtype : 'editinformationform',
                reference : 'editInformationForm',
                id : Ext.id(null, 'edit-information-form-'),
                defaults : {
                    labelWidth : labelWidth
                }
            } ],
            tools : [ {
                type : 'help',
                reference : 'helpTool',
                id : Ext.id(null, 'help-tool-'),
                callback : 'onHelpToolClicked',
                tooltip : 'Account Information Help'
            } ]
        }, {
            xtype : 'panel',
            reference : 'passwordPanel',
            id : Ext.id(null, 'password-panel-'),
            title : 'Change Password',
            margin : '5, 0, 5, 0',
            layout : 'vbox',
            items : [ {
                xtype : 'changepasswordform',
                reference : 'changePasswordForm',
                id : Ext.id(null, 'change-password-form-'),
                defaults : {
                    labelWidth : labelWidth
                }
            } ],
            tools : [ {
                type : 'help',
                reference : 'helpTool',
                id : Ext.id(null, 'help-tool-'),
                callback : 'onHelpToolClicked',
                tooltip : 'Change Password Help'
            } ]
        }, {
            xtype : 'panel',
            reference : 'deletePanel',
            id : Ext.id(null, 'delete-panel-'),
            margin : '5, 0, 5, 0',
            bodyPadding : 10,
            layout : {
                type : 'vbox',
                align : 'left'
            },
            items : [ {
                xtype : 'button',
                reference : 'deleteButton',
                id : Ext.id(null, 'delete-button-'),
                handler : 'onDeleteButtonClicked',
                text : 'Delete Account'
            } ]
        } ];
    }
});
