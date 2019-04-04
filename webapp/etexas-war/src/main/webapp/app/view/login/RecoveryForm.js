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
 * @class ETexas.view.login.RecoveryForm
 * @extends ETexas.view.model.ModelForm
 * 
 * A form panel to recover forgotten username/password information.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.login.RecoveryForm', {
    extend : 'ETexas.view.model.ModelForm',
    xtype : 'recoveryform',

    requires : [ 'ETexas.view.login.RecoveryFormController' ],

    controller : 'recoveryform',

    title : 'Recover Username/Password',

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

        return [ {
            xtype : 'textfield',
            reference : 'emailField',
            id : Ext.id(null, 'email-field-'),
            fieldLabel : 'Email',
            name : 'email',
            minWidth : 350,
            vtype : 'email',
            allowOnlyWhitespace : false,
            blankText : 'A valid email address is required.'
        }, {
            xtype : 'radiogroup',
            reference : 'recoveryTypeGroup',
            id : Ext.id(null, 'recovery-type-group-'),
            fieldLabel : 'Recover',
            vertical : true,
            columns : 1,
            items : [ {
                reference : 'usernameTypeField',
                id : Ext.id(null, 'username-type-field-'),
                boxLabel : 'Username',
                name : 'recoveryType',
                inputValue : 'username',
                checked : true
            }, {
                reference : 'passwordTypeField',
                id : Ext.id(null, 'password-type-field-'),
                boxLabel : 'Password',
                name : 'recoveryType',
                inputValue : 'password'
            } ]
        } ];
    },

    /** @inheritdoc */
    buildButtons : function() {

        return Ext.Array.insert(this.callParent(), 0, [ {
            reference : 'recoverButton',
            id : Ext.id(null, 'recover-button-'),
            handler : 'onRecoverButtonClicked',
            text : 'Recover'
        } ]);
    },

    /** @inheritdoc */
    buildTools : function() {

        return Ext.Array.insert(this.callParent(), 0, [ {
            type : 'help',
            reference : 'helpTool',
            id : Ext.id(null, 'help-tool-'),
            callback : 'onHelpToolClicked',
            tooltip : 'Recover Username/Password Help'
        } ]);
    }
});
