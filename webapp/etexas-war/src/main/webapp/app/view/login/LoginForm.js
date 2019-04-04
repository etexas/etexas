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
 * @class ETexas.view.login.LoginForm
 * @extends Ext.form.Panel
 * 
 * A form panel to log the current user into the application.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.login.LoginForm', {
    extend : 'Ext.form.Panel',
    xtype : 'loginform',
    cls : 'login-form',

    requires : [ 'ETexas.view.login.LoginFormController' ],

    controller : 'loginform',

    title : 'Login to eTEXAS',
    defaultButton : 'loginButton',
    bodyPadding : 15,
    width : 300,

    layout : {
        type : 'vbox',
        align : 'stretch'
    },

    /** @inheritdoc */
    initComponent : function() {

        this.items = this.buildItems();
        this.dockedItems = this.buildDockedItems();
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
            xtype : 'label',
            reference : 'sessionExpiredLabel',
            id : Ext.id(null, 'session-expired-label-'),
            text : 'Your session has expired.',
            hidden : !sessionStorage.getItem('authInvalidFlag'),
            margin : '0, 0, 10, 0',
            cls : 'red'
        }, {
            xtype : 'textfield',
            reference : 'usernameField',
            id : Ext.id(null, 'username-field-'),
            name : 'username',
            fieldLabel : 'Username',
            allowOnlyWhitespace : false,
            blankText : 'A valid username is required.'
        }, {
            xtype : 'textfield',
            reference : 'passwordField',
            id : Ext.id(null, 'password-field-'),
            inputType : 'password',
            name : 'password',
            fieldLabel : 'Password',
            allowOnlyWhitespace : false,
            blankText : 'A valid password is required.'

        }, {
            xtype : 'container',
            reference : 'recoverContainer',
            id : Ext.id(null, 'recover-container-'),
            layout : {
                type : 'vbox',
                align : 'end'
            },
            items : [ {
                xtype : 'displayfield',
                reference : 'recoverField',
                id : Ext.id(null, 'recover-field-'),
                value : '<a href="#">Forgot username/password?</a>',
                listeners : {
                    afterrender : 'onRecoverFieldRendered'
                }
            } ]
        } ];
    },

    /**
     * @method buildDockedItems
     * 
     * Builds the docked items for this form panel.
     * 
     * @template
     * @protected
     * @return {Object[]} The docked items for this form panel.
     */
    buildDockedItems : function() {

        return [ {
            xtype : 'toolbar',
            reference : 'footerToolbar',
            id : Ext.id(null, 'footer-toolbar-'),
            dock : 'bottom',
            ui : 'footer',
            items : [ {
                xtype : 'displayfield',
                reference : 'registrationField',
                id : Ext.id(null, 'registration-field-'),
                value : '<a href="#">Need an account? Sign up.</a>',
                listeners : {
                    afterrender : 'onRegistrationFieldRendered'
                }
            }, '->', {
                xtype : 'button',
                reference : 'loginButton',
                id : Ext.id(null, 'login-button-'),
                handler : 'onLoginButtonClicked',
                text : 'Login'
            } ]
        } ];
    }
});
