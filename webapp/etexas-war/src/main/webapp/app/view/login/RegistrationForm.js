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
 * @class ETexas.view.login.RegistrationForm
 * @extends ETexas.view.model.ModelForm
 * 
 * A form panel to register a new user.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.login.RegistrationForm', {
    extend : 'ETexas.view.model.ModelForm',
    xtype : 'registrationform',

    requires : [ 'ETexas.view.login.RegistrationFormController' ],

    controller : 'registrationform',

    title : 'Create an eTEXAS Account',
    defaultButton : 'registerButton',

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

        var labelWidth = new Ext.util.TextMetrics().getWidth('Confirm Password:');

        return [ {
            xtype : 'textfield',
            reference : 'usernameField',
            id : Ext.id(null, 'username-field-'),
            name : 'username',
            fieldLabel : 'Username',
            labelWidth : labelWidth,
            allowOnlyWhitespace : false,
            blankText : 'A valid username is required.',
            regex : /^[a-zA-Z]+[a-zA-Z0-9]*$/,
            regexText : 'Usernames must start with a valid letter and may only contain letters and digits.',
            minWidth : 500
        }, {
            xtype : 'textfield',
            reference : 'passwordField',
            id : Ext.id(null, 'password-field-'),
            inputType : 'password',
            name : 'password',
            fieldLabel : 'Password',
            labelWidth : labelWidth
        }, {
            xtype : 'textfield',
            reference : 'confirmPasswordField',
            id : Ext.id(null, 'confirm-password-field-'),
            inputType : 'password',
            fieldLabel : 'Confirm Password',
            labelWidth : labelWidth
        }, {
            xtype : 'textfield',
            reference : 'emailField',
            id : Ext.id(null, 'email-field-'),
            name : 'email',
            fieldLabel : 'Email Address',
            labelWidth : labelWidth,
            vtype : 'email',
            allowOnlyWhitespace : false,
            blankText : 'A valid email address is required.'
        }, {
            xtype : 'textfield',
            reference : 'firstNameField',
            id : Ext.id(null, 'first-name-field-'),
            name : 'firstName',
            fieldLabel : 'First Name',
            labelWidth : labelWidth,
            allowOnlyWhitespace : false,
            blankText : 'A valid first name is required.'
        }, {
            xtype : 'textfield',
            reference : 'lastNameField',
            id : Ext.id(null, 'last-name-field-'),
            name : 'lastName',
            fieldLabel : 'Last Name',
            labelWidth : labelWidth,
            allowOnlyWhitespace : false,
            blankText : 'A valid last name is required.'
        }, {
            xtype : 'textfield',
            reference : 'organizationField',
            id : Ext.id(null, 'organization-field-'),
            name : 'organization',
            fieldLabel : 'Organization',
            labelWidth : labelWidth
        } ];
    },

    /** @inheritdoc */
    buildButtons : function() {

        return Ext.Array.insert(this.callParent(), 0, [ {
            reference : 'registerButton',
            id : Ext.id(null, 'register-button-'),
            handler : 'onRegisterButtonClicked',
            text : 'Register'
        } ]);
    },

    /** @inheritdoc */
    buildTools : function() {

        return Ext.Array.insert(this.callParent(), 0, [ {
            type : 'help',
            reference : 'helpTool',
            id : Ext.id(null, 'help-tool-'),
            callback : 'onHelpToolClicked',
            tooltip : 'Create an eTEXAS Account Help'
        } ]);
    }
});
