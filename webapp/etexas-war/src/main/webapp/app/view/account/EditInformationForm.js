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
 * @class ETexas.view.account.EditInformationForm
 * @extends Ext.form.Panel
 * 
 * A form panel to edit account information.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.account.EditInformationForm', {
    extend : 'Ext.form.Panel',
    xtype : 'editinformationform',

    requires : [ 'ETexas.view.account.EditInformationFormController' ],

    controller : 'editinformationform',

    trackResetOnLoad : true,
    bodyPadding : 10,

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
            xtype : 'textfield',
            reference : 'usernameField',
            id : Ext.id(null, 'username-field-'),
            name : 'username',
            fieldLabel : 'Username',
            allowOnlyWhitespace : false,
            blankText : 'A valid username is required.',
            regex : /^[a-zA-Z]+[a-zA-Z0-9]*$/,
            regexText : 'Usernames must start with a valid letter and may only contain letters and digits.'
        }, {
            xtype : 'textfield',
            reference : 'firstNameField',
            id : Ext.id(null, 'first-name-field-'),
            name : 'firstName',
            fieldLabel : 'First Name',
            allowOnlyWhitespace : false,
            blankText : 'A valid first name is required.'
        }, {
            xtype : 'textfield',
            reference : 'lastNameField',
            id : Ext.id(null, 'last-name-field-'),
            name : 'lastName',
            fieldLabel : 'Last Name',
            allowOnlyWhitespace : false,
            blankText : 'A valid last name is required.'
        }, {
            xtype : 'textfield',
            reference : 'organizationField',
            id : Ext.id(null, 'organization-field-'),
            name : 'organization',
            fieldLabel : 'Organization'
        }, {
            xtype : 'textfield',
            reference : 'emailField',
            id : Ext.id(null, 'email-field-'),
            name : 'email',
            fieldLabel : 'Email Address',
            vtype : 'email',
            allowOnlyWhitespace : false,
            blankText : 'A valid email address is required.'
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
            dock : 'bottom',
            layout : {
                pack : 'end'
            },
            items : [ {
                reference : 'updateButton',
                id : Ext.id(null, 'update-button-'),
                handler : 'onUpdateButtonClicked',
                text : 'Update'
            }, {
                reference : 'resetButton',
                id : Ext.id(null, 'reset-button-'),
                handler : 'onResetButtonClicked',
                text : 'Reset'
            } ]
        } ];
    }
});
