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
 * @class ETexas.view.login.RegistrationFormController
 * @extends ETexas.view.model.ModelFormController
 * 
 * The {@link ETexas.view.login.RegistrationForm} controller.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.login.RegistrationFormController', {
    extend : 'ETexas.view.model.ModelFormController',
    alias : 'controller.registrationform',

    /** @inheritdoc */
    init : function(view) {

        view.lookupReference('passwordField').validator = Ext.bind(this.validatePasswordField, this);
        view.lookupReference('confirmPasswordField').validator = Ext.bind(this.validateConfirmPasswordField, this);
    },

    /**
     * @method onHelpToolClicked
     * 
     * Handles the event that is generated when the user clicks the Create an eTEXAS Account Help
     * tool.
     * 
     * @protected
     * @param {Ext.panel.Tool} tool The clicked tool.
     * @param {Ext.event.Event} e The click event.
     * @param {Ext.Component} owner The owner of the tool.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onHelpToolClicked : function(tool, e, owner, eOpts) {

        window.open(ETexas.util.UrlProvider.getHelpContentUrl() + '/Intro Topics/create_account.htm');
    },

    /**
     * @method onRegisterButtonClicked
     * 
     * Handles the event that is generated when the user clicks the Register button.
     * 
     * @protected
     * @param {Ext.button.Button} button The clicked button.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onRegisterButtonClicked : function(button, e, eOpts) {

        var view = this.getView();

        if (view.isValid()) {

            view.mask('Registering User...');
            view.submit({
                scope : this,
                method : 'POST',
                url : ETexas.util.UrlProvider.getUsersUrl(),
                success : function(form, action) {

                    var message = 'A verification email has been sent to \"' + view.lookupReference('emailField').getValue();
                    message = message.concat('\". Please follow the provided instructions to verify your email and complete the registration.');

                    Ext.destroy(view);
                    Ext.Msg.alert('Email Verification', message);
                },
                failure : function(form, action) {

                    view.unmask();
                }
            });
        }
    },

    /**
     * @method validatePasswordField
     * 
     * Validates the current value of the password field.
     * 
     * @protected
     * @param {String} value The current field value.
     * @return {Boolean/String} True if the current field value is valid, otherwise a string
     * validation message is returned.
     */
    validatePasswordField : function(value) {

        this.getView().lookupReference('confirmPasswordField').validate();
        return ETexas.util.Validation.validatePassword(value);
    },

    /**
     * @method validateConfirmPasswordField
     * 
     * Validates the current value of the confirm password field.
     * 
     * @protected
     * @param {String} value The current field value.
     * @return {Boolean/String} True if the current field value is valid, otherwise a string
     * validation message is returned.
     */
    validateConfirmPasswordField : function(value) {

        var passwordField = this.getView().lookupReference('passwordField');

        if (passwordField.getValue() !== value) {

            return 'The entered value does not match the entered password.';
        }

        return true;
    }
});
