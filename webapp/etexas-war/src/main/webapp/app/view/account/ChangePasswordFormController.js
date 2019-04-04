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
 * @class ETexas.view.account.ChangePasswordFormController
 * @extends Ext.app.ViewController
 * 
 * The {@link ETexas.view.account.ChangePasswordForm} controller.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.account.ChangePasswordFormController', {
    extend : 'Ext.app.ViewController',
    alias : 'controller.changepasswordform',

    /** @inheritdoc */
    init : function(view) {

        view.enableBubble(this.getBubbleEvents());
        view.lookupReference('newPasswordField').validator = Ext.bind(this.validateNewPasswordField, this);
        view.lookupReference('confirmNewPasswordField').validator = Ext.bind(this.validateConfirmNewPasswordField, this);
    },

    /**
     * @method getBubbleEvents
     * 
     * Returns the bubble events for this form panel.
     * 
     * @template
     * @protected
     * @return {String[]} The bubble events for this form panel.
     */
    getBubbleEvents : function() {

        return [ 'passwordchanged' ];
    },

    /**
     * @method onResetButtonClicked
     * 
     * Handles the event that is generated when the user clicks the Reset button.
     * 
     * @protected
     * @param {Ext.button.Button} button The clicked button.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onResetButtonClicked : function(button, e, eOpts) {

        this.getView().reset();
    },

    /**
     * @method onUpdateButtonClicked
     * 
     * Handles the event that is generated when the user clicks the Update button.
     * 
     * @protected
     * @param {Ext.button.Button} button The clicked button.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onUpdateButtonClicked : function(button, e, eOpts) {

        var view = this.getView();

        if (view.isValid()) {

            view.submit({
                scope : this,
                method : 'PUT',
                url : ETexas.util.UrlProvider.getCurrentUserPasswordUrl(),
                success : function(form, action) {

                    view.reset();
                    view.fireEvent('passwordchanged');
                }
            });
        }
    },

    /**
     * @method validateNewPasswordField
     * 
     * Validates the current value of the new password field.
     * 
     * @protected
     * @param {String} value The current field value.
     * @return {Boolean/String} True if the current field value is valid, otherwise a string
     * validation message is returned.
     */
    validateNewPasswordField : function(value) {

        this.getView().lookupReference('confirmNewPasswordField').validate();
        return ETexas.util.Validation.validatePassword(value);
    },

    /**
     * @method validateConfirmNewPasswordField
     * 
     * Validates the current value of the confirm new password field.
     * 
     * @protected
     * @param {String} value The current field value.
     * @return {Boolean/String} True if the current field value is valid, otherwise a string
     * validation message is returned.
     */
    validateConfirmNewPasswordField : function(value) {

        var passwordField = this.getView().lookupReference('newPasswordField');

        if (passwordField.getValue() !== value) {

            return 'The entered value does not match the entered password.';
        }

        return true;
    }
});
