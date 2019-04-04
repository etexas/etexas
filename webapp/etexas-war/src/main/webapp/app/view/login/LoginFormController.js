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
 * @class ETexas.view.login.LoginFormController
 * @extends Ext.app.ViewController
 * 
 * The {@link ETexas.view.login.LoginForm} controller.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.login.LoginFormController', {
    extend : 'Ext.app.ViewController',
    alias : 'controller.loginform',

    /** @inheritdoc */
    init : function(view) {

        view.enableBubble('login');
    },

    /**
     * @method onLoginButtonClicked
     * 
     * Handles the event that is generated when the user clicks the Login button.
     * 
     * @protected
     * @param {Ext.button.Button} button The clicked button.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onLoginButtonClicked : function(button, e, eOpts) {

        var view = this.getView();

        if (view.isValid()) {

            view.lookupReference('sessionExpiredLabel').hide();

            view.submit({
                scope : this,
                method : 'POST',
                url : ETexas.util.UrlProvider.getLoginUrl(),
                success : function(form, action) {

                    var response = Ext.JSON.decode(action.response.responseText);
                    view.fireEvent('login', response.username, response.token);
                }
            });
        }
    },

    /**
     * @method onRegistrationFieldRendered
     * 
     * Handles the initial setup for click events after the registration field is rendered.
     * 
     * @protected
     * @param {Ext.Component} component The rendered component.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onRegistrationFieldRendered : function(component, eOpts) {

        component.getEl().on('click', function(e, t, eOpts) {

            var registrationForm = Ext.create('ETexas.view.login.RegistrationForm', {
                id : Ext.id(null, 'registration-form-')
            });

            registrationForm.show();
        });
    },

    /**
     * @method onRecoverFieldRendered
     * 
     * Handles the initial setup for click events after the recover field is rendered.
     * 
     * @protected
     * @param {Ext.Component} component The rendered component.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onRecoverFieldRendered : function(component, eOpts) {

        component.getEl().on('click', function(e, t, eOpts) {

            var recoverForm = Ext.create('ETexas.view.login.RecoveryForm', {
                id : Ext.id(null, 'recover-form-')
            });

            recoverForm.show();
        });
    }
});
