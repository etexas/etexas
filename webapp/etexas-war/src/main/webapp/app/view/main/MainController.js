/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
/**
 * @class ETexas.view.main.MainController
 * @extends Ext.app.ViewController
 * 
 * The {@link ETexas.view.main.Main} controller.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.main.MainController', {
    extend : 'Ext.app.ViewController',
    alias : 'controller.main',

    /** @inheritdoc */
    init : function(view) {

        var me = this;

        Ext.Ajax.request({
            scope : this,
            url : ETexas.util.UrlProvider.getVersionUrl(),
            success : function(response, opts) {

                var versioning = Ext.JSON.decode(response.responseText);
                me.lookupReference('titleField').setValue(versioning.title);
                me.lookupReference('versionField').setValue(versioning.version);
                me.lookupReference('buildField').setValue(versioning.build);
            }
        });

        me.lookupReference('userButton').setText(ETexas.util.Config.getUser());
        me.lookupReference('perspectivesContainer').setActiveItem(me.lookupReference('simulationsPerspective'));
    },

    /**
     * @method onAccountDeleted
     * 
     * Handles the event that is generated when the user deletes their account.
     * 
     * @protected
     */
    onAccountDeleted : function() {

        this._logout();
    },

    /**
     * @method onHelpButtonClicked
     * 
     * Handles the event that is generated when the user clicks the Help button.
     * 
     * @protected
     * @param {Ext.button.Button} button The clicked button.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onHelpButtonClicked : function(button, e, eOpts) {

        window.open(ETexas.util.UrlProvider.getHelpContentUrl() + '/Home.htm');
    },

    /**
     * @method onLogOutItemClicked
     * 
     * Handles the event that is generated when the user clicks the Log Out menu item.
     * 
     * @protected
     * @param {Ext.menu.Item} item The clicked menu item.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onLogOutItemClicked : function(item, e, eOpts) {

        this._logout();
    },

    /**
     * @method onPasswordChanged
     * 
     * Handles the event that is generated when the user changes their password.
     * 
     * @protected
     */
    onPasswordChanged : function() {

        var me = this;
        var message = 'The password for the account has been successfully updated. Please login with the new password.';
        Ext.Msg.alert('Password Updated', message, function() {
            me._logout();
        });
    },

    /**
     * @method onUsernameChanged
     * 
     * Handles the event that is generated when the user changes their username.
     * 
     * @protected
     */
    onUsernameChanged : function() {

        var me = this;
        var message = 'The information for the account has been successfully updated. Please login with the new username.';
        Ext.Msg.alert('Information Updated', message, function() {
            me._logout();
        });
    },

    /**
     * @method _logout
     * 
     * Logs the current user out of the application.
     * 
     * @private
     */
    _logout : function() {

        Ext.Ajax.request({
            method : 'POST',
            url : ETexas.util.UrlProvider.getLogoutUrl()
        });

        sessionStorage.removeItem('user');
        sessionStorage.removeItem('token');
        ETexas.util.Config.setUser('log');
        ETexas.util.Config.setToken('out');
        window.location.reload();
    },

    /**
     * @method onAccountItemClicked
     * 
     * Handles the event that is generated when the user clicks the Manage Account item.
     * 
     * @protected
     * @param {Ext.menu.Item} item The clicked menu item.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onAccountItemClicked : function(item, e, eOpts) {

        this.lookupReference('applicationsButton').toggle(false);
        this.lookupReference('executionsButton').toggle(false);
        this.lookupReference('simulationsButton').toggle(false);
        this.lookupReference('perspectivesContainer').setActiveItem(this.lookupReference('accountPanel'));
    },

    /**
     * @method onApplicationsButtonToggled
     * 
     * Handles the event that is generated when the user toggles the Applications button.
     * 
     * @protected
     * @param {Ext.button.Button} button The toggled button.
     * @param {Boolean} pressed The pressed status.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onApplicationsButtonToggled : function(button, pressed, eOpts) {

        if (pressed) {

            this.lookupReference('perspectivesContainer').setActiveItem(this.lookupReference('applicationsPerspective'));
        }
    },

    /**
     * @method onExecutionsButtonToggled
     * 
     * Handles the event that is generated when the user toggles the Executions button.
     * 
     * @protected
     * @param {Ext.button.Button} button The toggled button.
     * @param {Boolean} pressed The pressed status.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onExecutionsButtonToggled : function(button, pressed, eOpts) {

        if (pressed) {

            this.lookupReference('perspectivesContainer').setActiveItem(this.lookupReference('executionsPerspective'));
        }
    },

    /**
     * @method onSimulationsButtonToggled
     * 
     * Handles the event that is generated when the user toggles the Simulations button.
     * 
     * @protected
     * @param {Ext.button.Button} button The toggled button.
     * @param {Boolean} pressed The pressed status.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onSimulationsButtonToggled : function(button, pressed, eOpts) {

        if (pressed) {

            this.lookupReference('perspectivesContainer').setActiveItem(this.lookupReference('simulationsPerspective'));
        }
    }
});