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
 * @class ETexas.view.account.EditInformationFormController
 * @extends Ext.app.ViewController
 * 
 * The {@link ETexas.view.account.EditInformationForm} controller.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.account.EditInformationFormController', {
    extend : 'Ext.app.ViewController',
    alias : 'controller.editinformationform',

    /** @inheritdoc */
    init : function(view) {

        this._updateInformation();
        view.enableBubble(this.getBubbleEvents());
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

        return [ 'usernamechanged' ];
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

        var me = this;
        var view = me.getView();

        if (view.isValid()) {

            view.submit({
                scope : this,
                method : 'PUT',
                url : ETexas.util.UrlProvider.getCurrentUserUrl(),
                success : function(form, action) {

                    var username = view.lookupReference('usernameField').getValue();

                    if (ETexas.util.Config.getUser() !== username) {

                        view.fireEvent('usernamechanged');
                    }
                    else {

                        me._updateInformation();
                        var message = 'The information for the account has been successfully updated.';
                        Ext.Msg.alert('Information Updated', message);
                    }
                }
            });
        }
    },

    /**
     * @method _updateInformation
     * 
     * Updates the information for the current user.
     * 
     * @private
     */
    _updateInformation : function() {

        var view = this.getView();

        Ext.Ajax.request({
            scope : this,
            url : ETexas.util.UrlProvider.getCurrentUserUrl(),
            success : function(response, opts) {

                var user = Ext.JSON.decode(response.responseText);

                view.getForm().setValues({
                    username : user.username,
                    firstName : user.firstName,
                    lastName : user.lastName,
                    organization : user.organization,
                    email : user.email
                });
            }
        });
    }
});
