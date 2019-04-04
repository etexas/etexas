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
 * @class ETexas.view.message.ExecutionMessageWindowController
 * @extends ETexas.view.window.BasicWindowController
 * 
 * The {@link ETexas.view.message.ExecutionMessageWindow} controller.
 * 
 * @author ttevendale
 */
Ext.define('ETexas.view.message.ExecutionMessageWindowController', {
    extend : 'ETexas.view.window.BasicWindowController',
    alias : 'controller.executionmessagewindow',

    /** @inheritdoc */
    init : function(view) {

        var me = this;
        var viewModel = this.getViewModel();

        /*
         * TODO: emyers - It is uncertain why the call to notify is necessary, but chained stores
         * are not loading without it at the moment. This should be revisited and, hopefully,
         * removed once the issue is resolved.
         */

        viewModel.notify();

        viewModel.get('executionMessageStore').on('datachanged', this._onExecutionMessageDataChanged, this);

        this.callParent([ view ]);
    },

    /**
     * @method _onExecutionMessageDataChanged
     * 
     * Handles the event that is generated when the store's data changes.
     * 
     * @private
     * @param {Ext.store.Store} store The store that had its data changed.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    _onExecutionMessageDataChanged : function(store, eOpts) {

        var selectedComposite = this.getViewModel().get('selectedComposite');
        var simulations = selectedComposite.getSimulations();
        store.each(function(record) {

            var simulation = simulations.getById(record.get('simulationId'));
            record.set('simulationName', simulation.get('name'));
        });
    },

    /**
     * @method onHelpToolClicked
     * 
     * Handles the event that is generated when the user clicks the Execution Message Help tool.
     * 
     * @protected
     * @param {Ext.panel.Tool} tool The clicked tool.
     * @param {Ext.event.Event} e The click event.
     * @param {Ext.Component} owner The owner of the tool.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onHelpToolClicked : function(tool, e, owner, eOpts) {

        window.open(ETexas.util.UrlProvider.getHelpContentUrl() + '/Executions/exec_messages.htm');
    }
});
