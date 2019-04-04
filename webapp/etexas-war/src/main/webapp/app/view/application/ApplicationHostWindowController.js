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
 * @class ETexas.view.application.ApplicationHostWindowController
 * @extends ETexas.view.window.BasicWindowController
 * 
 * The {@link ETexas.view.application.ApplicationHostWindow} controller.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.application.ApplicationHostWindowController', {
    extend : 'ETexas.view.window.BasicWindowController',
    alias : 'controller.applicationhostwindow',

    requires : [ 'ETexas.view.application.EditApplicationParameterForm' ],

    /** @inheritdoc */
    init : function(view) {

        var viewModel = this.getViewModel();
        var parameterToolbar = view.lookupReference('applicationParameterToolbar');
        this.getView().lookupReference('removeButton').setDisabled(true);

        viewModel.bind('{selectedParameter}', function(selectedParameter) {
            parameterToolbar.lookupReference('editButton').setDisabled(!selectedParameter);
        });
    },

    /**
     * @method onAddButtonClicked
     * 
     * Handles the event that is generated when the user clicks the > button.
     * 
     * @protected
     * @param {Ext.button.Button} button The clicked button.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onAddButtonClicked : function(button, e, eOpts) {

        var me = this;
        var viewModel = me.getViewModel();

        Ext.Ajax.request({
            scope : this,
            method : 'POST',
            waitMsg : 'Adding application...',
            url : viewModel.get('applicationsUrl'),
            params : {
                applicationProfileId : viewModel.get('selectedApplicationProfile').get('id')
            },
            success : function(response, opts) {
                var application = Ext.JSON.decode(response.responseText);
                viewModel.set('selectedApplicationProfile', null);
                me._updateHostedStore(application.id);
            }
        });
    },

    /**
     * @method onApplicationSelectionChange
     * 
     * Handles the event that is generated when the user changes the selected application(s).
     * 
     * @protected
     * @param {Ext.selection.Model} model The selection model.
     * @param {Ext.data.Model[]} selected The selected applications.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onApplicationSelectionChange : function(model, selected, eOpts) {

        this.getViewModel().set('selectedApplication', selected.length === 1 ? selected[0] : null);
        this.getView().lookupReference('removeButton').setDisabled(selected.length === 0);
    },

    /**
     * @method onHelpToolClicked
     * 
     * Handles the event that is generated when the user clicks the Hosted Applications Help tool.
     * 
     * @protected
     * @param {Ext.panel.Tool} tool The clicked tool.
     * @param {Ext.event.Event} e The click event.
     * @param {Ext.Component} owner The owner of the tool.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onHelpToolClicked : function(tool, e, owner, eOpts) {

        window.open(ETexas.util.UrlProvider.getHelpContentUrl() + '/Simulations/Composites/hosted_apps.htm');
    },

    /**
     * @method onHostedDataChanged
     * 
     * Handles the event that is generated when the hosted application data changes.
     * 
     * @protected
     * @param {Ext.data.Store} store The changed data store.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onHostedDataChanged : function(store, eOpts) {

        var viewModel = this.getViewModel();
        var availableStore = viewModel.get('availableStore');

        availableStore.filterBy(function(record) {

            var matches = store.query('name', record.get('name'), false, false, true).length;
            var canHost = record.get('deviceType') === viewModel.get('hostType');
            return matches === 0 && canHost;

        }, this);
    },

    /**
     * @method onRemoveButtonClicked
     * 
     * Handles the event that is generated when the user clicks the < button.
     * 
     * @protected
     * @param {Ext.button.Button} button The clicked button.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onRemoveButtonClicked : function(button, e, eOpts) {

        var me = this;
        var viewModel = me.getViewModel();
        var selectedApplications = this.getView().lookupReference('hostedGrid').getSelection();
        var waitMessage = (selectedApplications.length === 1) ? 'Removing Application...' : 'Removing Applications...';

        var applicationIds = [];
        selectedApplications.forEach(function(value, index, array) {
            applicationIds.push(value.get('id'));
        });

        var queryParams = Ext.Object.toQueryString({
            applicationIds : applicationIds
        });

        Ext.Ajax.request({
            scope : this,
            method : 'DELETE',
            waitMsg : waitMessage,
            url : viewModel.get('applicationsUrl') + '?' + queryParams,
            success : function(response, opts) {
                me._updateHostedStore();
            }
        });
    },

    /**
     * @method showEditApplicationParameterForm
     * 
     * Shows the form to edit the selected {@link ETexas.model.ApplicationParameterModel}.
     * 
     * @protected
     */
    showEditApplicationParameterForm : function() {

        var form = this.getView().add({
            xtype : 'editapplicationparameterform',
            id : Ext.id(null, 'edit-application-parameter-form-')
        });

        var me = this;
        var viewModel = this.getViewModel();
        form.on('applicationparameterupdated', function(applicationParameterId) {
            me._updateHostedStore(viewModel.get('selectedApplication').get('id'));
        });

        form.show();
    },

    /**
     * @method _updateHostedStore
     * 
     * Updates the application data for the selected application host.
     * 
     * @private
     * @param {Number} [applicationId] The ID of the selected application.
     */
    _updateHostedStore : function(applicationId) {

        var viewModel = this.getViewModel();
        var hostedStore = viewModel.get('hostedStore');
        var hostedGrid = this.getView().lookupReference('hostedGrid');
        hostedStore.load(function(records, operation, success) {
            var selectedApplication = applicationId ? hostedStore.getById(applicationId) : null;
            hostedGrid.setSelection(selectedApplication);
            viewModel.set('selectedApplication', selectedApplication);
        });
    }
});
