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
 * @class ETexas.view.log.LogContainerController
 * @extends Ext.app.ViewController
 * 
 * The {@link ETexas.view.log.LogContainer} controller.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.log.LogContainerController', {
    extend : 'Ext.app.ViewController',
    alias : 'controller.logcontainer',

    /** @inheritdoc */
    init : function(view) {

        var viewModel = this.getViewModel();
        viewModel.notify();

        viewModel.bind('{selectedExecution}', this._updateFilters, this);
        view.lookupReference('minTimeField').validator = Ext.bind(this.validateMinTimeField, this);
        view.lookupReference('maxTimeField').validator = Ext.bind(this.validateMaxTimeField, this);
        viewModel.get('logStore').on('datachanged', function(store, eOpts) {

            var logGrid = view.lookupReference('logGrid');
            logGrid.lookupReference('exportButton').setDisabled(store.count() === 0);

        }, this);

        this.callParent([ view ]);
    },

    /**
     * @method _updateFilters
     * 
     * Updates the search filters when the selected execution changes.
     * 
     * @private
     * @param {ETexas.model.ExecutionModel} selectedExecution The selected execution.
     */
    _updateFilters : function(selectedExecution) {

        this.lookupReference('minTimeField').reset();
        this.lookupReference('maxTimeField').reset();
        this._resetSearchResults();

        var viewModel = this.getViewModel();

        if (!selectedExecution || selectedExecution.get('status') === 'Error') {

            viewModel.get('hostFilterStore').removeAll();
            viewModel.get('applicationFilterStore').removeAll();
            viewModel.get('keyFilterStore').removeAll();
        }
        else {

            var me = this;
            me._loadHostFilterStore(function() {
                me._loadApplicationFilterStore(function() {
                    me._loadKeyFilterStore();
                });
            });
        }
    },

    /**
     * @method _loadHostFilterStore
     * 
     * Loads the host device filter data.
     * 
     * @private
     * @param {Ext.Function} [callback] The function to call after loading the host filter store.
     */
    _loadHostFilterStore : function(callback) {

        var viewModel = this.getViewModel();
        var hostFilterStore = viewModel.get('hostFilterStore');
        var selectedComposite = viewModel.get('selectedComposite');
        var selectedExecution = viewModel.get('selectedExecution');

        Ext.Ajax.request({
            scope : this,
            method : 'GET',
            url : ETexas.util.UrlProvider.getLogDevicesUrl(selectedComposite, selectedExecution),
            success : function(response, opts) {

                var devices = [];
                Ext.Array.each(Ext.JSON.decode(response.responseText), function(item, index, allItems) {

                    devices.push({
                        value : item
                    });
                });

                hostFilterStore.loadData(devices);

                if (callback) {

                    callback();
                }
            }
        });
    },

    /**
     * @method _loadApplicationFilterStore
     * 
     * Loads the application filter data.
     * 
     * @private
     * @param {Ext.Function} [callback] The function to call after loading the application filter
     * store.
     */
    _loadApplicationFilterStore : function(callback) {

        var me = this;
        var viewModel = me.getViewModel();
        var applicationFilterStore = viewModel.get('applicationFilterStore');
        var selectedComposite = viewModel.get('selectedComposite');
        var selectedExecution = viewModel.get('selectedExecution');

        Ext.Ajax.request({
            scope : this,
            method : 'GET',
            url : ETexas.util.UrlProvider.getLogApplicationsUrl(selectedComposite, selectedExecution),
            params : {
                deviceList : me._getSelectedDevices()
            },
            success : function(response, opts) {

                var applications = [];
                Ext.Array.each(Ext.JSON.decode(response.responseText), function(item, index, allItems) {

                    applications.push({
                        value : item
                    });
                });

                applicationFilterStore.loadData(applications);

                if (callback) {

                    callback();
                }
            }
        });
    },

    /**
     * @method _loadKeyFilterStore
     * 
     * Loads the key filter data.
     * 
     * @private
     * @param {Ext.Function} [callback] The function to call after loading the key filter store.
     */
    _loadKeyFilterStore : function(callback) {

        var me = this;
        var viewModel = me.getViewModel();
        var keyFilterStore = viewModel.get('keyFilterStore');
        var selectedComposite = viewModel.get('selectedComposite');
        var selectedExecution = viewModel.get('selectedExecution');

        Ext.Ajax.request({
            scope : this,
            method : 'GET',
            url : ETexas.util.UrlProvider.getLogKeysUrl(selectedComposite, selectedExecution),
            params : {
                deviceList : me._getSelectedDevices(),
                applicationList : me._getSelectedApplications()
            },
            success : function(response, opts) {

                var keys = [];
                Ext.Array.each(Ext.JSON.decode(response.responseText), function(item, index, allItems) {

                    keys.push({
                        value : item
                    });
                });

                keyFilterStore.loadData(keys);

                if (callback) {

                    callback();
                }
            }
        });
    },

    /**
     * @method _getLogParameters
     * 
     * Returns the log parameters.
     * 
     * @private
     * @param {Ext.form.field.Number} minTimeField The minimum simulation time to look for.
     * @param {Ext.form.field.Number} maxTimeField The maximum simulation time to look for.
     * @return {Object} The log parameters.
     */
    _getLogParameters : function(minTimeField, maxTimeField) {

        var params = this.getViewModel().get('_parameters');

        params.deviceList = this._getSelectedDevices();
        params.applicationList = this._getSelectedApplications();
        params.keyList = this._getSelectedKeys();

        if (minTimeField.getValue()) {

            params.minTime = minTimeField.getValue();
        }

        if (maxTimeField.getValue()) {

            params.maxTime = maxTimeField.getValue();
        }

        return params;
    },

    /**
     * @method _getSelectedDevices
     * 
     * Returns an array of the selected device MAC addresses.
     * 
     * @private
     * @return {Ext.Number[]} The selected device MAC addresses.
     */
    _getSelectedDevices : function() {

        var deviceList = [];
        var selectedDevices = this.lookupReference('hostFilterGrid').getSelection();
        for (var i = 0; i < selectedDevices.length; i++) {
            deviceList.push(selectedDevices[i].get('value'));
        }

        return deviceList;
    },

    /**
     * @method _getSelectedApplications
     * 
     * Returns an array of the selected application names.
     * 
     * @private
     * @return {Ext.String[]} The selected application names.
     */
    _getSelectedApplications : function() {

        var applicationList = [];
        var selectedApplications = this.lookupReference('applicationFilterGrid').getSelection();
        for (var i = 0; i < selectedApplications.length; i++) {
            applicationList.push(selectedApplications[i].get('value'));
        }

        return applicationList;
    },

    /**
     * @method _getSelectedKeys
     * 
     * Returns an array of the selected keys.
     * 
     * @private
     * @return {Ext.String[]} The selected keys.
     */
    _getSelectedKeys : function() {

        var keyList = [];
        var selectedKeys = this.lookupReference('keyFilterGrid').getSelection();
        for (var i = 0; i < selectedKeys.length; i++) {
            keyList.push(selectedKeys[i].get('value'));
        }

        return keyList;
    },

    /**
     * @method exportLogs
     * 
     * Exports the application logs using the current search criteria.
     * 
     * @protected
     */
    exportLogs : function() {

        var viewModel = this.getViewModel();

        // use a standard submit form to make the request
        Ext.create('Ext.form.Panel', {
            standardSubmit : true
        }).submit({
            url : ETexas.util.UrlProvider.getLogsUrl(viewModel.get('selectedComposite'), viewModel.get('selectedExecution')),
            params : viewModel.get('_parameters')
        });
    },

    /**
     * @method onApplicationFiltersChanged
     * 
     * Handles the event that is generated when the user selects application filters.
     * 
     * @protected
     * @param {Ext.selection.Model} model The selection model.
     * @param {Ext.data.Model[]} selected The selected applications.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onApplicationFiltersChanged : function(model, selected, eOpts) {

        var view = this.getView();
        view.mask('Loading Filters...');

        this._loadKeyFilterStore(function() {
            view.unmask();
        });
    },

    /**
     * @method onHostFiltersChanged
     * 
     * Handles the event that is generated when the user selects host device filters.
     * 
     * @protected
     * @param {Ext.selection.Model} model The selection model.
     * @param {Ext.data.Model[]} selected The selected host devices.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onHostFiltersChanged : function(model, selected, eOpts) {

        var me = this;
        var view = me.getView();
        view.mask('Loading Filters...');

        me._loadApplicationFilterStore(function() {
            me._loadKeyFilterStore(function() {
                view.unmask();
            });
        });
    },

    /**
     * @method resetLogs
     * 
     * Resets the application logs and current search criteria.
     * 
     * @protected
     */
    resetLogs : function() {

        var hostFilterGrid = this.lookupReference('hostFilterGrid');
        var selection = hostFilterGrid.getSelection();
        hostFilterGrid.getSelectionModel().deselectAll();

        if (selection.length === 0) {

            hostFilterGrid.fireEvent('selectionchange', hostFilterGrid, selection);
        }

        this.lookupReference('minTimeField').reset();
        this.lookupReference('maxTimeField').reset();
        this._resetSearchResults();
    },

    /**
     * @method _resetSearchResults
     * 
     * Resets the current log search results.
     * 
     * @private
     */
    _resetSearchResults : function() {

        var logStore = this.getViewModel().get('logStore');
        logStore.removeAll();
        logStore.fireEvent('load', logStore, [], {});
    },

    /**
     * @method searchLogs
     * 
     * Searches the application logs using the current search criteria.
     * 
     * @protected
     */
    searchLogs : function() {

        var minTimeField = this.lookupReference('minTimeField');
        var maxTimeField = this.lookupReference('maxTimeField');

        if (minTimeField.isValid() && maxTimeField.isValid()) {

            var logGrid = this.lookupReference('logGrid');
            var logStore = this.getViewModel().get('logStore');
            logStore.getProxy().setExtraParams(this._getLogParameters(minTimeField, maxTimeField));
            logStore.setPageSize(logGrid.lookupReference('resultsBox').getValue());

            logGrid.mask('Searching Logs...');
            logStore.loadPage(1, {
                callback : function(records, operation, success) {
                    logGrid.unmask();
                }
            });
        }
    },

    /**
     * @method validateMaxTimeField
     * 
     * Validates the current value of the maximum simulation time field.
     * 
     * @protected
     * @param {String} value The current field value.
     * @return {Boolean/String} True if the current field value is valid, otherwise a string
     * validation message is returned.
     */
    validateMaxTimeField : function(value) {

        if (value.length === 0) {

            return true;
        }

        var maxTime = parseFloat(value);
        var selectedExecution = this.getViewModel().get('selectedExecution');
        var totalTime = selectedExecution.get('maxSteps') * selectedExecution.get('stepSize');

        if (maxTime > totalTime) {

            return 'The maximum simulation time may not be more than ' + totalTime + ' seconds.';
        }

        var minTimeField = this.lookupReference('minTimeField');
        var minTime = minTimeField.getValue();

        if (minTime) {

            if (minTime > maxTime) {

                return 'The maximum simulation time may not be less than the minimum simulation time.';
            }

            minTimeField.clearInvalid();
        }

        return true;
    },

    /**
     * @method validateMinTimeField
     * 
     * Validates the current value of the minimum simulation time field.
     * 
     * @protected
     * @param {String} value The current field value.
     * @return {Boolean/String} True if the current field value is valid, otherwise a string
     * validation message is returned.
     */
    validateMinTimeField : function(value) {

        if (value.length === 0) {

            return true;
        }

        var minTime = parseFloat(value);

        if (minTime < 0) {

            return 'The minimum simulation time may not be less than 0 seconds.';
        }

        var maxTimeField = this.lookupReference('maxTimeField');
        var maxTime = maxTimeField.getValue();

        if (maxTime) {

            if (maxTime < minTime) {

                return 'The minimum simulation time may not exceed the maximum simulation time.';
            }

            maxTimeField.clearInvalid();
        }

        return true;
    }
});
