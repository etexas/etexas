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
 * @class ETexas.view.perspective.ExecutionsPerspectiveController
 * @extends Ext.app.ViewController
 * 
 * The {@link ETexas.view.perspective.ExecutionsPerspective} controller.
 * 
 * @author emyers
 * @author ttevendale
 */
Ext.define('ETexas.view.perspective.ExecutionsPerspectiveController', {
    extend : 'Ext.app.ViewController',
    alias : 'controller.executionsperspective',

    /** @inheritdoc */
    init : function(view) {

        var me = this;
        var viewModel = this.getViewModel();
        var executionToolbar = view.lookupReference('executionToolbar');
        executionToolbar.lookupReference('deleteButton').setDisabled(true);

        viewModel.bind('{selectedComposite}', function(selectedComposite) {

            if (selectedComposite) {

                me._updateExecutionStore();
            }
            else {

                viewModel.get('executionStore').removeAll();
            }

            executionToolbar.lookupReference('createButton').setDisabled(!selectedComposite);
        });

        viewModel.bind('{selectedExecution}', function(selectedExecution) {

            me._updateExecutionStores(selectedExecution);
            me._updateExecutionControls(selectedExecution);
            executionToolbar.lookupReference('editButton').setDisabled(!selectedExecution);
            executionToolbar.lookupReference('executionMessagesButton').setDisabled(!selectedExecution);
        });

        view.lookupReference('stepsField').validator = Ext.bind(this.validateStepsField, this);

        this.callParent([ view ]);
    },

    /**
     * @method _updateExecutionControls
     * 
     * Updates the execution controls when the selected execution changes.
     * 
     * @private
     * @param {ETexas.model.ExecutionModel} [selectedExecution] The selected execution.
     */
    _updateExecutionControls : function(selectedExecution) {

        var view = this.getView();
        var viewModel = this.getViewModel();

        if (!selectedExecution || viewModel.get('_currentExecutionId') !== selectedExecution.get('id')) {

            view.lookupReference('visualizationContainer').reset();
            viewModel.set('_currentExecutionId', selectedExecution !== null ? selectedExecution.get('id') : null);
        }
        else if (selectedExecution.get('status') === 'Complete') {

            view.lookupReference('visualizationContainer').reset();
        }

        if (!selectedExecution || selectedExecution.get('status') === 'Error') {

            view.lookupReference('detailsContainer').setDisabled(true);
        }
        else {

            var inProgress = selectedExecution.get('status') === 'In Progress';
            view.lookupReference('advanceButton').setDisabled(!inProgress);
            view.lookupReference('commandButton').setDisabled(!inProgress);
            view.lookupReference('finishButton').setDisabled(!inProgress);
            view.lookupReference('stepsField').setDisabled(!inProgress);
            view.lookupReference('detailsContainer').setDisabled(false);
        }
    },

    /**
     * @method _updateExecutionStores
     * 
     * Updates the execution stores when the selected execution changes.
     * 
     * @private
     * @param {ETexas.model.ExecutionModel} [selectedExecution] The selected execution.
     */
    _updateExecutionStores : function(selectedExecution) {

        var viewModel = this.getViewModel();

        if (selectedExecution && selectedExecution.get('status') === 'Complete') {

            viewModel.get('detectorStore').removeAll();
            viewModel.get('messageStore').removeAll();
            viewModel.get('signalStore').removeAll();
            viewModel.get('standaloneDeviceStore').removeAll();
            viewModel.get('vehicleStore').removeAll();

            viewModel.get('cellTowerStore').load();
            this._loadCommandStore();
            this._loadLaneStore();
            viewModel.get('topographyFeatureStore').load();
        }
        else if (selectedExecution && selectedExecution.get('status') === 'In Progress') {

            viewModel.get('cellTowerStore').load();
            this._loadCommandStore();
            this._loadDetectorStore();

            var me = this;
            this._loadLaneStore(function() {
                me._loadSignalStore();
            });

            viewModel.get('messageStore').load();
            viewModel.get('standaloneDeviceStore').load();
            viewModel.get('topographyFeatureStore').load();
            this._loadVehicleStore();
        }
        else {

            viewModel.get('cellTowerStore').removeAll();
            viewModel.get('commandStore').removeAll();
            viewModel.get('detectorStore').removeAll();
            viewModel.get('laneStore').removeAll();
            viewModel.get('messageStore').removeAll();
            viewModel.get('signalStore').removeAll();
            viewModel.get('standaloneDeviceStore').removeAll();
            viewModel.get('topographyFeatureStore').removeAll();
            viewModel.get('vehicleStore').removeAll();
        }
    },

    /**
     * @method _loadCommandStore
     * 
     * Loads the command data for the selected execution.
     * 
     * @private
     */
    _loadCommandStore : function() {

        var commandStore = this.getViewModel().get('commandStore');
        var selectedComposite = this.getViewModel().get('selectedComposite');
        var selectedExecution = this.getViewModel().get('selectedExecution');

        Ext.Ajax.request({
            scope : this,
            method : 'GET',
            url : ETexas.util.UrlProvider.getCommandsUrl(selectedComposite, selectedExecution),
            success : function(response, opts) {

                var commands = Ext.JSON.decode(response.responseText);

                Ext.Array.each(commands, function(item, index, allItems) {

                    var simulation = selectedComposite.getSimulations().getById(item.intersectionId);
                    item.simulationName = simulation ? simulation.get('name') : 'Unknown Simulation';
                });

                commandStore.setData(commands);
            }
        });
    },

    /**
     * @method _loadDetectorStore
     * 
     * Loads the detector data for the selected execution.
     * 
     * @private
     */
    _loadDetectorStore : function() {

        var detectorStore = this.getViewModel().get('detectorStore');
        var selectedComposite = this.getViewModel().get('selectedComposite');
        var selectedExecution = this.getViewModel().get('selectedExecution');

        Ext.Ajax.request({
            scope : this,
            method : 'GET',
            url : ETexas.util.UrlProvider.getDeployedDetectorsUrl(selectedComposite, selectedExecution),
            success : function(response, opts) {

                var detectors = [];
                var detectorMap = Ext.JSON.decode(response.responseText);

                Ext.Object.each(detectorMap, function(key, value, object) {

                    var simulation = selectedComposite.getSimulations().getById(key);

                    Ext.Array.forEach(value, function(item, index, allItems) {

                        item.simulationName = simulation.get('name');
                        detectors.push(item);

                    }, this);
                });

                detectorStore.setData(detectors);
            }
        });
    },

    /**
     * @method _loadLaneStore
     * 
     * Loads the lane data for the selected execution.
     * 
     * @private
     * @param {Ext.Function} [callback] The function to call after loading the lane store.
     */
    _loadLaneStore : function(callback) {

        var laneStore = this.getViewModel().get('laneStore');
        var selectedComposite = this.getViewModel().get('selectedComposite');

        Ext.Ajax.request({
            scope : this,
            method : 'GET',
            url : ETexas.util.UrlProvider.getLanesUrl(selectedComposite),
            success : function(response, opts) {

                var lanes = [];
                var laneMap = Ext.JSON.decode(response.responseText);

                Ext.Object.each(laneMap, function(key, value, object) {

                    var simulation = selectedComposite.getSimulations().getById(key);

                    Ext.Array.forEach(value, function(item, index, allItems) {

                        item.simulationName = simulation.get('name');
                        lanes.push(item);

                    }, this);
                });

                laneStore.setData(lanes);

                if (callback) {

                    callback();
                }
            }
        });
    },

    /**
     * @method _loadSignalStore
     * 
     * Loads the signal data for the selected execution.
     * 
     * @private
     */
    _loadSignalStore : function() {

        var signalStore = this.getViewModel().get('signalStore');
        var selectedComposite = this.getViewModel().get('selectedComposite');
        var selectedExecution = this.getViewModel().get('selectedExecution');

        Ext.Ajax.request({
            scope : this,
            method : 'GET',
            url : ETexas.util.UrlProvider.getSignalsUrl(selectedComposite, selectedExecution),
            success : function(response, opts) {

                var signals = [];
                var signalMap = Ext.JSON.decode(response.responseText);

                Ext.Object.each(signalMap, function(key, value, object) {

                    var simulation = selectedComposite.getSimulations().getById(key);

                    Ext.Array.forEach(value, function(item, index, allItems) {

                        item.simulationName = simulation.get('name');
                        signals.push(item);

                    }, this);
                });

                signalStore.setData(signals);
            }
        });
    },

    /**
     * @method _loadVehicleStore
     * 
     * Loads the vehicle data for the selected execution.
     * 
     * @private
     */
    _loadVehicleStore : function() {

        var vehicleStore = this.getViewModel().get('vehicleStore');
        var selectedComposite = this.getViewModel().get('selectedComposite');
        var selectedExecution = this.getViewModel().get('selectedExecution');

        Ext.Ajax.request({
            scope : this,
            method : 'GET',
            url : ETexas.util.UrlProvider.getVehiclesUrl(selectedComposite, selectedExecution),
            success : function(response, opts) {

                var vehicles = [];
                var vehicleMap = Ext.JSON.decode(response.responseText);

                Ext.Object.each(vehicleMap, function(key, value, object) {

                    var simulation = selectedComposite.getSimulations().getById(key);

                    Ext.Array.forEach(value, function(item, index, allItems) {

                        item.simulationName = simulation.get('name');
                        vehicles.push(item);

                    }, this);
                });

                vehicleStore.setData(vehicles);
            }
        });
    },

    /**
     * @method deleteExecutions
     * 
     * Deletes the selected {@link ETexas.model.ExecutionModel}s.
     * 
     * @protected
     */
    deleteExecutions : function() {

        var me = this;
        var view = this.getView();
        var selectedComposite = this.getViewModel().get('selectedComposite');
        var selectedExecutions = view.lookupReference('executionGrid').getSelection();

        var mask;
        var message;
        var messageTitle;

        if (selectedExecutions.length === 1) {

            mask = 'Deleting Execution...';
            message = 'Deleting \"' + selectedExecutions[0].get('name') + '\" will result in the loss of all data associated with the execution. Are you sure you want to delete it?';
            messageTitle = 'Delete Execution';
        }
        else {

            mask = 'Deleting Executions...';
            message = 'Deleting the selected executions will result in the loss of all data associated with the executions. Are you sure you want to delete them?';
            messageTitle = 'Delete Executions';
        }

        Ext.Msg.confirm(messageTitle, message, function(btn) {

            if (btn === 'yes') {

                var executionIds = [];
                selectedExecutions.forEach(function(value, index, array) {
                    executionIds.push(value.get('id'));
                });

                var queryParams = Ext.Object.toQueryString({
                    executionIds : executionIds
                });

                view.mask(mask);
                Ext.Ajax.request({
                    scope : this,
                    method : 'DELETE',
                    url : ETexas.util.UrlProvider.getExecutionsUrl(selectedComposite) + '?' + queryParams,
                    success : function(response, opts) {

                        view.unmask();
                        me._updateExecutionStore();
                    },
                    failure : function(response, opts) {

                        view.unmask();
                    }
                });
            }
        });
    },

    /**
     * @method onAdvanceButtonClicked
     * 
     * Handles the event that is generated when the user clicks the Advance button.
     * 
     * @protected
     * @param {Ext.button.Button} button The clicked button.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onAdvanceButtonClicked : function(button, e, eOpts) {

        var me = this;
        var view = this.getView();
        var viewModel = this.getViewModel();
        var selectedComposite = viewModel.get('selectedComposite');
        var selectedExecution = viewModel.get('selectedExecution');
        var stepsField = view.lookupReference('stepsField');

        if (stepsField.isValid()) {

            var main = view.findParentByType('main');
            main.mask('Advancing Execution...');

            Ext.Ajax.request({
                scope : this,
                method : 'PUT',
                url : ETexas.util.UrlProvider.getExecutionsUrl(selectedComposite, selectedExecution),
                success : function(response, opts) {

                    main.unmask();
                    me._updateExecutionStore(selectedExecution.get('id'));
                },
                failure : function(response, opts) {

                    main.unmask();
                },
                params : {
                    steps : stepsField.getValue(),
                    executionName : selectedExecution.get('name')
                }
            });
        }
    },

    /**
     * @method onExecutionSelectionChange
     * 
     * Handles the event that is generated when the user changes the selected execution(s).
     * 
     * @protected
     * @param {Ext.selection.Model} model The selection model.
     * @param {Ext.data.Model[]} selected The selected executions.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onExecutionSelectionChange : function(model, selected, eOpts) {

        this.getViewModel().set('selectedExecution', selected.length === 1 ? selected[0] : null);
        this.getView().lookupReference('executionToolbar').lookupReference('deleteButton').setDisabled(selected.length === 0);
    },

    /**
     * @method onExecutionsHelpToolClicked
     * 
     * Handles the event that is generated when the user clicks the Executions Help tool.
     * 
     * @protected
     * @param {Ext.panel.Tool} tool The clicked tool.
     * @param {Ext.event.Event} e The click event.
     * @param {Ext.Component} owner The owner of the tool.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onExecutionsHelpToolClicked : function(tool, e, owner, eOpts) {

        window.open(ETexas.util.UrlProvider.getHelpContentUrl() + '/Executions/executions_perspective.htm');
    },

    /**
     * @method onFinishButtonClicked
     * 
     * Handles the event that is generated when the user clicks the Finish button.
     * 
     * @protected
     * @param {Ext.button.Button} button The clicked button.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onFinishButtonClicked : function(button, e, eOpts) {

        var me = this;
        var view = this.getView();
        var viewModel = this.getViewModel();
        var selectedComposite = viewModel.get('selectedComposite');
        var selectedExecution = viewModel.get('selectedExecution');

        var main = view.findParentByType('main');
        main.mask('Finishing Execution...');

        Ext.Ajax.request({
            scope : this,
            method : 'PUT',
            url : ETexas.util.UrlProvider.getExecutionsUrl(selectedComposite, selectedExecution),
            success : function(response, opts) {

                main.unmask();
                me._updateExecutionStore(selectedExecution.get('id'));
            },
            failure : function(response, opts) {

                main.unmask();
            },
            params : {
                steps : viewModel.get('remainingSteps'),
                executionName : selectedExecution.get('name')
            }
        });
    },

    /**
     * @method onTextItemClicked
     * 
     * Handles the event that is generated when the user clicks the Text menu item.
     * 
     * @protected
     * @param {Ext.menu.Item} item The clicked menu item.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onTextItemClicked : function(item, e, eOpts) {

        this.lookupReference('displayContainer').setActiveItem(this.lookupReference('informationTabPanel'));
    },

    /**
     * @method onVisualizationItemClicked
     * 
     * Handles the event that is generated when the user clicks the Visualization menu item.
     * 
     * @protected
     * @param {Ext.menu.Item} item The clicked menu item.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onVisualizationItemClicked : function(item, e, eOpts) {

        this.lookupReference('displayContainer').setActiveItem(this.lookupReference('visualizationContainer'));
    },

    /**
     * @method showApplications
     * 
     * Shows the window to display the applications for a specified standalone device.
     * 
     * @protected
     * @param {ETexas.model.StandaloneDeviceModel} device The specified standalone device.
     */
    showApplications : function(device) {

        var window = Ext.create('ETexas.view.application.DeployedApplicationWindow', {
            id : Ext.id(null, 'deployed-application-window-')
        });

        var composite = this.getViewModel().get('selectedComposite');
        var deviceModel = Ext.create('ETexas.model.DeviceModel');
        deviceModel.set('id', device.get('deviceRuleId'));
        window.getViewModel().set('applicationsUrl', ETexas.util.UrlProvider.getDeviceApplicationsUrl(composite, deviceModel));
        window.show();
    },

    /**
     * @method showCreateExecutionForm
     * 
     * Shows the form to create a new {@link ETexas.model.ExecutionModel}.
     * 
     * @protected
     */
    showCreateExecutionForm : function() {

        var form = Ext.create('ETexas.view.execution.CreateExecutionForm', {
            id : Ext.id(null, 'create-execution-form-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        form.on('executionadded', this._updateExecutionStore, this);
        form.show();
    },

    /**
     * @method showEditExecutionForm
     * 
     * Shows the form to edit the selected {@link ETexas.model.ExecutionModel}.
     * 
     * @protected
     */
    showEditExecutionForm : function() {

        var form = Ext.create('ETexas.view.execution.EditExecutionForm', {
            id : Ext.id(null, 'edit-execution-form-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        form.on('executionupdated', this._updateExecutionStore, this);
        form.show();
    },

    /**
     * @method showExecutionMessages
     * 
     * Shows the {@link ETexas.model.ExecutionMessageModel} from the selected
     * {@link ETexas.model.ExecutionModel}.
     * 
     * @protected
     */
    showExecutionMessages : function() {

        var window = Ext.create('ETexas.view.message.ExecutionMessageWindow', {
            id : Ext.id(null, 'execution-message-window-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        window.show();
    },

    /**
     * @method showInjectionCommandForm
     * 
     * Shows the form to create a new injection {@link ETexas.model.CommandModel}.
     * 
     * @protected
     */
    showInjectionCommandForm : function() {

        var form = Ext.create('ETexas.view.command.InjectionCommandForm', {
            id : Ext.id(null, 'injection-command-form-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        form.on('commandadded', this._loadCommandStore, this);
        form.show();
    },

    /**
     * @method showLaneCommandForm
     * 
     * Shows the form to create a new lane {@link ETexas.model.CommandModel}.
     * 
     * @protected
     */
    showLaneCommandForm : function() {

        var form = Ext.create('ETexas.view.command.LaneCommandForm', {
            id : Ext.id(null, 'lane-command-form-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        form.on('commandadded', this._loadCommandStore, this);
        form.show();
    },

    /**
     * @method showMessages
     * 
     * Shows the window to display the messages for a specified standalone device.
     * 
     * @protected
     * @param {ETexas.model.StandaloneDeviceModel} device The specified standalone device.
     */
    showMessages : function(device) {

        var window = Ext.create('ETexas.view.message.MessageWindow', {
            id : Ext.id(null, 'message-window-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        window.getViewModel().set('deviceMac', device.get('deviceMac'));
        window.show();
    },

    /**
     * @method showOnBoardDevices
     * 
     * Shows the dialog to display the on board devices for a specified vehicle.
     * 
     * @protected
     * @param {ETexas.model.VehicleModel} vehicle The specified vehicle.
     */
    showOnBoardDevices : function(vehicle) {

        var window = Ext.create('ETexas.view.device.OnBoardDeviceWindow', {
            id : Ext.id(null, 'on-board-device-window-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        window.getViewModel().set('vehicle', vehicle);
        window.show();
    },

    /**
     * @method showSignalCommandForm
     * 
     * Shows the form to create a new signal {@link ETexas.model.CommandModel}.
     * 
     * @protected
     */
    showSignalCommandForm : function() {

        var form = Ext.create('ETexas.view.command.SignalCommandForm', {
            id : Ext.id(null, 'signal-command-form-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        form.on('commandadded', this._loadCommandStore, this);
        form.show();
    },

    /**
     * @method showSpeedCommandForm
     * 
     * Shows the form to create a new speed {@link ETexas.model.CommandModel}.
     * 
     * @protected
     */
    showSpeedCommandForm : function() {

        var form = Ext.create('ETexas.view.command.SpeedCommandForm', {
            id : Ext.id(null, 'speed-command-form-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        form.on('commandadded', this._loadCommandStore, this);
        form.show();
    },

    /**
     * @method _updateExecutionStore
     * 
     * Updates the execution data for the selected composite.
     * 
     * @private
     * @param {Number} [executionId] The ID of the selected execution.
     */
    _updateExecutionStore : function(executionId) {

        var viewModel = this.getViewModel();
        var executionStore = viewModel.get('executionStore');
        var executionGrid = this.getView().lookupReference('executionGrid');
        executionStore.load(function(records, operation, success) {
            var selectedExecution = executionId ? executionStore.getById(executionId) : null;
            executionGrid.setSelection(selectedExecution);
            viewModel.set('selectedExecution', selectedExecution);
        });
    },

    /**
     * @method validateStepsField
     * 
     * Validates the current value of the steps field.
     * 
     * @protected
     * @param {String} value The current field value.
     * @return {Boolean/String} True if the current field value is valid, otherwise a string
     * validation message is returned.
     */
    validateStepsField : function(value) {

        var viewModel = this.getViewModel();
        var selectedExecution = viewModel.get('selectedExecution');

        if (selectedExecution && value > viewModel.get('remainingSteps')) {

            return 'The number of steps to advance cannot exceed the number of remaining steps in the execution.';
        }

        return true;
    }
});
