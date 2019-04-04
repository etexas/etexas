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
 * @class ETexas.view.perspective.SimulationsPerspectiveController
 * @extends Ext.app.ViewController
 * 
 * The {@link ETexas.view.perspectives.SimulationsPerspective} controller.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.perspective.SimulationsPerspectiveController', {
    extend : 'Ext.app.ViewController',
    alias : 'controller.simulationsperspective',

    /** @inheritdoc */
    init : function(view) {

        var viewModel = this.getViewModel();
        var compositeToolbar = view.lookupReference('compositeToolbar');
        var simulationToolbar = view.lookupReference('simulationToolbar');
        compositeToolbar.lookupReference('deleteButton').setDisabled(true);
        simulationToolbar.lookupReference('deleteButton').setDisabled(true);

        viewModel.bind('{selectedComposite}', function(selectedComposite) {

            compositeToolbar.lookupReference('editButton').setDisabled(!selectedComposite);
            simulationToolbar.lookupReference('createButton').setDisabled(!selectedComposite);
        });

        viewModel.bind('{selectedSimulation}', function(selectedSimulation) {

            simulationToolbar.lookupReference('editButton').setDisabled(!selectedSimulation);
        });

        this.callParent([ view ]);
    },

    /**
     * @method deleteComposites
     * 
     * Deletes the selected {@link ETexas.model.CompositeModel}s.
     * 
     * @protected
     */
    deleteComposites : function() {

        var me = this;
        var view = this.getView();
        var selectedComposites = view.lookupReference('compositeGrid').getSelection();

        var mask;
        var message;
        var messageTitle;

        if (selectedComposites.length === 1) {

            mask = 'Deleting Composite...';
            messageTitle = 'Delete Composite';
            message = 'Deleting \"' + selectedComposites[0].get('name') + '\" will result in the loss of all data associated with the composite. Are you sure you want to delete it?';
        }
        else {

            mask = 'Deleting Composites...';
            messageTitle = 'Delete Composites';
            message = 'Deleting the selected composites will result in the loss of all data associated with the composites. Are you sure you want to delete them?';
        }

        Ext.Msg.confirm(messageTitle, message, function(btn) {

            var compositeIds = [];
            selectedComposites.forEach(function(value, index, array) {
                compositeIds.push(value.get('id'));
            });

            var queryParams = Ext.Object.toQueryString({
                compositeIds : compositeIds
            });

            if (btn === 'yes') {

                view.mask(mask);
                Ext.Ajax.request({
                    scope : this,
                    method : 'DELETE',
                    url : ETexas.util.UrlProvider.getCompositesUrl() + '?' + queryParams,
                    success : function(response, opts) {

                        view.unmask();
                        me._updateCompositeStore();
                    },
                    failure : function(response, opts) {

                        view.unmask();
                    }
                });
            }
        });
    },

    /**
     * @method deleteSimulations
     * 
     * Deletes the selected {@link ETexas.model.SimulationModel}s.
     * 
     * @protected
     */
    deleteSimulations : function() {

        var me = this;
        var view = this.getView();
        var selectedComposite = this.getViewModel().get('selectedComposite');
        var selectedSimulations = view.lookupReference('simulationGrid').getSelection();

        var mask;
        var message;
        var messageTitle;

        if (selectedSimulations.length === 1) {

            mask = 'Deleting Simulation...';
            messageTitle = 'Delete Simulation';
            message = 'Deleting \"' + selectedSimulations[0].get('name') + '\" will result in the loss of all data associated with the simulation. Are you sure you want to delete it?';
        }
        else {

            mask = 'Deleting Simulations...';
            messageTitle = 'Delete Simulations';
            message = 'Deleting the selected simulations will result in the loss of all data associated with the simulations. Are you sure you want to delete them?';
        }

        Ext.Msg.confirm(messageTitle, message, function(btn) {

            var simulationIds = [];
            selectedSimulations.forEach(function(value, index, array) {
                simulationIds.push(value.get('id'));
            });

            var queryParams = Ext.Object.toQueryString({
                simulationIds : simulationIds
            });

            if (btn === 'yes') {

                view.mask(mask);
                Ext.Ajax.request({
                    scope : this,
                    method : 'DELETE',
                    url : ETexas.util.UrlProvider.getSimulationsUrl(selectedComposite) + '?' + queryParams,
                    success : function(response, opts) {

                        view.unmask();
                        me._updateCompositeStore(selectedComposite.get('id'));
                    },
                    failure : function(response, opts) {

                        view.unmask();
                    }
                });
            }
        });
    },

    /**
     * @method editSourceFiles
     * 
     * Downloads the source file editor for the selected {@link ETexas.model.SimulationModel}.
     * 
     * @protected
     */
    editSourceFiles : function() {

        var viewModel = this.getViewModel();
        var simulationId = viewModel.get('selectedSimulation').get('id');
        var urlSuffix = '/JNLP/gdvsim-servlet/gdvsim.jnlp?simId=' + simulationId;

        if (viewModel.get('webStartUrl')) {

            document.location = viewModel.get('webStartUrl') + urlSuffix;
        }
        else {

            Ext.Ajax.request({
                scope : this,
                method : 'GET',
                waitMsg : 'Initializing Web Start...',
                url : ETexas.util.UrlProvider.getWebStartUrl(),
                success : function(response, opts) {

                    viewModel.set('webStartUrl', response.responseText);
                    document.location = viewModel.get('webStartUrl') + urlSuffix;
                }
            });
        }
    },

    /**
     * @method exportComposite
     * 
     * Exports the selected {@link ETexas.model.CompositeModel}.
     * 
     * @protected
     */
    exportComposite : function() {

        var selectedComposite = this.getViewModel().get('selectedComposite');
        document.location = ETexas.util.UrlProvider.getCompositeFilesUrl(selectedComposite);
    },

    /**
     * @method exportSimulation
     * 
     * Exports the selected {@link ETexas.model.SimulationModel}.
     * 
     * @protected
     */
    exportSimulation : function() {

        var viewModel = this.getViewModel();
        var selectedSimulation = viewModel.get('selectedSimulation');
        document.location = ETexas.util.UrlProvider.getSimulationFilesUrl(selectedSimulation);
    },

    /**
     * @method onHelpToolClicked
     * 
     * Handles the event that is generated when the user clicks the Composites Help tool.
     * 
     * @protected
     * @param {Ext.panel.Tool} tool The clicked tool.
     * @param {Ext.event.Event} e The click event.
     * @param {Ext.Component} owner The owner of the tool.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onCompositeHelpToolClicked : function(tool, e, owner, eOpts) {

        window.open(ETexas.util.UrlProvider.getHelpContentUrl() + '/Simulations/Composites/composites_about.htm');
    },

    /**
     * @method onCompositeSelectionChange
     * 
     * Handles the event that is generated when the user changes the selected composite(s).
     * 
     * @protected
     * @param {Ext.selection.Model} model The selection model.
     * @param {Ext.data.Model[]} selected The selected composites.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onCompositeSelectionChange : function(model, selected, eOpts) {

        this.getViewModel().set('selectedComposite', selected.length === 1 ? selected[0] : null);
        this.getView().lookupReference('compositeToolbar').lookupReference('deleteButton').setDisabled(selected.length === 0);
    },

    /**
     * @method onHelpToolClicked
     * 
     * Handles the event that is generated when the user clicks the Simulations Help tool.
     * 
     * @protected
     * @param {Ext.panel.Tool} tool The clicked tool.
     * @param {Ext.event.Event} e The click event.
     * @param {Ext.Component} owner The owner of the tool.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onSimulationHelpToolClicked : function(tool, e, owner, eOpts) {

        window.open(ETexas.util.UrlProvider.getHelpContentUrl() + '/Simulations/Simulations/simulations_about.htm');
    },

    /**
     * @method onSimulationSelectionChange
     * 
     * Handles the event that is generated when the user changes the selected simulation(s).
     * 
     * @protected
     * @param {Ext.selection.Model} model The selection model.
     * @param {Ext.data.Model[]} selected The selected simulations.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onSimulationSelectionChange : function(model, selected, eOpts) {

        this.getViewModel().set('selectedSimulation', selected.length === 1 ? selected[0] : null);
        this.getView().lookupReference('simulationToolbar').lookupReference('deleteButton').setDisabled(selected.length === 0);
    },

    /**
     * @method showCompositeReportingWindow
     * 
     * Shows the reporting window for the selected {@link ETexas.model.CompositeModel}.
     * 
     * @protected
     */
    showCompositeReportingWindow : function() {

        var window = Ext.create('ETexas.view.application.ApplicationHostWindow', {
            id : Ext.id(null, 'composite-reporting-window-')
        });

        var viewModel = this.getViewModel();
        var selectedComposite = viewModel.get('selectedComposite');

        var windowModel = window.getViewModel();
        windowModel.set('applicationsUrl', ETexas.util.UrlProvider.getReportApplicationsUrl(selectedComposite));
        windowModel.set('hostType', 'Report');

        window.show();
    },

    /**
     * @method showCompositeSettingsWindow
     * 
     * Shows the settings window for the selected {@link ETexas.model.CompositeModel}.
     * 
     * @protected
     */
    showCompositeSettingsWindow : function() {

        var window = Ext.create('ETexas.view.composite.CompositeSettingsWindow', {
            id : Ext.id(null, 'composite-settings-window-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        window.show();
    },

    /**
     * @method showCopyCompositeForm
     * 
     * Shows the form to copy the selected {@link ETexas.model.CompositeModel}.
     * 
     * @protected
     */
    showCopyCompositeForm : function() {

        var form = Ext.create('ETexas.view.composite.CopyCompositeForm', {
            id : Ext.id(null, 'copy-composite-form-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        form.on('compositecopied', this._updateCompositeStore, this);
        form.show();
    },

    /**
     * @method showCopySimulationForm
     * 
     * Shows the form to copy the selected {@link ETexas.model.SimulationModel}.
     * 
     * @protected
     */
    showCopySimulationForm : function() {

        var form = Ext.create('ETexas.view.simulation.CopySimulationForm', {
            id : Ext.id(null, 'copy-simulation-form-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        form.on('simulationcopied', this._updateCompositeStore, this);
        form.show();
    },

    /**
     * @method showCreateCompositeForm
     * 
     * Shows the form to create a new {@link ETexas.model.CompositeModel}.
     * 
     * @protected
     */
    showCreateCompositeForm : function() {

        var form = Ext.create('ETexas.view.composite.CreateCompositeForm', {
            id : Ext.id(null, 'create-composite-form-')
        });

        form.on('compositeadded', this._updateCompositeStore, this);
        form.show();
    },

    /**
     * @method showCreateSimulationForm
     * 
     * Shows the form to create a new {@link ETexas.model.SimulationModel}.
     * 
     * @protected
     */
    showCreateSimulationForm : function() {

        var form = Ext.create('ETexas.view.simulation.CreateSimulationForm', {
            id : Ext.id(null, 'create-simulation-form-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        form.on('simulationadded', this._updateCompositeStore, this);
        form.show();
    },

    /**
     * @method showDetectorsWindow
     * 
     * Shows the detectors window for the selected {@link ETexas.modelSimulationModel}.
     * 
     * @protected
     */
    showDetectorsWindow : function() {

        var window = Ext.create('ETexas.view.detector.DetectorWindow', {
            id : Ext.id(null, 'detector-window-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        window.show();
    },

    /**
     * @method showRenameCompositeForm
     * 
     * Shows the form to rename the selected {@link ETexas.model.CompositeModel}.
     * 
     * @protected
     */
    showRenameCompositeForm : function() {

        var form = Ext.create('ETexas.view.composite.RenameCompositeForm', {
            id : Ext.id(null, 'rename-composite-form-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        form.on('compositerenamed', this._updateCompositeStore, this);
        form.show();
    },

    /**
     * @method showUpdateSimulationForm
     * 
     * Shows the form to update the selected {@link ETexas.model.SimulationModel}.
     * 
     * @protected
     */
    showUpdateSimulationForm : function() {

        var form = Ext.create('ETexas.view.simulation.UpdateSimulationForm', {
            id : Ext.id(null, 'update-simulation-form-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        form.on('simulationupdated', this._updateCompositeStore, this);
        form.show();
    },

    /**
     * @method showUploadSimulationForm
     * 
     * Shows the form to upload a new {@link ETexas.model.SimulationModel}.
     * 
     * @protected
     */
    showUploadSimulationForm : function() {

        var form = Ext.create('ETexas.view.simulation.UploadSimulationForm', {
            id : Ext.id(null, 'upload-simulation-form-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        form.on('simulationuploaded', this._updateCompositeStore, this);
        form.show();
    },

    /**
     * @method _updateCompositeStore
     * 
     * Updates the composite data for the current user.
     * 
     * @private
     * @param {Number} [compositeId] The ID of the selected composite.
     * @param {Number} [simulationId] The ID of the selected simulation.
     */
    _updateCompositeStore : function(compositeId, simulationId) {

        var viewModel = this.getViewModel();
        var compositeStore = viewModel.get('compositeStore');
        var compositeGrid = this.getView().lookupReference('compositeGrid');
        var simulationGrid = this.getView().lookupReference('simulationGrid');
        compositeStore.load(function(records, operation, success) {
            var selectedComposite = compositeId ? compositeStore.getById(compositeId) : null;
            var selectedSimulation = (selectedComposite && simulationId) ? selectedComposite.getSimulations().getById(simulationId) : null;
            compositeGrid.setSelection(selectedComposite);
            simulationGrid.setSelection(selectedSimulation);
            viewModel.set('selectedComposite', selectedComposite);
            viewModel.notify();
            viewModel.set('selectedSimulation', selectedSimulation);
        });
    }
});
