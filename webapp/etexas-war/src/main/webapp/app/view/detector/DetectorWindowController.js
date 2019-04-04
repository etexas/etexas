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
 * @class ETexas.view.detector.DetectorWindowController
 * @extends ETexas.view.window.BasicWindowController
 * 
 * The {@link ETexas.view.detector.DetectorWindow} controller.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.detector.DetectorWindowController', {
    extend : 'ETexas.view.window.BasicWindowController',
    alias : 'controller.detectorwindow',

    requires : [ 'ETexas.view.detector.CreateDetectorForm', 'ETexas.view.detector.EditDetectorForm' ],

    /** @inheritdoc */
    init : function(view) {

        var viewModel = this.getViewModel();
        var detectorToolbar = view.lookupReference('detectorToolbar');
        detectorToolbar.lookupReference('deleteButton').setDisabled(true);

        viewModel.bind('{selectedDetector}', function(selectedDetector) {
            detectorToolbar.lookupReference('editButton').setDisabled(!selectedDetector);
        });

        this.callParent([ view ]);
    },

    /**
     * @method deleteDetectors
     * 
     * Deletes the selected {@link ETexas.model.DetectorModel}s.
     * 
     * @protected
     */
    deleteDetectors : function() {

        var me = this;
        var view = this.getView();
        var selectedComposite = this.getViewModel().get('selectedComposite');
        var selectedSimulation = this.getViewModel().get('selectedSimulation');
        var selectedDetectors = view.lookupReference('detectorGrid').getSelection();

        var mask;
        var message;
        var messageTitle;

        if (selectedDetectors.length === 1) {

            mask = 'Deleting Detector...';
            message = 'Deleting detector \"' + selectedDetectors[0].get('id') + '\" will result in the loss of all data associated with the detector. Are you sure you want to delete it?';
            messageTitle = 'Delete Detector';
        }
        else {

            mask = 'Deleting Detectors...';
            message = 'Deleting the selected detectors will result in the loss of all data associated with the detectors. Are you sure you want to delete them?';
            messageTitle = 'Delete Detectors';
        }

        Ext.Msg.confirm(messageTitle, message, function(btn) {

            if (btn === 'yes') {

                var detectorIds = [];
                selectedDetectors.forEach(function(value, index, array) {
                    detectorIds.push(value.get('id'));
                });

                var queryParams = Ext.Object.toQueryString({
                    detectorIds : detectorIds
                });

                view.mask(mask);
                Ext.Ajax.request({
                    scope : this,
                    method : 'DELETE',
                    url : ETexas.util.UrlProvider.getDetectorsUrl(selectedComposite, selectedSimulation) + '?' + queryParams,
                    success : function(response, opts) {

                        view.unmask();
                        me._updateDetectorStore();
                    },
                    failure : function(response, opts) {

                        view.unmask();
                    }
                });
            }
        });
    },

    /**
     * @method onDetectorSelectionChange
     * 
     * Handles the event that is generated when the user changes the selected detector(s).
     * 
     * @protected
     * @param {Ext.selection.Model} model The selection model.
     * @param {Ext.data.Model[]} selected The selected detectors.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onDetectorSelectionChange : function(model, selected, eOpts) {

        this.getViewModel().set('selectedDetector', selected.length === 1 ? selected[0] : null);
        this.getView().lookupReference('detectorToolbar').lookupReference('deleteButton').setDisabled(selected.length === 0);
    },

    /**
     * @method onHelpToolClicked
     * 
     * Handles the event that is generated when the user clicks the Detectors Help tool.
     * 
     * @protected
     * @param {Ext.panel.Tool} tool The clicked tool.
     * @param {Ext.event.Event} e The click event.
     * @param {Ext.Component} owner The owner of the tool.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onHelpToolClicked : function(tool, e, owner, eOpts) {

        window.open(ETexas.util.UrlProvider.getHelpContentUrl() + '/Simulations/Simulations/intro_detectors.htm');
    },

    /**
     * @method showCreateDetectorForm
     * 
     * Shows the form to create a new {@link ETexas.model.DetectorModel}.
     * 
     * @protected
     */
    showCreateDetectorForm : function() {

        var form = this.getView().add({
            xtype : 'createdetectorform',
            id : Ext.id(null, 'create-detector-form-')
        });

        form.on('detectoradded', this._updateDetectorStore, this);
        form.show();
    },

    /**
     * @method showEditDetectorForm
     * 
     * Shows the form to edit the selected {@link ETexas.model.DetectorModel}.
     * 
     * @protected
     */
    showEditDetectorForm : function() {

        var form = this.getView().add({
            xtype : 'editdetectorform',
            id : Ext.id(null, 'edit-detector-form-')
        });

        form.on('detectorupdated', this._updateDetectorStore, this);
        form.show();
    },

    /**
     * @method _updateDetectorStore
     * 
     * Updates the detector data for the selected simulation.
     * 
     * @private
     * @param {Number} [detectorId] The ID of the selected detector.
     */
    _updateDetectorStore : function(detectorId) {

        var viewModel = this.getViewModel();
        var detectorStore = viewModel.get('detectorStore');
        var detectorGrid = this.getView().lookupReference('detectorGrid');
        detectorStore.load(function(records, operation, success) {
            var selectedDetector = detectorId ? detectorStore.getById(detectorId) : null;
            detectorGrid.setSelection(selectedDetector);
            viewModel.set('selectedDetector', selectedDetector);
        });
    }
});
