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
 * @class ETexas.view.perspective.ExecutionsPerspectiveModel
 * @extends Ext.app.ViewModel
 * 
 * The {@link ETexas.view.perspective.ExecutionsPerspective} model.
 * 
 * @author emyers
 * @author ttevendale
 */
Ext.define('ETexas.view.perspective.ExecutionsPerspectiveModel', {
    extend : 'Ext.app.ViewModel',
    alias : 'viewmodel.executionsperspective',

    requires : [ 'ETexas.store.CellTowerStore', 'ETexas.store.CommandStore', 'ETexas.store.DeployedDetectorStore', 'ETexas.store.ExecutionStore', 'ETexas.store.LaneStore',
            'ETexas.store.MessageStore', 'ETexas.store.SignalStore', 'ETexas.store.StandaloneDeviceStore', 'ETexas.store.TopographyFeatureStore', 'ETexas.store.VehicleStore' ],

    data : {

        /** @property {ETexas.model.CompositeModel} selectedComposite The selected composite. */
        selectedComposite : null,

        /** @property {ETexas.model.ExecutionModel} selectedExecution The selected execution. */
        selectedExecution : null,

        /**
         * @private
         * @property {int} _currentExecutionId The current Execution ID.
         */
        _currentExecutionId : null
    },

    formulas : {

        /** @property {String} cellTowersUrl The URL for cell tower REST services. */
        cellTowersUrl : function(get) {

            var selectedComposite = get('selectedComposite');
            return selectedComposite ? ETexas.util.UrlProvider.getCellTowersUrl(selectedComposite) : null;
        },

        /** @property {String} emptyText The empty text for grids without completed execution data. */
        emptyText : function(get) {

            var selectedExecution = get('selectedExecution');

            if (selectedExecution && selectedExecution.get('status') === 'Complete') {

                return 'Data of this type is not available for completed executions.';
            }

            return '';
        },

        /** @property {String} executionsUrl The URL for execution REST services. */
        executionsUrl : function(get) {

            var selectedComposite = get('selectedComposite');
            return selectedComposite ? ETexas.util.UrlProvider.getExecutionsUrl(selectedComposite) : null;
        },

        /** @property {String} messagesUrl The URL for message REST services. */
        messagesUrl : function(get) {

            var selectedComposite = get('selectedComposite');
            var selectedExecution = get('selectedExecution');
            return ETexas.util.UrlProvider.getMessagesUrl(selectedComposite, selectedExecution);
        },

        /** @property {String/Number} remainingSteps The steps remaining in the selected execution. */
        remainingSteps : function(get) {

            var selectedExecution = get('selectedExecution');

            if (!selectedExecution || selectedExecution.get('status') === 'Error') {

                return 'N/A';
            }

            return selectedExecution.get('maxSteps') - selectedExecution.get('stepNumber');
        },

        /** @property {Number} simulationTime The current simulation time. */
        simulationTime : function(get) {

            var selectedExecution = get('selectedExecution');

            if (!selectedExecution || selectedExecution.get('status') === 'Error') {

                return 0;
            }

            return selectedExecution.get('stepSize') * selectedExecution.get('stepNumber');
        },

        /** @property {String} standaloneDevicesUrl The URL for standalone device REST services. */
        standaloneDevicesUrl : function(get) {

            var selectedComposite = get('selectedComposite');
            var selectedExecution = get('selectedExecution');
            return selectedExecution ? ETexas.util.UrlProvider.getStandaloneDevicesUrl(selectedComposite, selectedExecution) : null;
        },

        /** @property {String} topographyFeaturesUrl The URL for topography feature REST services. */
        topographyFeaturesUrl : function(get) {

            var selectedComposite = get('selectedComposite');

            if (!selectedComposite) {

                return null;
            }

            return ETexas.util.UrlProvider.getTopographyFeaturesUrl(selectedComposite);
        }
    },

    stores : {

        /**
         * @property {ETexas.store.CellTowerStore} cellTowerStore The
         * {@link ETexas.model.CellTowerModel} data for the selected execution.
         */
        cellTowerStore : {
            type : 'celltower',
            sorters : 'provider',
            proxy : {
                type : 'ajax',
                url : '{cellTowersUrl}'
            }
        },

        /**
         * @property {ETexas.store.CommandStore} commandStore The {@link ETexas.model.CommandModel}
         * data for the selected execution.
         */
        commandStore : {
            type : 'command',
            sorters : 'submissionTime',
            groupField : 'simulationName'
        },

        /**
         * @property {ETexas.store.DeployedDetectorStore} detectorStore The
         * {@link ETexas.model.DeployedDetectorModel} data for the selected execution.
         */
        detectorStore : {
            type : 'deployeddetector',
            sorters : 'detectorID',
            groupField : 'simulationName'
        },

        /**
         * @property {ETexas.store.ExecutionStore} executionStore The
         * {@link ETexas.model.ExecutionModel} data for the selected composite.
         */
        executionStore : {
            type : 'execution',
            sorters : 'name',
            proxy : {
                type : 'ajax',
                url : '{executionsUrl}'
            }
        },

        /**
         * @property {ETexas.store.LaneStore} laneStore The {@link ETexas.model.LaneModel} data for
         * the selected execution.
         */
        laneStore : {
            type : 'lane',
            sorters : 'laneId',
            groupField : 'simulationName'
        },

        /**
         * @property {ETexas.store.MessageStore} messageStore The {@link ETexas.model.MessageModel}
         * data for the selected execution.
         */
        messageStore : {
            type : 'message',
            proxy : {
                type : 'ajax',
                url : '{messagesUrl}'
            }
        },

        /**
         * @property {ETexas.store.SignalStore} signalStore The {@link ETexas.model.SignalModel}
         * data for the selected execution.
         */
        signalStore : {
            type : 'signal',
            sorters : 'laneId',
            groupField : 'simulationName'
        },

        /**
         * @property {ETexas.store.StandaloneDeviceStore} standaloneDeviceStore The
         * {@link ETexas.model.StandaloneDeviceModel} data for the selected execution.
         */
        standaloneDeviceStore : {
            type : 'standalonedevice',
            sorters : 'deviceMac',
            proxy : {
                type : 'ajax',
                url : '{standaloneDevicesUrl}'
            }
        },

        /**
         * @property {ETexas.store.TopographyFeatureStore} topographyFeatureStore The
         * {@link ETexas.model.TopographyFeatureModel} data for the selected execution.
         */
        topographyFeatureStore : {
            type : 'topographyfeature',
            proxy : {
                type : 'ajax',
                url : '{topographyFeaturesUrl}'
            }
        },

        /**
         * @property {ETexas.store.TopographyFeatureStore} buildingStore The building
         * {@link ETexas.model.TopographyFeatureModel} data for the selected execution.
         */
        buildingStore : {
            source : '{topographyFeatureStore}',
            sorters : 'name',
            filters : [ {
                property : 'type',
                value : 'BUILDING'
            } ]
        },

        /**
         * @property {ETexas.store.VehicleStore} vehicleStore The {@link ETexas.model.VehicleModel}
         * data for the selected execution.
         */
        vehicleStore : {
            type : 'vehicle',
            sorters : 'vehicleID',
            groupField : 'simulationName'
        }
    }
});
