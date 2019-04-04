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
 * @class ETexas.view.lanemapping.LaneMappingFormModel
 * @extends Ext.app.ViewModel
 * 
 * The {@link ETexas.view.lanemapping.LaneMappingForm} model.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.lanemapping.LaneMappingFormModel', {
    extend : 'Ext.app.ViewModel',
    alias : 'viewmodel.lanemappingform',

    requires : [ 'ETexas.store.LaneStore' ],

    data : {

        /** @property {ETexas.model.LaneModel} selectedSourceLane The selected source lane. */
        selectedSourceLane : null,

        /**
         * @property {ETexas.model.SimulationModel} selectedSourceSimulation The selected source
         * simulation.
         */
        selectedSourceSimulation : null,

        /** @property {ETexas.model.LaneModel} selectedTargetLane The selected target lane. */
        selectedTargetLane : null,

        /**
         * @property {ETexas.model.SimulationModel} selectedTargetSimulation The selected target
         * simulation.
         */
        selectedTargetSimulation : null
    },

    formulas : {

        /**
         * @property {ETexas.store.SimulationStore} simulationStore The
         * {@link ETexas.model.SimulationModel} data for the selected composite.
         */
        simulationStore : function(get) {

            return get('selectedComposite').getSimulations();
        }
    },

    stores : {

        /**
         * @property {ETexas.store.LaneStore} laneStore The {@link ETexas.model.LaneModel} data for
         * the selected composite.
         */
        laneStore : {
            type : 'lane',
            sorters : 'laneId'
        },

        /**
         * @property {ETexas.store.LaneStore} sourceLaneStore The source
         * {@link ETexas.model.LaneModel} data for the selected simulation.
         */
        sourceLaneStore : {
            type : 'lane',
            sorters : 'laneId'
        },

        /**
         * @property {ETexas.store.LaneStore} targetLaneStore The target
         * {@link ETexas.model.LaneModel} data for the selected simulation.
         */
        targetLaneStore : {
            type : 'lane',
            sorters : 'laneId'
        }
    }
});
