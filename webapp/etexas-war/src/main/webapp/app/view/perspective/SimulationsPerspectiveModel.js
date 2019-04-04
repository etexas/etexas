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
 * @class ETexas.view.perspective.SimulationsPerspectiveModel
 * @extends Ext.app.ViewModel
 * 
 * The {@link ETexas.view.perspective.SimulationsPerspective} model.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.perspective.SimulationsPerspectiveModel', {
    extend : 'Ext.app.ViewModel',
    alias : 'viewmodel.simulationsperspective',

    requires : [ 'ETexas.store.SimulationStore' ],

    data : {

        /** @property {ETexas.model.CompositeModel} selectedComposite The selected composite. */
        selectedComposite : null,

        /** @property {ETexas.model.SimulationModel} selectedSimulation The selected simulation. */
        selectedSimulation : null,

        /** @property {String} webStartUrl The URL for Web Start services. */
        webStartUrl : null
    },

    formulas : {

        /**
         * @property {ETexas.store.SimulationStore} simulationStore The
         * {@link ETexas.model.SimulationModel} data for the selected composite.
         */
        simulationStore : function(get) {

            var selectedComposite = get('selectedComposite');
            return selectedComposite ? selectedComposite.getSimulations() : null;
        }
    }
});
