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
 * @class ETexas.view.simulation.UpdateSimulationFormController
 * @extends ETexas.view.simulation.SimulationFormController
 * 
 * The {@link ETexas.view.simulation.UpdateSimulationForm} controller.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.simulation.UpdateSimulationFormController', {
    extend : 'ETexas.view.simulation.SimulationFormController',
    alias : 'controller.updatesimulationform',

    /** @inheritdoc */
    init : function(view) {

        var selectedSimulation = this.getViewModel().get('selectedSimulation');

        view.getForm().setValues({
            simulationName : selectedSimulation.get('name'),
            x : selectedSimulation.get('x'),
            y : selectedSimulation.get('y')
        });

        this.callParent([ view ]);
    },

    /**
     * @method onHelpToolClicked
     * 
     * Handles the event that is generated when the user clicks the Update Simulation Help tool.
     * 
     * @protected
     * @param {Ext.panel.Tool} tool The clicked tool.
     * @param {Ext.event.Event} e The click event.
     * @param {Ext.Component} owner The owner of the tool.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onHelpToolClicked : function(tool, e, owner, eOpts) {

        window.open(ETexas.util.UrlProvider.getHelpContentUrl() + '/Simulations/Simulations/rename_sim.htm');
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

        var view = this.getView();

        if (view.isValid()) {

            var selectedComposite = this.getViewModel().get('selectedComposite');
            var selectedSimulation = this.getViewModel().get('selectedSimulation');

            view.mask('Updating Simulation...');
            view.submit({
                scope : this,
                method : 'PUT',
                url : ETexas.util.UrlProvider.getSimulationsUrl(selectedComposite, selectedSimulation),
                success : function(form, action) {

                    view.fireEvent('simulationupdated', selectedComposite.get('id'), selectedSimulation.get('id'));
                    Ext.destroy(view);
                },
                failure : function(form, action) {

                    view.unmask();
                }
            });
        }
    },

    /** @inheritdoc */
    validateNameField : function(value, simulationStore) {

        value = value.trim();

        var selectedSimulation = this.getViewModel().get('selectedSimulation');

        if (selectedSimulation.get('name').toUpperCase() === value.toUpperCase()) {

            return true;
        }

        return this.callParent([ value, simulationStore ]);
    }
});
