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
 * @class ETexas.view.simulation.CopySimulationFormController
 * @extends ETexas.view.simulation.SimulationFormController
 * 
 * The {@link ETexas.view.simulation.CopySimulationForm} controller.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.simulation.CopySimulationFormController', {
    extend : 'ETexas.view.simulation.SimulationFormController',
    alias : 'controller.copysimulationform',

    /** @inheritdoc */
    init : function(view) {

        var selectedComposite = this.getViewModel().get('selectedComposite');
        var selectedSimulation = this.getViewModel().get('selectedSimulation');

        view.getForm().setValues({
            x : selectedSimulation.get('x'),
            y : selectedSimulation.get('y'),
            targetCompositeId : selectedComposite.get('id')
        });

        this.callParent([ view ]);
    },

    /**
     * @method onCompositeChange
     * 
     * Handles the event that is generated when the selected composite changes.
     * 
     * @protected
     * @param {Ext.form.field.Field} field The changed field.
     * @param {Object} newValue The new value.
     * @param {Object} oldValue The original value.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onCompositeChange : function(field, newValue, oldValue, eOpts) {

        var nameField = this.lookupReference('nameField');

        if (nameField.getValue().length !== 0) {

            nameField.validate();
        }
    },

    /**
     * @method onCopyButtonClicked
     * 
     * Handles the event that is generated when the user clicks the Copy button.
     * 
     * @protected
     * @param {Ext.button.Button} button The clicked button.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onCopyButtonClicked : function(button, e, eOpts) {

        var view = this.getView();

        if (view.isValid()) {

            var selectedComposite = this.getViewModel().get('selectedComposite');
            var selectedSimulation = this.getViewModel().get('selectedSimulation');
            var targetCompositeId = view.lookupReference('compositeBox').getSelection().get('id');

            view.mask('Copying Simulation...');
            view.submit({
                scope : this,
                method : 'POST',
                url : ETexas.util.UrlProvider.getSimulationsUrl(selectedComposite, selectedSimulation),
                success : function(form, action) {

                    var simulation = Ext.JSON.decode(action.response.responseText);
                    view.fireEvent('simulationcopied', targetCompositeId, simulation.id);
                    Ext.destroy(view);
                },
                failure : function(form, action) {

                    view.unmask();
                }
            });
        }
    },

    /**
     * @method onHelpToolClicked
     * 
     * Handles the event that is generated when the user clicks the Copy Simulation Help tool.
     * 
     * @protected
     * @param {Ext.panel.Tool} tool The clicked tool.
     * @param {Ext.event.Event} e The click event.
     * @param {Ext.Component} owner The owner of the tool.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onHelpToolClicked : function(tool, e, owner, eOpts) {

        window.open(ETexas.util.UrlProvider.getHelpContentUrl() + '/Simulations/Simulations/copy_sim.htm');
    },

    /** @inheritdoc */
    validateNameField : function(value, simulationStore) {

        var selectedComposite = this.lookupReference('compositeBox').getSelection();
        return this.callParent([ value, selectedComposite ? selectedComposite.getSimulations() : null ]);
    }
});