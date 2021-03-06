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
 * @class ETexas.view.command.CommandFormController
 * @extends ETexas.view.model.ModelFormController
 * 
 * The {@link ETexas.view.command.CommandForm} controller.
 * 
 * @abstract
 * @author emyers
 */
Ext.define('ETexas.view.command.CommandFormController', {
    extend : 'ETexas.view.model.ModelFormController',

    /**
     * @method onCreateButtonClicked
     * 
     * Handles the event that is generated when the user clicks the Create button.
     * 
     * @abstract
     * @protected
     * @param {Ext.button.Button} button The clicked button.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onCreateButtonClicked : function(button, e, eOpts) {},

    /**
     * @method onSimulationChange
     * 
     * Handles the event that is generated when the user selects a simulation.
     * 
     * @abstract
     * @protected
     * @param {Ext.form.field.Field} field The changed field.
     * @param {Object} newValue The new value.
     * @param {Object} oldValue The original value.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onSimulationChange : function(field, newValue, oldValue, eOpts) {}
});
