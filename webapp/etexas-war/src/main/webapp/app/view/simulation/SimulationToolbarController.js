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
 * @class ETexas.view.simulation.SimulationToolbarController
 * @extends ETexas.view.model.ModelToolbarController
 * 
 * The {@link ETexas.view.simulation.SimulationToolbar} controller.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.simulation.SimulationToolbarController', {
    extend : 'ETexas.view.model.ModelToolbarController',
    alias : 'controller.simulationtoolbar',

    /** @inheritdoc */
    getBubbleEvents : function() {

        return Ext.Array.push(this.callParent(), 'copysimulation', 'createsimulation', 'detectors', 'exportsimulation', 'updatesimulation', 'simulationsource', 'uploadsimulation');
    },

    /**
     * @method onCopyItemClicked
     * 
     * Handles the event that is generated when the user clicks the Copy menu item.
     * 
     * @protected
     * @param {Ext.menu.Item} The clicked menu item.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onCopyItemClicked : function(button, e, eOpts) {

        this.getView().fireEvent('copysimulation');
    },

    /**
     * @method onDetectorsItemClicked
     * 
     * Handles the event that is generated when the user clicks the Detectors menu item.
     * 
     * @protected
     * @param {Ext.menu.Item} The clicked menu item.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onDetectorsItemClicked : function(button, e, eOpts) {

        this.getView().fireEvent('detectors');
    },

    /**
     * @method onExportItemClicked
     * 
     * Handles the event that is generated when the user clicks the Export menu item.
     * 
     * @protected
     * @param {Ext.menu.Item} The clicked menu item.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onExportItemClicked : function(button, e, eOpts) {

        this.getView().fireEvent('exportsimulation');
    },

    /**
     * @method onUpdateItemClicked
     * 
     * Handles the event that is generated when the user clicks the Update menu item.
     * 
     * @protected
     * @param {Ext.menu.Item} The clicked menu item.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onUpdateItemClicked : function(button, e, eOpts) {

        this.getView().fireEvent('updatesimulation');
    },

    /**
     * @method onSourceItemClicked
     * 
     * Handles the event that is generated when the user clicks the Source Files menu item.
     * 
     * @protected
     * @param {Ext.menu.Item} The clicked menu item.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onSourceItemClicked : function(button, e, eOpts) {

        this.getView().fireEvent('simulationsource');
    },

    /**
     * @method onTemplateItemClicked
     * 
     * Handles the event that is generated when the user clicks the from Template menu item.
     * 
     * @protected
     * @param {Ext.menu.Item} The clicked menu item.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onTemplateItemClicked : function(button, e, eOpts) {

        this.getView().fireEvent('createsimulation');
    },

    /**
     * @method onUploadItemClicked
     * 
     * Handles the event that is generated when the user clicks the from Upload menu item.
     * 
     * @protected
     * @param {Ext.menu.Item} The clicked menu item.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onUploadItemClicked : function(button, e, eOpts) {

        this.getView().fireEvent('uploadsimulation');
    }
});
