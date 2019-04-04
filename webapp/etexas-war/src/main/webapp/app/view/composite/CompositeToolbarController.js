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
 * @class ETexas.view.composite.CompositeToolbarController
 * @extends ETexas.view.model.ModelToolbarController
 * 
 * The {@link ETexas.view.composite.CompositeToolbar} controller.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.composite.CompositeToolbarController', {
    extend : 'ETexas.view.model.ModelToolbarController',
    alias : 'controller.compositetoolbar',

    /** @inheritdoc */
    getBubbleEvents : function() {

        return Ext.Array.push(this.callParent(), 'compositereporting', 'compositesettings', 'copycomposite', 'exportcomposite', 'renamecomposite');
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

        this.getView().fireEvent('copycomposite');
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

        this.getView().fireEvent('exportcomposite');
    },

    /**
     * @method onRenameItemClicked
     * 
     * Handles the event that is generated when the user clicks the Rename menu item.
     * 
     * @protected
     * @param {Ext.menu.Item} The clicked menu item.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onRenameItemClicked : function(button, e, eOpts) {

        this.getView().fireEvent('renamecomposite');
    },

    /**
     * @method onReportingItemClicked
     * 
     * Handles the event that is generated when the user clicks the Reporting menu item.
     * 
     * @protected
     * @param {Ext.menu.Item} The clicked menu item.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onReportingItemClicked : function(button, e, eOpts) {

        this.getView().fireEvent('compositereporting');
    },

    /**
     * @method onSettingsItemClicked
     * 
     * Handles the event that is generated when the user clicks the Settings menu item.
     * 
     * @protected
     * @param {Ext.menu.Item} The clicked menu item.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onSettingsItemClicked : function(button, e, eOpts) {

        this.getView().fireEvent('compositesettings');
    }
});
