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
 * @class ETexas.view.composite.CompositeToolbar
 * @extends ETexas.view.model.ModelToolbar
 * 
 * A toolbar to initiate {@link ETexas.model.CompositeModel} operations.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.composite.CompositeToolbar', {
    extend : 'ETexas.view.model.ModelToolbar',
    xtype : 'compositetoolbar',

    requires : [ 'ETexas.view.composite.CompositeToolbarController' ],

    controller : 'compositetoolbar',

    /** @inheritdoc */
    buildItems : function() {

        var items = this.callParent();
        items[1].menu = [ {
            reference : 'copyItem',
            id : Ext.id(null, 'copy-item-'),
            handler : 'onCopyItemClicked',
            text : 'Copy'
        }, {
            reference : 'exportItem',
            id : Ext.id(null, 'export-item-'),
            handler : 'onExportItemClicked',
            text : 'Export'
        }, {
            reference : 'renameItem',
            id : Ext.id(null, 'rename-item-'),
            handler : 'onRenameItemClicked',
            text : 'Rename'
        }, '-', {
            reference : 'settingsItem',
            id : Ext.id(null, 'settings-item-'),
            handler : 'onSettingsItemClicked',
            text : 'Settings'
        }, '-', {
            reference : 'reportingItem',
            id : Ext.id(null, 'reporting-item-'),
            handler : 'onReportingItemClicked',
            text : 'Reporting'
        } ];

        return items;
    }
});
