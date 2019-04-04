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
 * @class ETexas.view.simulation.SimulationToolbar
 * @extends ETexas.view.model.ModelToolbar
 * 
 * A toolbar to initiate {@link ETexas.model.SimulationModel} operations.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.simulation.SimulationToolbar', {
    extend : 'ETexas.view.model.ModelToolbar',
    xtype : 'simulationtoolbar',

    requires : [ 'ETexas.view.simulation.SimulationToolbarController' ],

    controller : 'simulationtoolbar',

    /** @inheritdoc */
    buildItems : function() {

        var items = this.callParent();
        items[0].menu = [ {
            reference : 'templateItem',
            id : Ext.id(null, 'template-item-'),
            handler : 'onTemplateItemClicked',
            text : 'from Template'
        }, {
            reference : 'uploadItem',
            id : Ext.id(null, 'upload-item-'),
            handler : 'onUploadItemClicked',
            text : 'from Upload'
        } ];

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
            reference : 'updateItem',
            id : Ext.id(null, 'update-item-'),
            handler : 'onUpdateItemClicked',
            text : 'Update'
        }, '-', {
            reference : 'detectorsItem',
            id : Ext.id(null, 'detectors-item-'),
            handler : 'onDetectorsItemClicked',
            text : 'Detectors'
        }, '-', {
            reference : 'sourceItem',
            id : Ext.id(null, 'source-item-'),
            handler : 'onSourceItemClicked',
            text : 'Source Files'
        } ];

        return items;
    }
});
