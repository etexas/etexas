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
 * @class ETexas.view.device.CreateFixedCellularDeviceForm
 * @extends ETexas.view.device.FixedCellularDeviceForm
 * 
 * A form panel to create a new fixed cellular {@link ETexas.model.DeviceModel}.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.device.CreateFixedCellularDeviceForm', {
    extend : 'ETexas.view.device.FixedCellularDeviceForm',
    xtype : 'createfixedcellulardeviceform',

    requires : [ 'ETexas.view.device.CreateFixedCellularDeviceFormController' ],

    controller : 'createfixedcellulardeviceform',

    title : 'Create Fixed Cellular Device',

    /** @inheritdoc */
    buildButtons : function() {

        return Ext.Array.insert(this.callParent(), 0, [ {
            reference : 'createButton',
            id : Ext.id(null, 'create-button-'),
            handler : 'onCreateButtonClicked',
            text : 'Create'
        } ]);
    },

    /** @inheritdoc */
    buildTools : function() {

        return Ext.Array.insert(this.callParent(), 0, [ {
            type : 'help',
            reference : 'helpTool',
            id : Ext.id(null, 'help-tool-'),
            callback : 'onHelpToolClicked',
            tooltip : 'Create Fixed Cellular Device Help'
        } ]);
    }
});
