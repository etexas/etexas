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
 * @class ETexas.view.deviceprofile.CellularDeviceProfileForm
 * @extends ETexas.view.deviceprofile.DeviceProfileForm
 * 
 * An abstract form panel for cellular {@link ETexas.model.DeviceProfileModel} operations.
 * 
 * @abstract
 * @author emyers
 */
Ext.define('ETexas.view.deviceprofile.CellularDeviceProfileForm', {
    extend : 'ETexas.view.deviceprofile.DeviceProfileForm',

    /** @inheritdoc */
    buildItems : function() {

        var minDevicesMsg = 'The minimum number of devices per affected vehicle must be in the range of 0 devices to 8 devices.';
        var maxDevicesMsg = 'The maximum number of devices per affected vehicle must be in the range of 0 devices to 8 devices.';

        return Ext.Array.insert(this.callParent(), 1, [ {
            xtype : 'numberfield',
            reference : 'minDevicesField',
            id : Ext.id(null, 'min-devices-field-'),
            name : 'minDevices',
            fieldLabel : 'Min Devices',
            allowDecimals : false,
            allowExponential : false
        }, {
            xtype : 'numberfield',
            reference : 'maxDevicesField',
            id : Ext.id(null, 'max-devices-field-'),
            name : 'maxDevices',
            fieldLabel : 'Max Devices',
            allowDecimals : false,
            allowExponential : false
        } ]);
    }
});
