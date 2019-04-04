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
 * @class ETexas.view.device.FixedCellularDeviceFormController
 * @extends ETexas.view.device.DeviceFormController
 * 
 * The {@link ETexas.view.device.FixedCellularDeviceForm} controller.
 * 
 * @abstract
 * @author emyers
 */
Ext.define('ETexas.view.device.FixedCellularDeviceFormController', {
    extend : 'ETexas.view.device.DeviceFormController',

    /** @inheritdoc */
    init : function(view) {

        view.lookupReference('macAddressField').validator = Ext.bind(this.validateMacAddressField, this);
        this.callParent([ view ]);
    },

    /**
     * @method validateMacAddressField
     * 
     * Validates the current value of the MAC address field.
     * 
     * @protected
     * @param {String} value The current field value.
     * @return {Boolean/String} True if the current field value is valid, otherwise a string
     * validation message is returned.
     */
    validateMacAddressField : function(value) {

        if (value.length === 0) {

            return 'A valid MAC address is required.';
        }

        var deviceStore = this.getViewModel().get('deviceStore');

        if (deviceStore.query('macAddress', value, false, false, true).length !== 0) {

            return 'A device with the MAC address \"' + value + '\" already exists in the composite.';
        }

        return true;
    }
});
