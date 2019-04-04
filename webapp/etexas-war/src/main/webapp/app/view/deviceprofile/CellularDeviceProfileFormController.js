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
 * @class ETexas.view.deviceprofile.CellularDeviceProfileFormController
 * @extends ETexas.view.deviceprofile.DeviceProfileFormController
 * 
 * The {@link ETexas.view.deviceprofile.CellularDeviceProfileForm} controller.
 * 
 * @abstract
 * @author emyers
 */
Ext.define('ETexas.view.deviceprofile.CellularDeviceProfileFormController', {
    extend : 'ETexas.view.deviceprofile.DeviceProfileFormController',

    /** @inheritdoc */
    init : function(view) {

        view.lookupReference('minDevicesField').validator = Ext.bind(this.validateMinDevicesField, this);
        view.lookupReference('maxDevicesField').validator = Ext.bind(this.validateMaxDevicesField, this);
        this.callParent([ view ]);
    },

    /**
     * @method validateMinDevicesField
     * 
     * Validates the current value of the minimum devices field.
     * 
     * @protected
     * @param {String} value The current field value.
     * @return {Boolean/String} True if the current field value is valid, otherwise a string
     * validation message is returned.
     */
    validateMinDevicesField : function(value) {

        if (value.length === 0) {

            return 'A valid minimum number of devices per affected vehicle is required.';
        }

        if (value < 0 || value > 8) {

            return 'The minimum number of devices per affected vehicle must be in the range of 0 devices to 8 devices.';
        }

        var maxDevicesField = this.lookupReference('maxDevicesField');
        var maxDevices = maxDevicesField.getValue();

        if (maxDevices) {

            if (maxDevices < value) {

                return 'The minimum number of devices per affected vehicle may not exceed the maximum number of devices per affected vehicle.';
            }

            maxDevicesField.clearInvalid();

            return true;
        }

        return true;
    },

    /**
     * @method validateMaxDevicesField
     * 
     * Validates the current value of the maximum devices field.
     * 
     * @protected
     * @param {String} value The current field value.
     * @return {Boolean/String} True if the current field value is valid, otherwise a string
     * validation message is returned.
     */
    validateMaxDevicesField : function(value) {

        if (value.length === 0) {

            return 'A valid maximum number of devices per affected vehicle is required.';
        }

        if (value < 0 || value > 8) {

            return 'The maximum number of devices per affected vehicle must be in the range of 0 devices to 8 devices.';
        }

        var minDevicesField = this.lookupReference('minDevicesField');
        var minDevices = minDevicesField.getValue();

        if (minDevices) {

            if (value < minDevices) {

                return 'The maximum number of devices per affected vehicle may not be less than the minimum number of devices per affected vehicle.';
            }

            minDevicesField.validate();

            return true;
        }

        return true;
    },

    /** @inheritdoc */
    validatePercentageField : function(value, deviceProfileId) {

        var parentResult = this.callParent([ value, deviceProfileId ]);

        if (parentResult !== true) {

            return parentResult;
        }

        var totalPercentage = parseFloat(value);
        var cellularDeviceProfileStore = this.getViewModel().get('cellularDeviceProfileStore');

        cellularDeviceProfileStore.each(function(record) {
            if (!deviceProfileId || record.get('id') !== deviceProfileId) {
                totalPercentage += record.get('percentage');
            }
        });

        if (totalPercentage > 100) {

            return 'The total percentage for all cellular device profiles may not exceed 100 percent.';
        }

        return true;
    }
});
