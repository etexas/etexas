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
 * @class ETexas.view.deviceprofile.DeviceProfileFormController
 * @extends ETexas.view.model.ModelFormController
 * 
 * The {@link ETexas.view.deviceprofile.DeviceProfileForm} controller.
 * 
 * @abstract
 * @author emyers
 */
Ext.define('ETexas.view.deviceprofile.DeviceProfileFormController', {
    extend : 'ETexas.view.model.ModelFormController',

    /** @inheritdoc */
    init : function(view) {

        view.lookupReference('nameField').validator = Ext.bind(this.validateNameField, this);
        view.lookupReference('percentageField').validator = Ext.bind(this.validatePercentageField, this);
        this.callParent([ view ]);
    },

    /**
     * @method validateNameField
     * 
     * Validates the current value of the name field.
     * 
     * @protected
     * @param {String} value The current field value.
     * @return {Boolean/String} True if the current field value is valid, otherwise a string
     * validation message is returned.
     */
    validateNameField : function(value) {

        value = value.trim();

        if (value.length === 0) {

            return 'A valid device profile name is required.';
        }

        var deviceProfileStore = this.getViewModel().get('deviceProfileStore');

        if (deviceProfileStore.query('name', value, false, false, true).length !== 0) {

            return 'A device profile with the name \"' + value + '\" already exists in the composite.';
        }

        return true;
    },

    /**
     * @method validatePercentageField
     * 
     * Validates the current value of the percentage field.
     * 
     * @protected
     * @param {String} value The current field value.
     * @param {Number} [deviceProfileId] The ID of the selected device profile.
     * @return {Boolean/String} True if the current field value is valid, otherwise a string
     * validation message is returned.
     */
    validatePercentageField : function(value, deviceProfileId) {

        if (value.length === 0) {

            return 'A valid device profile percentage is required.';
        }

        if (value < 1 || value > 100) {

            return 'Device profile percentages must be in the range of 1 percent to 100 percent.';
        }

        return true;
    }
});
