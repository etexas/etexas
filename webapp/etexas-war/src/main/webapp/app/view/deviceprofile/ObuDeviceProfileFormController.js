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
 * @class ETexas.view.deviceprofile.ObuDeviceProfileFormController
 * @extends ETexas.view.deviceprofile.DeviceProfileFormController
 * 
 * The abstract parent form panel view controller for OBU device profile forms.
 * 
 * @abstract
 * @author emyers
 */
Ext.define('ETexas.view.deviceprofile.ObuDeviceProfileFormController', {
    extend : 'ETexas.view.deviceprofile.DeviceProfileFormController',

    /** @inheritdoc */
    validatePercentageField : function(value, deviceProfileId) {

        var parentResult = this.callParent([ value, deviceProfileId ]);

        if (parentResult !== true) {

            return parentResult;
        }

        var totalPercentage = parseFloat(value);
        var obuDeviceProfileStore = this.getViewModel().get('obuDeviceProfileStore');

        obuDeviceProfileStore.each(function(record) {
            if (!deviceProfileId || record.get('id') !== deviceProfileId) {
                totalPercentage += record.get('percentage');
            }
        });

        if (totalPercentage > 100) {

            return 'The total percentage for all OBU device profiles may not exceed 100 percent.';
        }

        return true;
    }
});
