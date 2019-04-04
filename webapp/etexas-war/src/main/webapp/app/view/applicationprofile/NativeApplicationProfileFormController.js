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
 * @class ETexas.view.applicationprofile.NativeApplicationProfileFormController
 * @extends ETexas.view.applicationprofile.ApplicationProfileFormController
 * 
 * The {@link ETexas.view.application.NativeApplicationProfileForm} controller.
 * 
 * @abstract
 * @author emyers
 */
Ext.define('ETexas.view.applicationprofile.NativeApplicationProfileFormController', {
    extend : 'ETexas.view.applicationprofile.ApplicationProfileFormController',

    /** @inheritdoc */
    init : function(view) {

        view.lookupReference('hostAddressField').validator = Ext.bind(this.validateHostAddressField, this);
        this.callParent([ view ]);
    },

    /**
     * @method validateHostAddressField
     * 
     * Validates the current value of the host address field.
     * 
     * @protected
     * @param {String} value The current field value.
     * @return {Boolean/String} True if the current field value is valid, otherwise a string
     * validation message is returned.
     */
    validateHostAddressField : function(value) {

        value = value.trim();

        if (value.length === 0) {

            return 'A valid host address is required.';
        }

        if (!/^((\d{1}|[1-9]\d|[1]\d{2}|[2][0-4]\d|[2][5][0-5])\.){3}(\d{1}|[1-9]\d|[1]\d{2}|[2][0-4]\d|[2][5][0-5])$/.test(value)) {
            if (!/^(([0]{1}|[1-9a-fA-F][0-9a-fA-F]{0,3}):){7}([0]{1}|[1-9a-fA-F][0-9a-fA-F]{0,3})$/.test(value)) {

                return 'The value \"' + value + '\" is not a recognized IP address.';
            }
        }

        return true;
    }
});
