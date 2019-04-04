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
 * @class ETexas.util.Validation
 * @singleton
 * 
 * Provides global utility functions to validate common field values.
 * 
 * @author emyers
 */
Ext.define('ETexas.util.Validation', {
    singleton : true,

    /**
     * @method validatePassword
     * 
     * Validates the specified password value.
     * 
     * @public
     * @param {String} value The password value.
     * @return {Boolean/String} True if the password value is valid, otherwise a string validation
     * message is returned.
     */
    validatePassword : function(value) {

        value = value.trim();

        if (value.length === 0) {

            return 'A valid password is required.';
        }

        if (!/^(?=.*[a-z])(?=.*[A-Z])(?=.*[0-9])(?=.*[!@#\$%\^&\*])(?=.{15,})/.test(value)) {

            var message = 'Passwords must meet the following minimum requirements:';
            message = message.concat('<p><ul><li>Contain at least 15 total characters</li>');
            message = message.concat('<li>Contain at least 1 digit</li>');
            message = message.concat('<li>Contain at least 1 uppercase letter</li>');
            message = message.concat('<li>Contain at least 1 lowercase letter</li>');
            message = message.concat('<li>Contain at least 1 special character (!@#$%^&*)</li></ul></p>');
            return message;
        }

        return true;
    }
});
