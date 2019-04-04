/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
/**
 * @class ETexas.util.Functions
 * @singleton
 * 
 * Provides global utility functions.
 * 
 * @author emyers
 */
Ext.define('ETexas.util.Functions', {
    singleton : true,

    /**
     * @method showExceptionMessage
     * 
     * Shows an exception message for a JSON exception object.
     * 
     * @public
     * @param {Object} The JSON exception.
     */
    showExceptionMessage : function(exception) {

        var message;

        if (exception.exceptionType === 'ValidationException') {

            message = this._buildValidationMessage(exception.messages);
        }
        else {

            message = exception.messages[0];
        }

        Ext.Msg.alert(exception.title, message);
    },

    /**
     * @method _buildValidationMessage
     * 
     * Converts the array of validation messages into a single list message.
     * 
     * @private
     * @param {String[]} messages The array of validation messages.
     * @return {String} A single list message.
     */
    _buildValidationMessage : function(messages) {

        var message = 'The following issue(s) were found during validation:<ul>';

        messages.forEach(function(item, index) {

            message = message.concat('<li>' + item + '</li>');
        });

        message = message.concat('</ul>');

        return message;
    }
});