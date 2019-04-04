/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
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
 * @class ETexas.model.MessageModel
 * @extends Ext.data.Model
 * 
 * A device message.
 * 
 * @author ttevendale
 */
Ext.define('ETexas.model.MessageModel', {
    extend : 'Ext.data.Model',

    fields : [ {
        name : 'messageId',
        type : 'int'
    }, {
        name : 'type',
        type : 'string'
    }, {
        name : 'originAddress',
        type : 'int'
    }, {
        name : 'destinationAddress',
        type : 'int',
        convert : function(value) {

            if (value === 281474976710655) {

                return "Broadcast";
            }
            return value;
        }
    }, {
        name : 'messageType',
        type : 'string'
    }, {
        name : 'message',
        type : 'string',
        convert : function(value, record) {

            if (value) {

                if (record.get('messageType') === 'Raw Bytes') {

                    var ret = [];
                    for (var i = 0; i < value.length; i += 2) {

                        ret.push(value.substr(i, 2));
                    }
                    return ret.join(' ');
                }
                else if (record.get('messageType') === 'Cell Message') {

                    return value.message;
                }
            }
        }
    } ]
});