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
 * @class ETexas.view.message.MessageWindowModel
 * @extends Ext.app.ViewModel
 * 
 * The {@link ETexas.view.message.MessageWindow} model.
 * 
 * @author ttevendale
 */
Ext.define('ETexas.view.message.MessageWindowModel', {
    extend : 'Ext.app.ViewModel',
    alias : 'viewmodel.messagewindow',

    requires : [ 'ETexas.store.MessageStore' ],

    data : {

        /** @property {Number} deviceMac The MAC address of the selected device. */
        deviceMac : null
    },

    formulas : {

        /** @property {String} messagesByDeviceUrl The URL for messages by device REST services. */
        messagesByDeviceUrl : function(get) {

            return ETexas.util.UrlProvider.getMessagesByDeviceUrl(get('selectedComposite'), get('selectedExecution'), get('deviceMac'));
        }
    },

    stores : {

        /**
         * @property {ETexas.store.MessageStore} messageStore The {@link ETexas.model.MessageModel}
         * data for the selected device.
         */
        messageStore : {
            type : 'message',
            autoLoad : true,
            groupField : 'type',
            proxy : {
                type : 'ajax',
                url : '{messagesByDeviceUrl}'
            }
        }
    }
});
