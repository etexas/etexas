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
 * @class ETexas.view.message.MessageGrid
 * @extends Ext.grid.Panel
 * 
 * A grid panel to view {@link ETexas.model.MessageModel} data.
 * 
 * @author ttevendale
 */
Ext.define('ETexas.view.message.MessageGrid', {
    extend : 'Ext.grid.Panel',
    xtype : 'messagegrid',

    /** @inheritdoc */
    initComponent : function() {

        this.columns = this.buildColumns();
        this.callParent();
    },

    /**
     * @method buildColumns
     * 
     * Builds the columns for this grid panel.
     * 
     * @template
     * @protected
     * @return {Ext.grid.column.Column[]} The columns for this grid panel.
     */
    buildColumns : function() {

        return [ {
            reference : 'messageIdColumn',
            id : Ext.id(null, 'message-id-column-'),
            dataIndex : 'messageId',
            text : 'Message ID',
            flex : 1
        }, {
            reference : 'originAddressColumn',
            id : Ext.id(null, 'origin-address-column-'),
            dataIndex : 'originAddress',
            text : 'Origin MAC Address',
            flex : 2
        }, {
            reference : 'destinationAddressColumn',
            id : Ext.id(null, 'destination-address-column-'),
            dataIndex : 'destinationAddress',
            text : 'Destination MAC Address',
            flex : 2
        }, {
            reference : 'channelColumn',
            id : Ext.id(null, 'channel-column-'),
            dataIndex : 'channel',
            text : 'Channel',
            renderer : function(value, meta, record, rowIndex) {

                var channel = record.get('channel');
                if (!channel) {

                    value = 'N/A';
                }

                return value;
            },
            flex : 2
        }, {
            reference : 'messageTypeColumn',
            id : Ext.id(null, 'message-type-column-'),
            dataIndex : 'messageType',
            text : 'Type',
            flex : 2
        }, {
            reference : 'messageColumn',
            id : Ext.id(null, 'message-column-'),
            dataIndex : 'message',
            text : 'Data',
            cellWrap : true,
            flex : 4
        } ];
    }
});
