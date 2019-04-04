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
 * @class ETexas.view.signal.SignalGrid
 * @extends Ext.grid.Panel
 * 
 * A grid panel to view {@link ETexas.model.SignalModel} data.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.signal.SignalGrid', {
    extend : 'Ext.grid.Panel',
    xtype : 'signalgrid',

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
            reference : 'laneColumn',
            id : Ext.id(null, 'lane-column-'),
            dataIndex : 'laneId',
            text : 'Lane',
            flex : 1
        }, {
            reference : 'typeColumn',
            id : Ext.id(null, 'type-column-'),
            dataIndex : 'typeIndication',
            text : 'Type',
            flex : 1
        }, {
            reference : 'stateColumn',
            id : Ext.id(null, 'state-column-'),
            dataIndex : 'stateIndication',
            text : 'State',
            flex : 1
        }, {
            reference : 'colorColumn',
            id : Ext.id(null, 'color-column-'),
            dataIndex : 'colorIndication',
            text : 'Color',
            flex : 1
        }, {
            reference : 'changeColumn',
            id : Ext.id(null, 'change-column-'),
            dataIndex : 'timeToChange',
            text : 'Time to Change (s)',
            flex : 1
        } ];
    }
});
