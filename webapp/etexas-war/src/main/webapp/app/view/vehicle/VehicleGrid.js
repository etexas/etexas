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
 * @class ETexas.view.vehicle.VehicleGrid
 * @extends Ext.grid.Panel
 * 
 * A grid panel to view {@link ETexas.model.VehicleModel} data.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.vehicle.VehicleGrid', {
    extend : 'Ext.grid.Panel',
    xtype : 'vehiclegrid',

    requires : [ 'ETexas.view.vehicle.VehicleGridController' ],

    controller : 'vehiclegrid',

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
            reference : 'idColumn',
            id : Ext.id(null, 'id-column-'),
            dataIndex : 'vehicleID',
            text : 'ID',
            flex : 1
        }, {
            reference : 'laneColumn',
            id : Ext.id(null, 'lane-column-'),
            dataIndex : 'laneID',
            text : 'Lane',
            flex : 1
        }, {
            reference : 'typeColumn',
            id : Ext.id(null, 'type-column-'),
            dataIndex : 'type',
            text : 'Type',
            flex : 2
        }, {
            reference : 'xColumn',
            id : Ext.id(null, 'x-column-'),
            dataIndex : 'x',
            text : 'X (cm)',
            flex : 1,
            renderer : Ext.util.Format.numberRenderer('###,##0.0#')
        }, {
            reference : 'yColumn',
            id : Ext.id(null, 'y-column-'),
            dataIndex : 'y',
            text : 'Y (cm)',
            flex : 1,
            renderer : Ext.util.Format.numberRenderer('###,##0.0#')
        }, {
            reference : 'speedColumn',
            id : Ext.id(null, 'speed-column-'),
            dataIndex : 'speed',
            text : 'Speed (m/s)',
            flex : 1,
            renderer : Ext.util.Format.numberRenderer('###,##0.0#')
        }, {
            xtype : 'widgetcolumn',
            reference : 'devicesColumn',
            id : Ext.id(null, 'devices-column-'),
            text : 'Devices',
            align : 'center',
            flex : 2,
            widget : {
                xtype : 'button',
                cls : 'device-button',
                tooltip : 'Show On Board Devices',
                handler : 'onDevicesButtonClicked'
            }
        } ];
    }
});
