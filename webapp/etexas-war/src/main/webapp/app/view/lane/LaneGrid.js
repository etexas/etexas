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
 * @class ETexas.view.lane.LaneGrid
 * @extends ETexas.view.model.ModelGrid
 * 
 * A grid panel to view {@link ETexas.model.LaneModel} data.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.lane.LaneGrid', {
    extend : 'ETexas.view.model.ModelGrid',
    xtype : 'lanegrid',

    /** @inheritdoc */
    buildColumns : function() {

        return [ {
            reference : 'idColumn',
            id : Ext.id(null, 'id-column-'),
            dataIndex : 'laneId',
            text : 'ID',
            flex : 1
        }, {
            reference : 'approachColumn',
            id : Ext.id(null, 'approach-column-'),
            dataIndex : 'approachId',
            text : 'Approach',
            flex : 2
        }, {
            reference : 'typeColumn',
            id : Ext.id(null, 'type-column-'),
            dataIndex : 'type',
            text : 'Type',
            flex : 2
        }, {
            xtype : 'templatecolumn',
            reference : 'movementsColumn',
            id : Ext.id(null, 'movements-column-'),
            tpl : new Ext.XTemplate('<ul class="condensed">', '<tpl foreach="laneMovements.entry">', '<li>{value.movement}</li>', '</tpl>', '</ul>'),
            text : 'Movements',
            flex : 2
        }, {
            reference : 'speedLimitColumn',
            id : Ext.id(null, 'speed-limit-column-'),
            dataIndex : 'speedLimitInMetersPerSecond',
            text : 'Speed Limit (m/s)',
            flex : 3,
            renderer : Ext.util.Format.numberRenderer('###,##0.0#')
        }, {
            xtype : 'templatecolumn',
            reference : 'nodesColumn',
            id : Ext.id(null, 'nodes-column-'),
            tpl : new Ext.XTemplate('<tpl for="laneGeomList">', '{#}: ({x}, {y}, {z}, {width})<br/>', '</tpl>'),
            text : 'Nodes (x, y, z, w) (cm)',
            flex : 3
        } ];
    }
});
