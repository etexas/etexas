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
 * @class ETexas.view.celltower.CellTowerGrid
 * @extends ETexas.view.model.ModelGrid
 * 
 * A grid panel to view {@link ETexas.model.CellTowerModel} data.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.celltower.CellTowerGrid', {
    extend : 'ETexas.view.model.ModelGrid',
    xtype : 'celltowergrid',

    requires : [ 'ETexas.view.grid.column.NameColumn' ],

    /** @inheritdoc */
    buildColumns : function() {

        return Ext.Array.push(this.callParent(), {
            xtype : 'namecolumn',
            reference : 'providerColumn',
            id : Ext.id(null, 'provider-column-'),
            dataIndex : 'provider',
            text : 'Provider',
            flex : 3
        }, {
            reference : 'xColumn',
            id : Ext.id(null, 'x-column-'),
            dataIndex : 'x',
            text : 'X (cm)',
            flex : 2
        }, {
            reference : 'yColumn',
            id : Ext.id(null, 'y-column-'),
            dataIndex : 'y',
            text : 'Y (cm)',
            flex : 2
        }, {
            reference : 'zColumn',
            id : Ext.id(null, 'z-column-'),
            dataIndex : 'z',
            text : 'Z (cm)',
            flex : 2
        });
    }
});
