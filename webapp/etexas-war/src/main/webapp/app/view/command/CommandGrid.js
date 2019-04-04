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
 * @class ETexas.view.command.CommandGrid
 * @extends Ext.grid.Panel
 * 
 * A grid panel to view {@link ETexas.model.CommandModel} data.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.command.CommandGrid', {
    extend : 'Ext.grid.Panel',
    xtype : 'commandgrid',

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
            reference : 'timeColumn',
            id : Ext.id(null, 'time-column-'),
            dataIndex : 'submissionTime',
            text : 'Time (s)',
            flex : 1
        }, {
            reference : 'sourceColumn',
            id : Ext.id(null, 'source-column-'),
            dataIndex : 'source',
            text : 'Source',
            flex : 2
        }, {
            reference : 'descriptionColumn',
            id : Ext.id(null, 'description-column-'),
            dataIndex : 'description',
            text : 'Description',
            cellWrap : true,
            flex : 4
        } ];
    }
});
