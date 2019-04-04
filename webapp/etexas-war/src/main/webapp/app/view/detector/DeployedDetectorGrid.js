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
 * @class ETexas.view.detector.DeployedDetectorGrid
 * @extends Ext.grid.Panel
 * 
 * A grid panel to view {@link ETexas.model.DeployedDetectorModel} data.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.detector.DeployedDetectorGrid', {
    extend : 'Ext.grid.Panel',
    xtype : 'deployeddetectorgrid',

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
            dataIndex : 'detectorID',
            text : 'ID',
            flex : 1
        }, {
            xtype : 'templatecolumn',
            reference : 'lanesColumn',
            id : Ext.id(null, 'lanes-column-'),
            tpl : new Ext.XTemplate('<tpl for="laneIDs" between=",">', '{.}', '</tpl>'),
            text : 'Lanes',
            flex : 2
        }, {
            xtype : 'templatecolumn',
            reference : 'capabilitiesColumn',
            id : Ext.id(null, 'capabilities-column-'),
            tpl : new Ext.XTemplate(this._buildCapabilitiesTemplate()),
            text : 'Capabilities',
            flex : 3
        }, {
            xtype : 'templatecolumn',
            reference : 'boundaryColumn',
            id : Ext.id(null, 'boundary-column-'),
            tpl : new Ext.XTemplate('<tpl for="area.x">', '{#}: ({.}, {[parent.area.y[xindex-1]]})<br/>', '</tpl>'),
            text : 'Boundary Points (x, y) (cm)',
            flex : 3
        } ];
    },

    /**
     * @method _buildCapabilitiesTemplate
     * 
     * Builds the XTemplate text for the capabilities column.
     * 
     * @private
     * @return {String[]} The XTemplate text for the capabilities column.
     */
    _buildCapabilitiesTemplate : function() {

        var template = [ '<ul class="condensed">' ];
        template.push('<tpl if="lengthDetectCap == true">', '<li>Length Detection</li>', '</tpl>');
        template.push('<tpl if="presenceDetectCap == true">', '<li>Presence Detection</li>', '</tpl>');
        template.push('<tpl if="pulseDetectCap == true">', '<li>Pulse Detection</li>', '</tpl>');
        template.push('<tpl if="speedDetectCap == true">', '<li>Speed Detection</li>', '</tpl>');
        template.push('</ul>');

        return template;
    }
});
