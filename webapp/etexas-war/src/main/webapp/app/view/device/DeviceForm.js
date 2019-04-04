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
 * @class ETexas.view.device.DeviceForm
 * @extends ETexas.view.model.ModelForm
 * 
 * An abstract form panel for {@link ETexas.model.DeviceModel} operations.
 * 
 * @abstract
 * @author emyers
 */
Ext.define('ETexas.view.device.DeviceForm', {
    extend : 'ETexas.view.model.ModelForm',

    /** @inheritdoc */
    initComponent : function() {

        this.items = this.buildItems();
        this.callParent();
    },

    /**
     * @method buildItems
     * 
     * Builds the items for this form panel.
     * 
     * @template
     * @protected
     * @return {Object[]} The items for this form panel.
     */
    buildItems : function() {

        return [ {
            xtype : 'textfield',
            reference : 'nameField',
            id : Ext.id(null, 'name-field-'),
            name : 'deviceName',
            fieldLabel : 'Name',
            minWidth : 350
        }, {
            xtype : 'numberfield',
            reference : 'xField',
            id : Ext.id(null, 'x-field-'),
            name : 'x',
            fieldLabel : 'X (cm)',
            allowBlank : false,
            blankText : 'A valid x coordinate is required.',
            allowExponential : false
        }, {
            xtype : 'numberfield',
            reference : 'yField',
            id : Ext.id(null, 'y-field-'),
            name : 'y',
            fieldLabel : 'Y (cm)',
            allowBlank : false,
            blankText : 'A valid y coordinate is required.',
            allowExponential : false
        }, {
            xtype : 'numberfield',
            reference : 'zField',
            id : Ext.id(null, 'z-field-'),
            name : 'z',
            fieldLabel : 'Z (cm)',
            allowBlank : false,
            blankText : 'A valid z coordinate is required.',
            allowExponential : false
        } ];
    }
});
