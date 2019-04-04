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
 * @class ETexas.view.topographyfeature.BuildingForm
 * @extends ETexas.view.topographyfeature.TopographyFeatureForm
 * 
 * An abstract form panel for building {@link ETexas.model.TopographyFeatureModel} operations.
 * 
 * @abstract
 * @author ttevendale
 */
Ext.define('ETexas.view.topographyfeature.BuildingForm', {
    extend : 'ETexas.view.topographyfeature.TopographyFeatureForm',

    /** @inheritdoc */
    buildItems : function() {

        return Ext.Array.push(this.callParent(), [ {
            xtype : 'numberfield',
            reference : 'widthField',
            id : Ext.id(null, 'width-field-'),
            name : 'width',
            fieldLabel : 'Width (cm)',
            allowBlank : false,
            blankText : 'A valid width is required.',
            allowExponential : false,
            minValue : 1,
            minText : 'Building widths must be greater than or equal to 1 cm.'
        }, {
            xtype : 'numberfield',
            reference : 'lengthField',
            id : Ext.id(null, 'length-field-'),
            name : 'length',
            fieldLabel : 'Length (cm)',
            allowBlank : false,
            blankText : 'A valid length is required.',
            allowExponential : false,
            minValue : 1,
            minText : 'Building lengths must be greater than or equal to 1 cm.'
        }, {
            xtype : 'numberfield',
            reference : 'heightField',
            id : Ext.id(null, 'height-field-'),
            name : 'height',
            fieldLabel : 'Height (cm)',
            allowBlank : false,
            blankText : 'A valid height is required.',
            allowExponential : false,
            minValue : 1,
            minText : 'Building heights must be greater than or equal to 1 cm.'
        } ]);
    }
});
