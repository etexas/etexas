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
 * @class ETexas.view.applicationprofile.NativeApplicationProfileForm
 * @extends ETexas.view.applicationprofile.ApplicationProfileForm
 * 
 * An abstract form panel for native {@link ETexas.model.ApplicationProfileModel} operations.
 * 
 * @abstract
 * @author emyers
 */
Ext.define('ETexas.view.applicationprofile.NativeApplicationProfileForm', {
    extend : 'ETexas.view.applicationprofile.ApplicationProfileForm',

    /** @inheritdoc */
    buildItems : function() {

        var portNumberMsg = 'Port number values must be in the range of 1 to 65,535.';

        return Ext.Array.insert(this.callParent(), 1, [ {
            xtype : 'textfield',
            reference : 'commandLineField',
            id : Ext.id(null, 'command-line-field-'),
            name : 'commandLine',
            fieldLabel : 'Command Line',
            allowOnlyWhitespace : false,
            blankText : 'A valid command line is required.'
        }, {
            xtype : 'textfield',
            reference : 'hostAddressField',
            id : Ext.id(null, 'host-address-field-'),
            name : 'hostAddress',
            fieldLabel : 'Host Address'
        }, {
            xtype : 'numberfield',
            reference : 'portNumberField',
            id : Ext.id(null, 'port-number-field-'),
            name : 'portNumber',
            fieldLabel : 'Port Number',
            allowBlank : false,
            blankText : 'A valid port number is required.',
            allowDecimal : false,
            allowExponential : false,
            minValue : 1,
            maxValue : 65535,
            minText : portNumberMsg,
            maxText : portNumberMsg
        } ]);
    }
});
