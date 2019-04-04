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
 * @class ETexas.view.command.SignalCommandForm
 * @extends ETexas.view.command.CommandForm
 * 
 * A form panel to create a new signal {@link ETexas.model.CommandModel}.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.command.SignalCommandForm', {
    extend : 'ETexas.view.command.CommandForm',
    xtype : 'signalcommandform',

    requires : [ 'ETexas.view.command.SignalCommandFormController', 'ETexas.view.command.SignalCommandFormModel' ],

    controller : 'signalcommandform',
    viewModel : {
        type : 'signalcommandform'
    },

    title : 'Create Signal Command',

    /** @inheritdoc */
    buildItems : function() {

        return Ext.Array.push(this.callParent(), {
            xtype : 'combobox',
            reference : 'commandBox',
            id : Ext.id(null, 'command-box-'),
            name : 'command',
            fieldLabel : 'Command',
            displayField : 'description',
            valueField : 'command',
            queryMode : 'local',
            editable : false,
            bind : {
                store : '{commandStore}'
            },
            allowBlank : false,
            blankText : 'A selected signal command is required.'
        }, {
            xtype : 'numberfield',
            reference : 'timeField',
            id : Ext.id(null, 'time-field-'),
            name : 'time',
            fieldLabel : 'Time (s)',
            allowBlank : false,
            blankText : 'A valid time is required.',
            minValue : 0.1,
            minText : 'The minimum time must be at least 0.1 s.'
        });
    },

    /** @inheritdoc */
    buildTools : function() {

        return Ext.Array.insert(this.callParent(), 0, [ {
            type : 'help',
            reference : 'helpTool',
            id : Ext.id(null, 'help-tool-'),
            callback : 'onHelpToolClicked',
            tooltip : 'Create Signal Command Help'
        } ]);
    }
});
