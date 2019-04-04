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
 * @class ETexas.view.command.InjectionCommandForm
 * @extends ETexas.view.command.CommandForm
 * 
 * A form panel to create a new injection {@link ETexas.model.CommandModel}.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.command.InjectionCommandForm', {
    extend : 'ETexas.view.command.CommandForm',
    xtype : 'injectioncommandform',

    requires : [ 'ETexas.view.command.InjectionCommandFormController', 'ETexas.view.command.InjectionCommandFormModel' ],

    controller : 'injectioncommandform',
    viewModel : {
        type : 'injectioncommandform'
    },

    title : 'Create Injection Command',

    /** @inheritdoc */
    buildItems : function() {

        var speedMsg = 'The speed of injected vehicles must be in the range of 0.4 m/s to 49 m/s.';

        return Ext.Array.push(this.callParent(), {
            xtype : 'combobox',
            reference : 'laneIdBox',
            id : Ext.id(null, 'lane-id-box-'),
            name : 'laneId',
            fieldLabel : 'Lane',
            displayField : 'laneId',
            queryMode : 'local',
            editable : false,
            bind : {
                store : '{simulationLaneStore}'
            },
            allowBlank : false,
            blankText : 'A selected lane is required.'
        }, {
            xtype : 'numberfield',
            reference : 'speedField',
            id : Ext.id(null, 'speed-field-'),
            name : 'speed',
            fieldLabel : 'Speed (m/s)',
            allowBlank : false,
            blankText : 'A valid speed is required.',
            minValue : 0.4,
            minText : speedMsg,
            maxValue : 49,
            maxText : speedMsg
        });
    },

    /** @inheritdoc */
    buildTools : function() {

        return Ext.Array.insert(this.callParent(), 0, [ {
            type : 'help',
            reference : 'helpTool',
            id : Ext.id(null, 'help-tool-'),
            callback : 'onHelpToolClicked',
            tooltip : 'Create Injection Command Help'
        } ]);
    }
});
