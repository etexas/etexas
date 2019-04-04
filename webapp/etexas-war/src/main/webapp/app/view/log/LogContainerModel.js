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
 * @class ETexas.view.log.LogContainerModel
 * @extends Ext.app.ViewModel
 * 
 * The {@link ETexas.view.log.LogContainer} model.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.log.LogContainerModel', {
    extend : 'Ext.app.ViewModel',
    alias : 'viewmodel.logcontainer',

    requires : [ 'ETexas.store.LogStore' ],

    data : {

        /**
         * @private
         * @property {Object} _parameters The parameters that were last searched for.
         */
        _parameters : {}
    },

    formulas : {

        /** @property {String} logsUrl The URL for log REST services. */
        logsUrl : function(get) {

            var selectedComposite = get('selectedComposite');
            var selectedExecution = get('selectedExecution');
            return selectedExecution ? ETexas.util.UrlProvider.getLogsUrl(selectedComposite, selectedExecution) : null;
        }
    },

    stores : {

        /**
         * @property {ETexas.store.LogStore} logStore The {@link ETexas.model.LogModel} data for the
         * selected execution.
         */
        logStore : {
            type : 'log',
            sorters : 'simulationTime',
            proxy : {
                type : 'ajax',
                url : '{logsUrl}',
                reader : {
                    type : 'json',
                    rootProperty : 'logs',
                    totalProperty : 'total'
                }
            }
        },

        /**
         * @property {Ext.data.Store} hostFilterStore The host devices available to filter the
         * {@link ETexas.model.LogModel} data for the selected execution.
         */
        hostFilterStore : {
            fields : [ {
                name : 'id',
                type : 'string'
            }, {
                name : 'value',
                type : 'int'
            } ],
            sorters : 'value'
        },

        /**
         * @property {Ext.data.Store} applicationFilterStore The applications available to filter
         * the {@link ETexas.model.LogModel} data for the selected execution.
         */
        applicationFilterStore : {
            fields : [ {
                name : 'id',
                type : 'string'
            }, {
                name : 'value',
                type : 'string'
            } ],
            sorters : 'value'
        },

        /**
         * @property {Ext.data.Store} keyFilterStore The keys available to filter the
         * {@link ETexas.model.LogModel} data for the selected execution.
         */
        keyFilterStore : {
            fields : [ {
                name : 'id',
                type : 'string'
            }, {
                name : 'value',
                type : 'string'
            } ],
            sorters : 'value'
        }
    }
});
