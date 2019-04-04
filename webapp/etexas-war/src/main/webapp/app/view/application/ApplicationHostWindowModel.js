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
 * @class ETexas.view.application.ApplicationHostWindowModel
 * @extends Ext.app.ViewModel
 * 
 * The {@link ETexas.view.application.ApplicationHostWindow} model.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.application.ApplicationHostWindowModel', {
    extend : 'Ext.app.ViewModel',
    alias : 'viewmodel.applicationhostwindow',

    requires : [ 'ETexas.store.ApplicationStore' ],

    data : {

        /** @property {String} applicationsUrl The URL for application REST services. */
        applicationsUrl : null,

        /** @property {String} hostType The target application device type. */
        hostType : null,

        /**
         * @property {ETexas.model.ApplicationModel} selectedApplication The selected (hosted)
         * application.
         */
        selectedApplication : null,

        /**
         * @property {ETexas.model.ApplicationProfileModel} selectedApplicationProfile The selected
         * (available) application profile.
         */
        selectedApplicationProfile : null,

        /**
         * @property {ETexas.model.ApplicationParameterModel} selectedParameter The selected
         * application parameter.
         */
        selectedParameter : null
    },

    formulas : {

        /**
         * @property {ETexas.store.ApplicationParameterStore} parameterStore The
         * {@link ETexas.model.ApplicationParameterModel} data for the selected application.
         */
        parameterStore : function(get) {

            var selectedApplication = get('selectedApplication');
            return selectedApplication ? selectedApplication.getParameters() : null;
        },

        /**
         * @property {String} selectedApplicationUrl The URL for application REST services when a
         * single application is selected.
         */
        selectedApplicationUrl : function(get) {

            var selectedApplication = get('selectedApplication');
            return selectedApplication ? get('applicationsUrl') + '/' + selectedApplication.get('id') : null;
        },

        /**
         * @property {String} selectedParameterUrl The URL for application parameter REST services
         * when a single application parameter is selected.
         */
        selectedParameterUrl : function(get) {

            var selectedApplicationUrl = get('selectedApplicationUrl');

            if (selectedApplicationUrl) {

                var selectedParameter = get('selectedParameter');
                return selectedParameter ? selectedApplicationUrl + '/parameters/' + selectedParameter.get('id') : null;
            }

            return null;
        }
    },

    stores : {

        /**
         * @property {ETexas.store.ApplicationProfileStore} availableStore The available
         * {@link ETexas.model.ApplicationProfileModel} data for the selected application host.
         */
        availableStore : {
            source : '{applicationProfileStore}',
            sorters : 'name'
        },

        /**
         * @property {ETexas.store.ApplicationStore} hostedStore The hosted
         * {@link ETexas.model.ApplicationModel} data for the selected application host.
         */
        hostedStore : {
            type : 'application',
            sorters : 'name',
            autoLoad : true,
            proxy : {
                type : 'ajax',
                url : '{applicationsUrl}'
            },
            listeners : {
                datachanged : 'onHostedDataChanged'
            }
        }
    }
});
