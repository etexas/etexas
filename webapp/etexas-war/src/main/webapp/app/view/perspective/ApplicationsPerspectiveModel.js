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
 * @class ETexas.view.perspective.ApplicationsPerspectiveModel
 * @extends Ext.app.ViewModel
 * 
 * The {@link ETexas.view.perspective.ApplicationsPerspective} model.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.perspective.ApplicationsPerspectiveModel', {
    extend : 'Ext.app.ViewModel',
    alias : 'viewmodel.applicationsperspective',

    data : {

        /**
         * @property {ETexas.model.ApplicationProfileModel} selectedEmbeddedApplicationProfile The
         * selected embedded application profile.
         */
        selectedEmbeddedApplicationProfile : null,

        /**
         * @property {ETexas.model.ApplicationProfileModel} selectedJarApplicationProfile The
         * selected JAR application profile.
         */
        selectedJarApplicationProfile : null,

        /**
         * @property {ETexas.model.ApplicationProfileModel} selectedNativeApplicationProfile The
         * selected native application profile.
         */
        selectedNativeApplicationProfile : null,

        /**
         * @property {ETexas.model.ApplicationProfileModel} selectedRemoteApplicationProfile The
         * selected remote application profile.
         */
        selectedRemoteApplicationProfile : null
    },

    stores : {

        /**
         * @property {ETexas.store.ApplicationProfileStore} embeddedApplicationProfileStore The
         * embedded {@link ETexas.model.ApplicationProfileModel} data for the current user.
         */
        embeddedApplicationProfileStore : {
            source : '{applicationProfileStore}',
            sorters : 'name',
            filters : [ {
                property : 'isEmbedded',
                value : true
            } ]
        },

        /**
         * @property {ETexas.store.ApplicationProfileStore} jarApplicationProfileStore The JAR
         * {@link ETexas.model.ApplicationProfileModel} data for the current user.
         */
        jarApplicationProfileStore : {
            source : '{applicationProfileStore}',
            sorters : 'name',
            groupField : 'fileName',
            filters : [ {
                property : 'type',
                value : 'JAR'
            }, {
                property : 'isEmbedded',
                value : false
            } ]
        },

        /**
         * @property {ETexas.store.ApplicationProfileStore} nativeApplicationProfileStore The native
         * {@link ETexas.model.ApplicationProfileModel} data for the current user.
         */
        nativeApplicationProfileStore : {
            source : '{applicationProfileStore}',
            sorters : 'name',
            filters : [ {
                property : 'type',
                value : 'NATIVE'
            } ]
        },

        /**
         * @property {ETexas.store.ApplicationProfileStore} remoteApplicationProfileStore The remote
         * {@link ETexas.model.ApplicationProfileModel} data for the current user.
         */
        remoteApplicationProfileStore : {
            source : '{applicationProfileStore}',
            sorters : 'name',
            filters : [ {
                property : 'type',
                value : 'REMOTE'
            } ]
        }
    }
});
