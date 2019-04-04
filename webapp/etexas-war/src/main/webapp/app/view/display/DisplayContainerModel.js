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
 * Creates the THREEDISPLAY name space if it doesn't exist yet. NOTE: this is only here to fix the
 * JSlint error; THREEDISPLAY should never come in empty (or it's a bug).
 */
var THREEDISPLAY = THREEDISPLAY || {};
/**
 * @class ETexas.view.display.DisplayContainerModel
 * @extends Ext.app.ViewModel
 * 
 * The {@link ETexas.view.display.DisplayContainer} model.
 * 
 * @author ttevendale
 */
Ext.define('ETexas.view.display.DisplayContainerModel', {
    extend : 'Ext.app.ViewModel',
    alias : 'viewmodel.displaycontainer',

    requires : [ 'ETexas.store.DeviceProfileStore' ],

    data : {

        /**
         * @private
         * @property {THREEDISPLAY.Main} _ThreeDisplay The instance of ThreeJS in this container.
         */
        _ThreeDisplay : new THREEDISPLAY.Main(),

        /**
         * @private
         * @property {Object} _infoBoxData The current data in the info box.
         */
        _infoBoxData : {}
    },

    formulas : {

        /** @property {String} deviceProfilesUrl The URL for device profile REST services. */
        deviceProfilesUrl : function(get) {

            var selectedComposite = get('selectedComposite');

            return ETexas.util.UrlProvider.getDeviceProfilesUrl(selectedComposite);
        }
    },

    stores : {

        /**
         * @property {ETexas.store.DeviceProfileStore} deviceProfileStore The
         * {@link ETexas.model.DeviceProfileModel} data for the selected composite.
         */
        deviceProfileStore : {
            type : 'deviceprofile',
            proxy : {
                type : 'ajax',
                url : '{deviceProfilesUrl}'
            }
        }
    }
});
