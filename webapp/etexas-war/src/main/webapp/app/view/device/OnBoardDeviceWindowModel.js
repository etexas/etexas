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
 * @class ETexas.view.device.OnBoardDeviceWindowModel
 * @extends Ext.app.ViewModel
 * 
 * The {@link ETexas.view.device.OnBoardDeviceWindow} model.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.device.OnBoardDeviceWindowModel', {
    extend : 'Ext.app.ViewModel',
    alias : 'viewmodel.onboarddevicewindow',

    requires : [ 'ETexas.store.OnBoardDeviceStore' ],

    data : {

        /** @property {ETexas.model.VehicleModel} vehicle The selected vehicle. */
        vehicle : null
    },

    formulas : {

        /** @property {String} onBoardDevicesUrl The URL for on board device REST services. */
        onBoardDevicesUrl : function(get) {

            var selectedComposite = get('selectedComposite');
            var selectedExecution = get('selectedExecution');
            var vehicle = get('vehicle');
            return ETexas.util.UrlProvider.getOnBoardDevicesUrl(selectedComposite, selectedExecution, vehicle);
        }
    },

    stores : {

        /**
         * @property {ETexas.store.OnBoardDeviceStore} onBoardDeviceStore The
         * {@link ETexas.model.OnBoardDeviceModel} data for the selected vehicle.
         */
        onBoardDeviceStore : {
            type : 'onboarddevice',
            autoLoad : true,
            proxy : {
                type : 'ajax',
                url : '{onBoardDevicesUrl}'
            }
        }
    }
});
