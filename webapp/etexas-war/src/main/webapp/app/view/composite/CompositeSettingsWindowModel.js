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
 * @class ETexas.view.composite.CompositeSettingsWindowModel
 * @extends Ext.app.ViewModel
 * 
 * The {@link ETexas.view.composite.CompositeSettingsWindow} model.
 * 
 * @author emyers
 * @author ttevendale
 */
Ext.define('ETexas.view.composite.CompositeSettingsWindowModel', {
    extend : 'Ext.app.ViewModel',
    alias : 'viewmodel.compositesettingswindow',

    requires : [ 'ETexas.store.CellTowerStore', 'ETexas.store.DeviceStore', 'ETexas.store.DeviceProfileStore', 'ETexas.store.LaneMappingStore', 'ETexas.store.TopographyFeatureStore' ],

    data : {

        /** @property {Number} latitude The latitude (DD) of the selected composite. */
        latitude : null,

        /** @property {Number} longitude The longitude (DD) of the selected composite. */
        longitude : null,

        /**
         * @property {String} geographicCalculator The geographic calculator for the selected
         * composite.
         */
        geographicCalculator : null,

        /**
         * @property {String} propagationLossModel The propagation loss model for the selected
         * composite.
         */
        propagationLossModel : null,

        /**
         * @property {String} communicationsModel The communications model for the selected
         * composite.
         */
        communicationsModel : null,

        /**
         * @property {ETexas.model.TopographyFeatureModel} selectedBuilding The selected building.
         */
        selectedBuilding : null,

        /** @property {ETexas.model.CellTowerModel} selectedCellTower The selected cell tower. */
        selectedCellTower : null,

        /**
         * @property {ETexas.model.DeviceProfileModel} selectedCellularDeviceProfile The selected
         * cellular device profile.
         */
        selectedCellularDeviceProfile : null,

        /**
         * @property {ETexas.model.DeviceModel} selectedFixedCellularDevice The selected fixed
         * cellular device.
         */
        selectedFixedCellularDevice : null,

        /** @property {ETexas.model.LaneMappingModel} selectedLaneMapping The selected lane mapping. */
        selectedLaneMapping : null,

        /**
         * @property {ETexas.model.DeviceProfileModel} selectedObuDeviceProfile The selected OBU
         * device profile.
         */
        selectedObuDeviceProfile : null,

        /** @property {ETexas.model.DeviceModel} selectedRseDevice The selected RSE device. */
        selectedRseDevice : null
    },

    formulas : {

        /** @property {String} cellTowersUrl The URL for cell tower REST services. */
        cellTowersUrl : function(get) {

            return ETexas.util.UrlProvider.getCellTowersUrl(get('selectedComposite'));
        },

        /** @property {String} devicesUrl The URL for device REST services. */
        devicesUrl : function(get) {

            return ETexas.util.UrlProvider.getDevicesUrl(get('selectedComposite'));
        },

        /** @property {String} deviceProfilesUrl The URL for device profile REST services. */
        deviceProfilesUrl : function(get) {

            return ETexas.util.UrlProvider.getDeviceProfilesUrl(get('selectedComposite'));
        },

        /** @property {String} laneMappingsUrl The URL for lane mapping REST services. */
        laneMappingsUrl : function(get) {

            return ETexas.util.UrlProvider.getLaneMappingsUrl(get('selectedComposite'));
        },

        /** @property {String} topographyFeaturesUrl The URL for topography feature REST services. */
        topographyFeaturesUrl : function(get) {

            return ETexas.util.UrlProvider.getTopographyFeaturesUrl(get('selectedComposite'));
        }
    },

    stores : {

        /**
         * @property {ETexas.storeTopographyFeatureStore} buildingStore The building
         * {@link ETexas.model.TopographyFeatureModel} data for the selected composite.
         */
        buildingStore : {
            source : '{topographyFeatureStore}',
            sorters : 'name',
            filters : [ {
                property : 'type',
                value : 'BUILDING'
            } ]
        },

        /** @property {Ext.data.Store} calculatorStore The valid geographic calculator values. */
        calculatorStore : {

            fields : [ {
                name : 'calculator',
                type : 'string'
            } ],

            data : [ {
                calculator : 'Cartesian'
            }, {
                calculator : 'Geodetic 2D'
            }, {
                calculator : 'Geodetic 3D'
            }, {
                calculator : 'Spherical'
            } ]
        },

        /**
         * @property {ETexas.store.CellTowerStore} cellTowerStore The
         * {@link ETexas.model.CellTowerModel} data for the selected simulation.
         */
        cellTowerStore : {
            type : 'celltower',
            sorters : 'provider',
            autoLoad : true,
            proxy : {
                type : 'ajax',
                url : '{cellTowersUrl}'
            }
        },

        /**
         * @property {ETexas.store.DeviceProfileStore} cellularDeviceProfileStore The cellular
         * {@link ETexas.model.DeviceProfileModel} data for the selected composite.
         */
        cellularDeviceProfileStore : {
            source : '{deviceProfileStore}',
            sorters : 'name',
            filters : [ {
                property : 'type',
                value : 'CELLULAR'
            } ]
        },

        /** @property {Ext.data.Store} communicationsStore The valid communications model values. */
        communicationsStore : {

            fields : [ {
                name : 'communicationsModel',
                type : 'string'
            } ],

            data : [ {
                communicationsModel : 'Idealized'
            }, {
                communicationsModel : 'NS3'
            } ]
        },

        /**
         * @property {ETexas.store.DeviceStore} deviceStore The {@link ETexas.model.DeviceModel}
         * data for the selected composite.
         */
        deviceStore : {
            type : 'device',
            autoLoad : true,
            proxy : {
                type : 'ajax',
                url : '{devicesUrl}'
            }
        },

        /**
         * @property {ETexas.store.DeviceProfileStore} deviceProfileStore The
         * {@link ETexas.model.DeviceProfileModel} data for the selected composite.
         */
        deviceProfileStore : {
            type : 'deviceprofile',
            autoLoad : true,
            proxy : {
                type : 'ajax',
                url : '{deviceProfilesUrl}'
            }
        },

        /**
         * @property {ETexas.store.DeviceStore} fixedCellularDeviceStore The fixed cellular
         * {@link ETexas.model.DeviceModel} data for the selected simulation.
         */
        fixedCellularDeviceStore : {
            source : '{deviceStore}',
            sorters : 'name',
            filters : [ {
                property : 'type',
                value : 'FIXED_CELLULAR'
            } ]
        },

        /**
         * @property {ETexas.store.LaneMappingStore} laneMappingStore The
         * {@link ETexas.model.LaneMappingModel} data for the selected composite.
         */
        laneMappingStore : {
            type : 'lanemapping',
            proxy : {
                type : 'ajax',
                url : '{laneMappingsUrl}'
            }
        },

        /**
         * @property {ETexas.store.DeviceProfileStore} obuDeviceProfileStore The OBU
         * {@link ETexas.model.DeviceProfileModel} data for the selected composite.
         */
        obuDeviceProfileStore : {
            source : '{deviceProfileStore}',
            sorters : 'name',
            filters : [ {
                property : 'type',
                value : 'OBU'
            } ]
        },

        /**
         * @property {Ext.data.Store} propagationLossModelStore The valid propagation loss model
         * values.
         */
        propagationLossModelStore : {

            fields : [ {
                name : 'propagationLossModel',
                type : 'string'
            } ],

            data : [ {
                propagationLossModel : 'Open'
            }, {
                propagationLossModel : 'Suburban'
            }, {
                propagationLossModel : 'Urban'
            } ]
        },

        /**
         * @property {ETexas.store.DeviceStore} rseDeviceStore The RSE
         * {@link ETexas.model.DeviceModel} data for the selected simulation.
         */
        rseDeviceStore : {
            source : '{deviceStore}',
            sorters : 'name',
            filters : [ {
                property : 'type',
                value : 'RSE'
            } ]
        },

        /**
         * @property {ETexas.store.TopographyFeatureStore} topographyFeatureStore The
         * {@link ETexas.model.TopographyFeatureModel} data for the selected composite.
         */
        topographyFeatureStore : {
            type : 'topographyfeature',
            autoLoad : true,
            proxy : {
                type : 'ajax',
                url : '{topographyFeaturesUrl}'
            }
        }
    }
});
