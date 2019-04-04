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
 * @class ETexas.view.composite.CompositeSettingsWindowController
 * @extends ETexas.view.window.BasicWindowController
 * 
 * The {@link ETexas.view.composite.CompositeSettingsWindow} controller.
 * 
 * @author emyers
 * @author ttevendale
 */
Ext.define('ETexas.view.composite.CompositeSettingsWindowController', {
    extend : 'ETexas.view.window.BasicWindowController',
    alias : 'controller.compositesettingswindow',

    requires : [ 'ETexas.view.application.ApplicationHostWindow', 'ETexas.view.celltower.CreateCellTowerForm', 'ETexas.view.celltower.EditCellTowerForm',
            'ETexas.view.cellularconfiguration.EditCellularConfigurationForm', 'ETexas.view.composite.EditCompositeOptionsForm', 'ETexas.view.device.CreateFixedCellularDeviceForm',
            'ETexas.view.device.CreateRseDeviceForm', 'ETexas.view.device.EditFixedCellularDeviceForm', 'ETexas.view.device.EditRseDeviceForm',
            'ETexas.view.deviceprofile.CreateCellularDeviceProfileForm', 'ETexas.view.deviceprofile.CreateObuDeviceProfileForm', 'ETexas.view.deviceprofile.EditCellularDeviceProfileForm',
            'ETexas.view.deviceprofile.EditObuDeviceProfileForm', 'ETexas.view.lanemapping.CreateLaneMappingForm', 'ETexas.view.lanemapping.EditLaneMappingForm',
            'ETexas.view.topographyfeature.CreateBuildingForm', 'ETexas.view.topographyfeature.EditBuildingForm', 'ETexas.view.window.BasicWindow' ],

    /** @inheritdoc */
    init : function(view) {

        var viewModel = this.getViewModel();
        var buildingToolbar = view.lookupReference('buildingToolbar');
        var cellTowerToolbar = view.lookupReference('cellTowerToolbar');
        var cellularDeviceProfileToolbar = view.lookupReference('cellularDeviceProfileToolbar');
        var fixedCellularDeviceToolbar = view.lookupReference('fixedCellularDeviceToolbar');
        var laneMappingToolbar = view.lookupReference('laneMappingToolbar');
        var obuDeviceProfileToolbar = view.lookupReference('obuDeviceProfileToolbar');
        var rseDeviceToolbar = view.lookupReference('rseDeviceToolbar');

        buildingToolbar.lookupReference('deleteButton').setDisabled(true);
        cellTowerToolbar.lookupReference('deleteButton').setDisabled(true);
        cellularDeviceProfileToolbar.lookupReference('deleteButton').setDisabled(true);
        fixedCellularDeviceToolbar.lookupReference('deleteButton').setDisabled(true);
        laneMappingToolbar.lookupReference('deleteButton').setDisabled(true);
        obuDeviceProfileToolbar.lookupReference('deleteButton').setDisabled(true);
        rseDeviceToolbar.lookupReference('deleteButton').setDisabled(true);

        viewModel.bind('{selectedBuilding}', function(selectedBuilding) {
            buildingToolbar.lookupReference('editButton').setDisabled(!selectedBuilding);
        });

        viewModel.bind('{selectedCellTower}', function(selectedCellTower) {
            cellTowerToolbar.lookupReference('editButton').setDisabled(!selectedCellTower);
        });

        viewModel.bind('{selectedCellularDeviceProfile}', function(selectedCellularDeviceProfile) {
            cellularDeviceProfileToolbar.lookupReference('editButton').setDisabled(!selectedCellularDeviceProfile);
            cellularDeviceProfileToolbar.lookupReference('applicationsButton').setDisabled(!selectedCellularDeviceProfile);
        });

        viewModel.bind('{selectedFixedCellularDevice}', function(selectedFixedCellularDevice) {
            fixedCellularDeviceToolbar.lookupReference('editButton').setDisabled(!selectedFixedCellularDevice);
            fixedCellularDeviceToolbar.lookupReference('applicationsButton').setDisabled(!selectedFixedCellularDevice);
        });

        viewModel.bind('{selectedLaneMapping}', function(selectedLaneMapping) {
            laneMappingToolbar.lookupReference('editButton').setDisabled(!selectedLaneMapping);
        });

        viewModel.bind('{selectedObuDeviceProfile}', function(selectedObuDeviceProfile) {
            obuDeviceProfileToolbar.lookupReference('editButton').setDisabled(!selectedObuDeviceProfile);
            obuDeviceProfileToolbar.lookupReference('applicationsButton').setDisabled(!selectedObuDeviceProfile);
        });

        var optionsPanel = view.lookupReference('compositeOptionsPanel');
        optionsPanel.lookupReference('latitudeField').setBind('{latitude}');
        optionsPanel.lookupReference('longitudeField').setBind('{longitude}');
        optionsPanel.lookupReference('geographicCalculatorField').setBind('{geographicCalculator}');
        optionsPanel.lookupReference('propagationLossModelField').setBind('{propagationLossModel}');
        optionsPanel.lookupReference('communicationsModelField').setBind('{communicationsModel}');

        viewModel.bind('{selectedRseDevice}', function(selectedRseDevice) {
            rseDeviceToolbar.lookupReference('editButton').setDisabled(!selectedRseDevice);
            rseDeviceToolbar.lookupReference('applicationsButton').setDisabled(!selectedRseDevice);
        });

        this._updateOptions();
        viewModel.notify();
        this._updateLaneMappingStore();
        this.callParent([ view ]);
    },

    /**
     * @method deleteBuildings
     * 
     * Deletes the selected building {@link ETexas.model.TopographyFeatureModel}s.
     * 
     * @protected
     */
    deleteBuildings : function() {

        var me = this;
        var view = this.getView();
        var selectedComposite = this.getViewModel().get('selectedComposite');
        var selectedBuildings = view.lookupReference('buildingGrid').getSelection();

        var mask;
        var message;
        var messageTitle;

        if (selectedBuildings.length === 1) {

            mask = 'Deleting Building...';
            message = 'Deleting \"' + selectedBuildings[0].get('name') + '\" will result in the loss of all data associated with the building. Are you sure you want to delete it?';
            messageTitle = 'Delete Building';
        }
        else {

            mask = 'Deleting Buildings...';
            message = 'Deleting the selected buildings will result in the loss of all data associated with the buildings. Are you sure you want to delete them?';
            messageTitle = 'Delete Buildings';
        }

        Ext.Msg.confirm(messageTitle, message, function(btn) {

            if (btn === 'yes') {

                var buildingIds = [];
                selectedBuildings.forEach(function(value, index, array) {
                    buildingIds.push(value.get('id'));
                });

                var queryParams = Ext.Object.toQueryString({
                    featureIds : buildingIds
                });

                view.mask(mask);
                Ext.Ajax.request({
                    scope : this,
                    method : 'DELETE',
                    url : ETexas.util.UrlProvider.getTopographyFeaturesUrl(selectedComposite) + '?' + queryParams,
                    success : function(response, opts) {

                        view.unmask();
                        me._updateBuildingStore();
                    },
                    failure : function(response, opts) {

                        view.unmask();
                    }
                });
            }
        });
    },

    /**
     * @method deleteCellTowers
     * 
     * Deletes the selected {@link ETexas.model.CellTowerModel}s.
     * 
     * @protected
     */
    deleteCellTowers : function() {

        var me = this;
        var view = this.getView();
        var selectedComposite = this.getViewModel().get('selectedComposite');
        var selectedCellTowers = view.lookupReference('cellTowerGrid').getSelection();

        var mask;
        var message;
        var messageTitle;

        if (selectedCellTowers.length === 1) {

            mask = 'Deleting Cell Tower...';
            message = 'Deleting cell tower \"' + selectedCellTowers[0].get('id') + '\" will result in the loss of all data associated with the cell tower. Are you sure you want to delete it?';
            messageTitle = 'Delete Cell Tower';
        }
        else {

            mask = 'Deleting Cell Towers...';
            message = 'Deleting the selected cell towers will result in the loss of all data associated with the cell towers. Are you sure you want to delete them?';
            messageTitle = 'Delete Cell Towers';
        }

        Ext.Msg.confirm(messageTitle, message, function(btn) {

            if (btn === 'yes') {

                var cellTowerIds = [];
                selectedCellTowers.forEach(function(value, index, array) {
                    cellTowerIds.push(value.get('id'));
                });

                var queryParams = Ext.Object.toQueryString({
                    cellTowerIds : cellTowerIds
                });

                view.mask(mask);
                Ext.Ajax.request({
                    scope : this,
                    method : 'DELETE',
                    url : ETexas.util.UrlProvider.getCellTowersUrl(selectedComposite) + '?' + queryParams,
                    success : function(response, opts) {

                        view.unmask();
                        me._updateCellTowerStore();
                    },
                    failure : function(response, opts) {

                        view.unmask();
                    }
                });
            }
        });
    },

    /**
     * @method deleteCellularDeviceProfiles
     * 
     * Deletes the selected cellular {@link ETexas.model.DeviceProfileModel}s.
     * 
     * @protected
     */
    deleteCellularDeviceProfiles : function() {

        var me = this;
        var view = this.getView();
        var selectedComposite = this.getViewModel().get('selectedComposite');
        var selectedCellularDeviceProfiles = view.lookupReference('cellularDeviceProfileGrid').getSelection();

        var mask;
        var message;
        var messageTitle;

        if (selectedCellularDeviceProfiles.length === 1) {

            mask = 'Deleting Cellular Device Profile...';
            message = 'Deleting \"' + selectedCellularDeviceProfiles[0].get('name') + '\" will result in the loss of all data associated with the cellular device profile. ';
            message = message.concat('Are you sure you want to delete it?');
            messageTitle = 'Delete Cellular Device Profile';
        }
        else {

            mask = 'Deleting Cellular Device Profiles...';
            message = 'Deleting the selected cellular device profiles will result in the loss of all data associated with the cellular device profiles. Are you sure you want to delete them?';
            messageTitle = 'Delete Cellular Device Profiles';
        }

        Ext.Msg.confirm(messageTitle, message, function(btn) {

            if (btn === 'yes') {

                var cellularDeviceProfileIds = [];
                selectedCellularDeviceProfiles.forEach(function(value, index, array) {
                    cellularDeviceProfileIds.push(value.get('id'));
                });

                var queryParams = Ext.Object.toQueryString({
                    deviceProfileIds : cellularDeviceProfileIds
                });

                view.mask(mask);
                Ext.Ajax.request({
                    scope : this,
                    method : 'DELETE',
                    url : ETexas.util.UrlProvider.getDeviceProfilesUrl(selectedComposite) + '?' + queryParams,
                    success : function(response, opts) {

                        view.unmask();
                        me._updateCellularDeviceProfileStore();
                    },
                    failure : function(response, opts) {

                        view.unmask();
                    }
                });
            }
        });
    },

    /**
     * @method deleteFixedCellularDevices
     * 
     * Deletes the selected fixed cellular {@link ETexas.model.DeviceModel}s.
     * 
     * @protected
     */
    deleteFixedCellularDevices : function() {

        var me = this;
        var view = this.getView();
        var selectedComposite = this.getViewModel().get('selectedComposite');
        var selectedFixedCellularDevices = view.lookupReference('fixedCellularDeviceGrid').getSelection();

        var mask;
        var message;
        var messageTitle;

        if (selectedFixedCellularDevices.length === 1) {

            mask = 'Deleting Fixed Cellular Device...';
            message = 'Deleting \"' + selectedFixedCellularDevices[0].get('name') + '\" will result in the loss of all data associated with the fixed cellular device. ';
            message = message.concat('Are you sure you want to delete it?');
            messageTitle = 'Delete Fixed Cellular Device';
        }
        else {

            mask = 'Deleting Fixed Cellular Devices...';
            message = 'Deleting the selected fixed cellular devices will result in the loss of all data associated with the fixed cellular devices. Are you sure you want to delete them?';
            messageTitle = 'Delete Fixed Cellular Devices';
        }

        Ext.Msg.confirm(messageTitle, message, function(btn) {

            if (btn === 'yes') {

                var fixedCellularDeviceIds = [];
                selectedFixedCellularDevices.forEach(function(value, index, array) {
                    fixedCellularDeviceIds.push(value.get('id'));
                });

                var queryParams = Ext.Object.toQueryString({
                    deviceIds : fixedCellularDeviceIds
                });

                view.mask(mask);
                Ext.Ajax.request({
                    scope : this,
                    method : 'DELETE',
                    url : ETexas.util.UrlProvider.getDevicesUrl(selectedComposite) + '?' + queryParams,
                    success : function(response, opts) {

                        view.unmask();
                        me._updateFixedCellularDeviceStore();
                    },
                    failure : function(response, opts) {

                        view.unmask();
                    }
                });
            }
        });
    },

    /**
     * @method deleteLaneMappings
     * 
     * Deletes the selected {@link ETexas.model.LaneMappingModel}s.
     * 
     * @protected
     */
    deleteLaneMappings : function() {

        var me = this;
        var view = this.getView();
        var selectedComposite = this.getViewModel().get('selectedComposite');
        var selectedLaneMappings = view.lookupReference('laneMappingGrid').getSelection();

        var mask;
        var message;
        var messageTitle;

        if (selectedLaneMappings.length === 1) {

            mask = 'Deleting Lane Mapping...';
            message = 'Deleting lane mapping \"' + selectedLaneMappings[0].get('id') + '\" will result in the loss of all data associated with the lane mapping. Are you sure you want to delete it?';
            messageTitle = 'Delete Lane Mapping';
        }
        else {

            mask = 'Deleting Lane Mappings...';
            message = 'Deleting the selected lane mappings will result in the loss of all data associated with the lane mappings. Are you sure you want to delete them?';
            messageTitle = 'Delete Lane Mappings';
        }

        Ext.Msg.confirm(messageTitle, message, function(btn) {

            if (btn === 'yes') {

                var laneMappingIds = [];
                selectedLaneMappings.forEach(function(value, index, array) {
                    laneMappingIds.push(value.get('id'));
                });

                var queryParams = Ext.Object.toQueryString({
                    laneMappingIds : laneMappingIds
                });

                view.mask(mask);
                Ext.Ajax.request({
                    scope : this,
                    method : 'DELETE',
                    url : ETexas.util.UrlProvider.getLaneMappingsUrl(selectedComposite) + '?' + queryParams,
                    success : function(response, opts) {

                        view.unmask();
                        me._updateLaneMappingStore();
                    },
                    failure : function(response, opts) {

                        view.unmask();
                    }
                });
            }
        });
    },

    /**
     * @method deleteObuDeviceProfiles
     * 
     * Deletes the selected OBU {@link ETexas.model.DeviceProfileModel}s.
     * 
     * @protected
     */
    deleteObuDeviceProfiles : function() {

        var me = this;
        var view = this.getView();
        var selectedComposite = this.getViewModel().get('selectedComposite');
        var selectedObuDeviceProfiles = view.lookupReference('obuDeviceProfileGrid').getSelection();

        var mask;
        var message;
        var messageTitle;

        if (selectedObuDeviceProfiles.length === 1) {

            mask = 'Deleting OBU Device Profile...';
            message = 'Deleting \"' + selectedObuDeviceProfiles[0].get('name') + '\" will result in the loss of all data associated with the OBU device profile. Are you sure you want to delete it?';
            messageTitle = 'Delete OBU Device Profile';
        }
        else {

            mask = 'Deleting OBU Device Profiles...';
            message = 'Deleting the selected OBU device profiles will result in the loss of all data associated with the OBU device profiles. Are you sure you want to delete them?';
            messageTitle = 'Delete OBU Device Profiles';
        }

        Ext.Msg.confirm(messageTitle, message, function(btn) {

            if (btn === 'yes') {

                var obuDeviceProfileIds = [];
                selectedObuDeviceProfiles.forEach(function(value, index, array) {
                    obuDeviceProfileIds.push(value.get('id'));
                });

                var queryParams = Ext.Object.toQueryString({
                    deviceProfileIds : obuDeviceProfileIds
                });

                view.mask(mask);
                Ext.Ajax.request({
                    scope : this,
                    method : 'DELETE',
                    url : ETexas.util.UrlProvider.getDeviceProfilesUrl(selectedComposite) + '?' + queryParams,
                    success : function(response, opts) {

                        view.unmask();
                        me._updateObuDeviceProfileStore();
                    },
                    failure : function(response, opts) {

                        view.unmask();
                    }
                });
            }
        });
    },

    /**
     * @method deleteRseDevices
     * 
     * Deletes the selected RSE {@link ETexas.model.DeviceModel}s.
     * 
     * @protected
     */
    deleteRseDevices : function() {

        var me = this;
        var view = this.getView();
        var selectedComposite = this.getViewModel().get('selectedComposite');
        var selectedRseDevices = view.lookupReference('rseDeviceGrid').getSelection();

        var mask;
        var message;
        var messageTitle;

        if (selectedRseDevices.length === 1) {

            mask = 'Deleting RSE Device...';
            message = 'Deleting \"' + selectedRseDevices[0].get('name') + '\" will result in the loss of all data associated with the RSE device. Are you sure you want to delete it?';
            messageTitle = 'Delete RSE Device';
        }
        else {

            mask = 'Deleting RSE Devices...';
            message = 'Deleting the selected RSE devices will result in the loss of all data associated with the RSE devices. Are you sure you want to delete them?';
            messageTitle = 'Delete RSE Devices';
        }

        Ext.Msg.confirm(messageTitle, message, function(btn) {

            if (btn === 'yes') {

                var rseDeviceIds = [];
                selectedRseDevices.forEach(function(value, index, array) {
                    rseDeviceIds.push(value.get('id'));
                });

                var queryParams = Ext.Object.toQueryString({
                    deviceIds : rseDeviceIds
                });

                view.mask(mask);
                Ext.Ajax.request({
                    scope : this,
                    method : 'DELETE',
                    url : ETexas.util.UrlProvider.getDevicesUrl(selectedComposite) + '?' + queryParams,
                    success : function(response, opts) {

                        view.unmask();
                        me._updateRseDeviceStore();
                    },
                    failure : function(response, opts) {

                        view.unmask();
                    }
                });
            }
        });
    },

    /**
     * @method onBuildingSelectionChange
     * 
     * Handles the event that is generated when the user changes the selected building(s).
     * 
     * @protected
     * @param {Ext.selection.Model} model The selection model.
     * @param {Ext.data.Model[]} selected The selected buildings.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onBuildingSelectionChange : function(model, selected, eOpts) {

        this.getViewModel().set('selectedBuilding', selected.length === 1 ? selected[0] : null);
        this.getView().lookupReference('buildingToolbar').lookupReference('deleteButton').setDisabled(selected.length === 0);
    },

    /**
     * @method onCellTowerSelectionChange
     * 
     * Handles the event that is generated when the user changes the selected cell tower(s).
     * 
     * @protected
     * @param {Ext.selection.Model} model The selection model.
     * @param {Ext.data.Model[]} selected The selected cell towers.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onCellTowerSelectionChange : function(model, selected, eOpts) {

        this.getViewModel().set('selectedCellTower', selected.length === 1 ? selected[0] : null);
        this.getView().lookupReference('cellTowerToolbar').lookupReference('deleteButton').setDisabled(selected.length === 0);
    },

    /**
     * @method onCellularDeviceProfileSelectionChange
     * 
     * Handles the event that is generated when the user changes the selected cellular device
     * profile(s).
     * 
     * @protected
     * @param {Ext.selection.Model} model The selection model.
     * @param {Ext.data.Model[]} selected The selected cellular device profiles.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onCellularDeviceProfileSelectionChange : function(model, selected, eOpts) {

        this.getViewModel().set('selectedCellularDeviceProfile', selected.length === 1 ? selected[0] : null);
        this.getView().lookupReference('cellularDeviceProfileToolbar').lookupReference('deleteButton').setDisabled(selected.length === 0);
    },

    /**
     * @method onFixedCellularDeviceSelectionChange
     * 
     * Handles the event that is generated when the user changes the selected fixed cellular
     * device(s).
     * 
     * @protected
     * @param {Ext.selection.Model} model The selection model.
     * @param {Ext.data.Model[]} selected The selected fixed cellular device.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onFixedCellularDeviceSelectionChange : function(model, selected, eOpts) {

        this.getViewModel().set('selectedFixedCellularDevice', selected.length === 1 ? selected[0] : null);
        this.getView().lookupReference('fixedCellularDeviceToolbar').lookupReference('deleteButton').setDisabled(selected.length === 0);
    },

    /**
     * @method onHelpToolClicked
     * 
     * Handles the event that is generated when the user clicks the Composite Settings Help tool.
     * 
     * @protected
     * @param {Ext.panel.Tool} tool The clicked tool.
     * @param {Ext.event.Event} e The click event.
     * @param {Ext.Component} owner The owner of the tool.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onHelpToolClicked : function(tool, e, owner, eOpts) {

        window.open(ETexas.util.UrlProvider.getHelpContentUrl() + '/Simulations/Composites/settings_composite.htm');
    },

    /**
     * @method onLaneMappingSelectionChange
     * 
     * Handles the event that is generated when the user changes the selected lane mapping(s).
     * 
     * @protected
     * @param {Ext.selection.Model} model The selection model.
     * @param {Ext.data.Model[]} selected The selected lane mappings.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onLaneMappingSelectionChange : function(model, selected, eOpts) {

        this.getViewModel().set('selectedLaneMapping', selected.length === 1 ? selected[0] : null);
        this.getView().lookupReference('laneMappingToolbar').lookupReference('deleteButton').setDisabled(selected.length === 0);
    },

    /**
     * @method onObuDeviceProfileSelectionChange
     * 
     * Handles the event that is generated when the user changes the selected OBU device profile(s).
     * 
     * @protected
     * @param {Ext.selection.Model} model The selection model.
     * @param {Ext.data.Model[]} selected The selected OBU device profiles.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onObuDeviceProfileSelectionChange : function(model, selected, eOpts) {

        this.getViewModel().set('selectedObuDeviceProfile', selected.length === 1 ? selected[0] : null);
        this.getView().lookupReference('obuDeviceProfileToolbar').lookupReference('deleteButton').setDisabled(selected.length === 0);
    },

    /**
     * @method onRseDeviceSelectionChange
     * 
     * Handles the event that is generated when the user changes the selected RSE device(s).
     * 
     * @protected
     * @param {Ext.selection.Model} model The selection model.
     * @param {Ext.data.Model[]} selected The selected RSE devices.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onRseDeviceSelectionChange : function(model, selected, eOpts) {

        this.getViewModel().set('selectedRseDevice', selected.length === 1 ? selected[0] : null);
        this.getView().lookupReference('rseDeviceToolbar').lookupReference('deleteButton').setDisabled(selected.length === 0);
    },

    /**
     * @method showCellularDeviceProfileApplicationsWindow
     * 
     * Shows the window to view the hosted {@link ETexas.model.ApplicationModel} data for the
     * selected cellular {@link ETexas.model.DeviceProfileModel}.
     * 
     * @protected
     */
    showCellularDeviceProfileApplicationsWindow : function() {

        var window = this.getView().add({
            xtype : 'applicationhostwindow',
            id : Ext.id(null, 'cellular-device-profile-applications-window-')
        });

        var viewModel = this.getViewModel();
        var selectedComposite = viewModel.get('selectedComposite');
        var selectedDeviceProfile = viewModel.get('selectedCellularDeviceProfile');

        var windowModel = window.getViewModel();
        windowModel.set('applicationsUrl', ETexas.util.UrlProvider.getDeviceProfileApplicationsUrl(selectedComposite, selectedDeviceProfile));
        windowModel.set('hostType', 'Cellular');
        window.show();
    },

    /**
     * @method showCreateBuildingForm
     * 
     * Shows the form to create a new building {@link ETexas.model.TopographyFeatureModel}.
     * 
     * @protected
     */
    showCreateBuildingForm : function() {

        var form = this.getView().add({
            xtype : 'createbuildingform',
            id : Ext.id(null, 'create-building-form-')
        });

        form.on('buildingadded', this._updateBuildingStore, this);
        form.show();
    },

    /**
     * @method showCreateCellTowerForm
     * 
     * Shows the form to create a new {@link ETexas.model.CellTowerModel}.
     * 
     * @protected
     */
    showCreateCellTowerForm : function() {

        var form = this.getView().add({
            xtype : 'createcelltowerform',
            id : Ext.id(null, 'create-cell-tower-form-')
        });

        form.on('celltoweradded', this._updateCellTowerStore, this);
        form.show();
    },

    /**
     * @method showCreateCellularDeviceProfileForm
     * 
     * Shows the form to create a new cellular {@link ETexas.model.DeviceProfileModel}.
     * 
     * @protected
     */
    showCreateCellularDeviceProfileForm : function() {

        var form = this.getView().add({
            xtype : 'createcellulardeviceprofileform',
            id : Ext.id(null, 'create-cellular-device-profile-form-')
        });

        form.on('cellulardeviceprofileadded', this._updateCellularDeviceProfileStore, this);
        form.show();
    },

    /**
     * @method showCreateFixedCellularDeviceForm
     * 
     * Shows the form to create a new fixed cellular {@link ETexas.model.DeviceModel}.
     * 
     * @protected
     */
    showCreateFixedCellularDeviceForm : function() {

        var form = this.getView().add({
            xtype : 'createfixedcellulardeviceform',
            id : Ext.id(null, 'create-fixed-cellular-device-form-')
        });

        form.on('fixedcellulardeviceadded', this._updateFixedCellularDeviceStore, this);
        form.show();
    },

    /**
     * @method showCreateLaneMappingForm
     * 
     * Shows the form to create a new {@link ETexas.model.LaneMappingModel}.
     * 
     * @protected
     */
    showCreateLaneMappingForm : function() {

        var form = this.getView().add({
            xtype : 'createlanemappingform',
            id : Ext.id(null, 'create-lane-mapping-form-')
        });

        form.on('lanemappingadded', this._updateLaneMappingStore, this);
        form.show();
    },

    /**
     * @method showCreateObuDeviceProfileForm
     * 
     * Shows the form to create a new OBU {@link ETexas.model.DeviceProfileModel}.
     * 
     * @protected
     */
    showCreateObuDeviceProfileForm : function() {

        var form = this.getView().add({
            xtype : 'createobudeviceprofileform',
            id : Ext.id(null, 'create-obu-device-profile-form-')
        });

        form.on('obudeviceprofileadded', this._updateObuDeviceProfileStore, this);
        form.show();
    },

    /**
     * @method showCreateRseDeviceForm
     * 
     * Shows the form to create a new RSE {@link ETexas.model.DeviceModel}.
     * 
     * @protected
     */
    showCreateRseDeviceForm : function() {

        var form = this.getView().add({
            xtype : 'creatersedeviceform',
            id : Ext.id(null, 'create-rse-device-form-')
        });

        form.on('rsedeviceadded', this._updateRseDeviceStore, this);
        form.show();
    },

    /**
     * @method showEditBuildingForm
     * 
     * Shows the form to edit the selected building {@link ETexas.model.TopographyFeatureModel}.
     * 
     * @protected
     */
    showEditBuildingForm : function() {

        var form = this.getView().add({
            xtype : 'editbuildingform',
            id : Ext.id(null, 'edit-building-form-')
        });

        form.on('buildingupdated', this._updateBuildingStore, this);
        form.show();
    },

    /**
     * @method showEditCellTowerForm
     * 
     * Shows the form to edit the selected {@link ETexas.model.CellTowerModel}.
     * 
     * @protected
     */
    showEditCellTowerForm : function() {

        var form = this.getView().add({
            xtype : 'editcelltowerform',
            id : Ext.id(null, 'edit-cell-tower-form-')
        });

        form.on('celltowerupdated', this._updateCellTowerStore, this);
        form.show();
    },

    /**
     * @method showEditCellularConfigurationForm
     * 
     * Shows the form to edit the cellular configuration for the selected
     * {@link ETexas.model.CompositeModel}.
     * 
     * @protected
     */
    showEditCellularConfigurationForm : function() {

        var form = this.getView().add({
            xtype : 'editcellularconfigurationform',
            id : Ext.id(null, 'edit-cellular-configuration-form-')
        });

        form.show();
    },

    /**
     * @method showEditCellularDeviceProfileForm
     * 
     * Shows the form to edit the selected cellular {@link ETexas.model.DeviceProfileModel}.
     * 
     * @protected
     */
    showEditCellularDeviceProfileForm : function() {

        var form = this.getView().add({
            xtype : 'editcellulardeviceprofileform',
            id : Ext.id(null, 'edit-cellular-device-profile-form-')
        });

        form.on('cellulardeviceprofileupdated', this._updateCellularDeviceProfileStore, this);
        form.show();
    },

    /**
     * @method showEditCompositeOptionsForm
     * 
     * Shows the form to edit the options for the selected {@link ETexas.model.CompositeModel}.
     * 
     * @protected
     */
    showEditCompositeOptionsForm : function() {

        var form = this.getView().add({
            xtype : 'editcompositeoptionsform',
            id : Ext.id(null, 'edit-composite-options-form-')
        });

        form.on('compositeoptionsupdated', this._updateOptions, this);
        form.show();
    },

    /**
     * @method showEditFixedCellularDeviceForm
     * 
     * Shows the form to edit the selected fixed cellular {@link ETexas.model.DeviceModel}.
     * 
     * @protected
     */
    showEditFixedCellularDeviceForm : function() {

        var form = this.getView().add({
            xtype : 'editfixedcellulardeviceform',
            id : Ext.id(null, 'edit-fixed-cellular-device-form-')
        });

        form.on('fixedcellulardeviceupdated', this._updateFixedCellularDeviceStore, this);
        form.show();
    },

    /**
     * @method showEditLaneMappingForm
     * 
     * Shows the form to edit the selected {@link ETexas.model.LaneMappingModel}.
     * 
     * @protected
     */
    showEditLaneMappingForm : function() {

        var form = this.getView().add({
            xtype : 'editlanemappingform',
            id : Ext.id(null, 'edit-lane-mapping-form-')
        });

        form.on('lanemappingupdated', this._updateLaneMappingStore, this);
        form.show();
    },

    /**
     * @method showEditObuDeviceProfileForm
     * 
     * Shows the form to edit the selected OBU {@link ETexas.model.DeviceProfileModel}.
     * 
     * @protected
     */
    showEditObuDeviceProfileForm : function() {

        var form = this.getView().add({
            xtype : 'editobudeviceprofileform',
            id : Ext.id(null, 'edit-obu-device-profile-form-')
        });

        form.on('obudeviceprofileupdated', this._updateObuDeviceProfileStore, this);
        form.show();
    },

    /**
     * @method showEditRseDeviceForm
     * 
     * Shows the form to edit the selected RSE {@link ETexas.model.DeviceModel}.
     * 
     * @protected
     */
    showEditRseDeviceForm : function() {

        var form = this.getView().add({
            xtype : 'editrsedeviceform',
            id : Ext.id(null, 'edit-rse-device-form-')
        });

        form.on('rsedeviceupdated', this._updateRseDeviceStore, this);
        form.show();
    },

    /**
     * @method showFixedCellularDeviceApplicationsWindow
     * 
     * Shows the window to view the hosted {@link ETexas.model.ApplicationModel} data for the
     * selected fixed cellular {@link ETexas.model.DeviceModel}.
     * 
     * @protected
     */
    showFixedCellularDeviceApplicationsWindow : function() {

        var window = this.getView().add({
            xtype : 'applicationhostwindow',
            id : Ext.id(null, 'fixed-cellular-device-applications-window-')
        });

        var viewModel = this.getViewModel();
        var selectedComposite = viewModel.get('selectedComposite');
        var selectedDevice = viewModel.get('selectedFixedCellularDevice');

        var windowModel = window.getViewModel();
        windowModel.set('applicationsUrl', ETexas.util.UrlProvider.getDeviceApplicationsUrl(selectedComposite, selectedDevice));
        windowModel.set('hostType', 'Cellular');
        window.show();
    },

    /**
     * @method showObuDeviceProfileApplicationsWindow
     * 
     * Shows the window to view the hosted {@link ETexas.model.ApplicationModel} data for the
     * selected OBU {@link ETexas.model.DeviceProfileModel}.
     * 
     * @protected
     */
    showObuDeviceProfileApplicationsWindow : function() {

        var window = this.getView().add({
            xtype : 'applicationhostwindow',
            id : Ext.id(null, 'obu-device-profile-applications-window-')
        });

        var viewModel = this.getViewModel();
        var selectedComposite = viewModel.get('selectedComposite');
        var selectedDeviceProfile = viewModel.get('selectedObuDeviceProfile');

        var windowModel = window.getViewModel();
        windowModel.set('applicationsUrl', ETexas.util.UrlProvider.getDeviceProfileApplicationsUrl(selectedComposite, selectedDeviceProfile));
        windowModel.set('hostType', 'OBU');
        window.show();
    },

    /**
     * @method showRseDeviceApplicationsWindow
     * 
     * Shows the window to view the hosted {@link ETexas.model.ApplicationModel} data for the
     * selected RSE {@link ETexas.model.DeviceModel}.
     * 
     * @protected
     */
    showRseDeviceApplicationsWindow : function() {

        var window = this.getView().add({
            xtype : 'applicationhostwindow',
            id : Ext.id(null, 'rse-device-applications-window-')
        });

        var viewModel = this.getViewModel();
        var selectedComposite = viewModel.get('selectedComposite');
        var selectedDevice = viewModel.get('selectedRseDevice');

        var windowModel = window.getViewModel();
        windowModel.set('applicationsUrl', ETexas.util.UrlProvider.getDeviceApplicationsUrl(selectedComposite, selectedDevice));
        windowModel.set('hostType', 'RSE');
        window.show();
    },

    /**
     * @method _updateBuildingStore
     * 
     * Updates the building data for the selected composite.
     * 
     * @private
     * @param {Number} [buildingId] The ID of the selected building.
     */
    _updateBuildingStore : function(buildingId) {

        var viewModel = this.getViewModel();
        var topographyFeatureStore = viewModel.get('topographyFeatureStore');
        var buildingGrid = this.getView().lookupReference('buildingGrid');
        topographyFeatureStore.load(function(records, operation, success) {
            var selectedBuilding = buildingId ? topographyFeatureStore.getById(buildingId) : null;
            buildingGrid.setSelection(selectedBuilding);
            viewModel.set('selectedBuilding', selectedBuilding);
        });
    },

    /**
     * @method _updateCellTowerStore
     * 
     * Updates the cell tower data for the selected simulation.
     * 
     * @private
     * @param {Number} [cellTowerId] The ID of the selected cell tower.
     */
    _updateCellTowerStore : function(cellTowerId) {

        var viewModel = this.getViewModel();
        var cellTowerStore = viewModel.get('cellTowerStore');
        var cellTowerGrid = this.getView().lookupReference('cellTowerGrid');
        cellTowerStore.load(function(records, operation, success) {
            var selectedCellTower = cellTowerId ? cellTowerStore.getById(cellTowerId) : null;
            cellTowerGrid.setSelection(selectedCellTower);
            viewModel.set('selectedCellTower', selectedCellTower);
        });
    },

    /**
     * @method _updateCellularDeviceProfileStore
     * 
     * Updates the cellular device profile data for the selected composite.
     * 
     * @private
     * @param {Number} [cellularDeviceProfileId] The ID of the selected cellular device profile.
     */
    _updateCellularDeviceProfileStore : function(cellularDeviceProfileId) {

        var viewModel = this.getViewModel();
        var deviceProfileStore = viewModel.get('deviceProfileStore');
        var cellularDeviceProfileGrid = this.getView().lookupReference('cellularDeviceProfileGrid');
        deviceProfileStore.load(function(records, operation, success) {
            var selectedCellularDeviceProfile = cellularDeviceProfileId ? deviceProfileStore.getById(cellularDeviceProfileId) : null;
            cellularDeviceProfileGrid.setSelection(selectedCellularDeviceProfile);
            viewModel.set('selectedCellularDeviceProfile', selectedCellularDeviceProfile);
        });
    },

    /**
     * @method _updateFixedCellularDeviceStore
     * 
     * Updates the fixed cellular device data for the selected composite.
     * 
     * @private
     * @param {Number} [fixedCellularDeviceId] The ID of the selected fixed cellular device.
     */
    _updateFixedCellularDeviceStore : function(fixedCellularDeviceId) {

        var viewModel = this.getViewModel();
        var deviceStore = viewModel.get('deviceStore');
        var fixedCellularDeviceGrid = this.getView().lookupReference('fixedCellularDeviceGrid');
        deviceStore.load(function(records, operation, success) {
            var selectedFixedCellularDevice = fixedCellularDeviceId ? deviceStore.getById(fixedCellularDeviceId) : null;
            fixedCellularDeviceGrid.setSelection(selectedFixedCellularDevice);
            viewModel.set('selectedFixedCellularDevice', selectedFixedCellularDevice);
        });
    },

    /**
     * @method _updateLaneMappingStore
     * 
     * Updates the lane mapping data for the selected composite.
     * 
     * @private
     * @param {Number} [laneMappingId] The ID of the selected lane mapping.
     */
    _updateLaneMappingStore : function(laneMappingId) {

        var viewModel = this.getViewModel();
        var laneMappingStore = viewModel.get('laneMappingStore');
        var laneMappingGrid = this.getView().lookupReference('laneMappingGrid');
        laneMappingStore.load(function(records, operation, success) {

            var simulations = viewModel.get('selectedComposite').getSimulations();
            laneMappingStore.each(function(laneMapping) {
                laneMapping.set('sourceSimulationName', simulations.getById(laneMapping.get('sourceSimulation')).get('name'));
                laneMapping.set('targetSimulationName', simulations.getById(laneMapping.get('targetSimulation')).get('name'));
            });

            laneMappingStore.commitChanges();
            var selectedLaneMapping = laneMappingId ? laneMappingStore.getById(laneMappingId) : null;
            laneMappingGrid.setSelection(selectedLaneMapping);
            viewModel.set('selectedLaneMapping', selectedLaneMapping);
        });
    },

    /**
     * @method _updateObuDeviceProfileStore
     * 
     * Updates the OBU device profile data for the selected composite.
     * 
     * @private
     * @param {Number} [obuDeviceProfileId] The ID of the selected OBU device profile.
     */
    _updateObuDeviceProfileStore : function(obuDeviceProfileId) {

        var viewModel = this.getViewModel();
        var deviceProfileStore = viewModel.get('deviceProfileStore');
        var obuDeviceProfileGrid = this.getView().lookupReference('obuDeviceProfileGrid');
        deviceProfileStore.load(function(records, operation, success) {
            var selectedObuDeviceProfile = obuDeviceProfileId ? deviceProfileStore.getById(obuDeviceProfileId) : null;
            obuDeviceProfileGrid.setSelection(selectedObuDeviceProfile);
            viewModel.set('selectedObuDeviceProfile', selectedObuDeviceProfile);
        });
    },

    /**
     * @method _updateOptions
     * 
     * Updates the composite options data.
     * 
     * @private
     */
    _updateOptions : function() {

        var viewModel = this.getViewModel();
        var selectedComposite = viewModel.get('selectedComposite');

        Ext.Ajax.request({
            scope : this,
            method : 'GET',
            url : ETexas.util.UrlProvider.getCompositeOptionsUrl(selectedComposite),
            success : function(response, opts) {

                var options = Ext.JSON.decode(response.responseText);
                viewModel.set('latitude', options.latitude);
                viewModel.set('longitude', options.longitude);
                viewModel.set('geographicCalculator', options.geographicCalculator);
                viewModel.set('propagationLossModel', options.propagationLossModel);
                viewModel.set('communicationsModel', options.communicationsModel);
            }
        });
    },

    /**
     * @method _updateRseDeviceStore
     * 
     * Updates the RSE device data for the selected simulation.
     * 
     * @private
     * @param {Number} [rseDeviceId] The ID of the selected RSE device.
     */
    _updateRseDeviceStore : function(rseDeviceId) {

        var viewModel = this.getViewModel();
        var deviceStore = viewModel.get('deviceStore');
        var rseDeviceGrid = this.getView().lookupReference('rseDeviceGrid');
        deviceStore.load(function(records, operation, success) {
            var selectedRseDevice = rseDeviceId ? deviceStore.getById(rseDeviceId) : null;
            rseDeviceGrid.setSelection(selectedRseDevice);
            viewModel.set('selectedRseDevice', selectedRseDevice);
        });
    }
});
