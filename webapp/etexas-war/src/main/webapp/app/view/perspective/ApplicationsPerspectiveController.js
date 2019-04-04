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
 * @class ETexas.view.perspective.ApplicationsPerspectiveController
 * @extends Ext.app.ViewController
 * 
 * The {@link ETexas.view.perspective.ApplicationsPerspective} controller.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.perspective.ApplicationsPerspectiveController', {
    extend : 'Ext.app.ViewController',
    alias : 'controller.applicationsperspective',

    /** @inheritdoc */
    init : function(view) {

        /*
         * TODO: emyers - It is uncertain why the call to notify is necessary, but chained stores
         * are not loading without it at the moment. This should be revisited and, hopefully,
         * removed once the issue is resolved.
         */

        var viewModel = this.getViewModel();
        viewModel.notify();

        var embeddedApplicationProfileToolbar = view.lookupReference('embeddedApplicationProfileToolbar');
        var jarApplicationProfileToolbar = view.lookupReference('jarApplicationProfileToolbar');
        var nativeApplicationProfileToolbar = view.lookupReference('nativeApplicationProfileToolbar');
        var remoteApplicationProfileToolbar = view.lookupReference('remoteApplicationProfileToolbar');

        jarApplicationProfileToolbar.lookupReference('deleteButton').setDisabled(true);
        nativeApplicationProfileToolbar.lookupReference('deleteButton').setDisabled(true);
        remoteApplicationProfileToolbar.lookupReference('deleteButton').setDisabled(true);

        viewModel.bind('{selectedEmbeddedApplicationProfile}', function(selectedEmbeddedApplicationProfile) {
            embeddedApplicationProfileToolbar.lookupReference('parametersButton').setDisabled(!selectedEmbeddedApplicationProfile);
        });

        viewModel.bind('{selectedJarApplicationProfile}', function(selectedJarApplicationProfile) {
            jarApplicationProfileToolbar.lookupReference('editButton').setDisabled(!selectedJarApplicationProfile);
            jarApplicationProfileToolbar.lookupReference('parametersButton').setDisabled(!selectedJarApplicationProfile);
        });

        viewModel.bind('{selectedNativeApplicationProfile}', function(selectedNativeApplicationProfile) {
            nativeApplicationProfileToolbar.lookupReference('editButton').setDisabled(!selectedNativeApplicationProfile);
            nativeApplicationProfileToolbar.lookupReference('parametersButton').setDisabled(!selectedNativeApplicationProfile);
        });

        viewModel.bind('{selectedRemoteApplicationProfile}', function(selectedRemoteApplicationProfile) {
            remoteApplicationProfileToolbar.lookupReference('editButton').setDisabled(!selectedRemoteApplicationProfile);
            remoteApplicationProfileToolbar.lookupReference('parametersButton').setDisabled(!selectedRemoteApplicationProfile);
        });
    },

    /**
     * @method deleteJarApplicationProfiles
     * 
     * Deletes the selected JAR {@link ETexas.model.ApplicationProfileModel}s.
     * 
     * @protected
     */
    deleteJarApplicationProfiles : function() {

        var me = this;
        var view = this.getView();
        var selectedApplicationProfiles = view.lookupReference('jarApplicationProfileGrid').getSelection();

        var mask;
        var message;
        var messageTitle;

        if (selectedApplicationProfiles.length === 1) {

            mask = 'Deleting JAR Application...';
            message = 'Deleting \"' + selectedApplicationProfiles[0].get('name') + '\" will affect any executions with instances of the application. ';
            message = message.concat('All JAR applications uploaded from the same file will also be removed. Are you sure you want to delete it?');
            messageTitle = 'Delete JAR Application';
        }
        else {

            mask = 'Deleting JAR Applications...';
            message = 'Deleting the selected JAR applications will affect any executions with instances of the applications. All JAR applications uploaded from the ';
            message = message.concat('same files will also be removed. Are you sure you want to delete them?');
            messageTitle = 'Delete JAR Applications';
        }

        Ext.Msg.confirm(messageTitle, message, function(btn) {

            if (btn === 'yes') {

                var applicationProfileIds = [];
                selectedApplicationProfiles.forEach(function(value, index, array) {
                    applicationProfileIds.push(value.get('id'));
                });

                var queryParams = Ext.Object.toQueryString({
                    applicationProfileIds : applicationProfileIds
                });

                view.mask(mask);
                Ext.Ajax.request({
                    scope : this,
                    method : 'DELETE',
                    url : ETexas.util.UrlProvider.getApplicationProfilesUrl() + '?' + queryParams,
                    success : function(response, opts) {

                        view.unmask();
                        me._updateJarApplicationProfileStore();
                    },
                    failure : function(response, opts) {

                        view.unmask();
                    }
                });
            }
        });
    },

    /**
     * @method deleteNativeApplicationProfiles
     * 
     * Deletes the selected native {@link ETexas.model.ApplicationProfileModel}s.
     * 
     * @protected
     */
    deleteNativeApplicationProfiles : function() {

        var me = this;
        var view = this.getView();
        var selectedApplicationProfiles = view.lookupReference('nativeApplicationProfileGrid').getSelection();

        var mask;
        var message;
        var messageTitle;

        if (selectedApplicationProfiles.length === 1) {

            mask = 'Deleting Native Application...';
            message = 'Deleting \"' + selectedApplicationProfiles[0].get('name') + '\" will affect any executions with instances of the application. Are you sure you want to delete it?';
            messageTitle = 'Delete Native Application';
        }
        else {

            mask = 'Deleting Native Applications...';
            message = 'Deleting the selected native applications will affect any executions with instances of the applications. Are you sure you want to delete them?';
            messageTitle = 'Delete Native Applications';
        }

        Ext.Msg.confirm(messageTitle, message, function(btn) {

            if (btn === 'yes') {

                var applicationProfileIds = [];
                selectedApplicationProfiles.forEach(function(value, index, array) {
                    applicationProfileIds.push(value.get('id'));
                });

                var queryParams = Ext.Object.toQueryString({
                    applicationProfileIds : applicationProfileIds
                });

                view.mask(mask);
                Ext.Ajax.request({
                    scope : this,
                    method : 'DELETE',
                    url : ETexas.util.UrlProvider.getApplicationProfilesUrl() + '?' + queryParams,
                    success : function(response, opts) {

                        view.unmask();
                        me._updateNativeApplicationProfileStore();
                    },
                    failure : function(response, opts) {

                        view.unmask();
                    }
                });
            }
        });
    },

    /**
     * @method deleteRemoteApplicationProfiles
     * 
     * Deletes the selected remote {@link ETexas.model.ApplicationProfileModel}s.
     * 
     * @protected
     */
    deleteRemoteApplicationProfiles : function() {

        var me = this;
        var view = this.getView();
        var selectedApplicationProfiles = view.lookupReference('remoteApplicationProfileGrid').getSelection();

        var mask;
        var message;
        var messageTitle;

        if (selectedApplicationProfiles.length === 1) {

            mask = 'Deleting Remote Application...';
            message = 'Deleting \"' + selectedApplicationProfiles[0].get('name') + '\" will affect any executions with instances of the application. Are you sure you want to delete it?';
            messageTitle = 'Delete Remote Application';
        }
        else {

            mask = 'Deleting Remote Applications...';
            message = 'Deleting the selected remote applications will affect any executions with instances of the applications. Are you sure you want to delete them?';
            messageTitle = 'Delete Remote Applications';
        }

        Ext.Msg.confirm(messageTitle, message, function(btn) {

            if (btn === 'yes') {

                var applicationProfileIds = [];
                selectedApplicationProfiles.forEach(function(value, index, array) {
                    applicationProfileIds.push(value.get('id'));
                });

                var queryParams = Ext.Object.toQueryString({
                    applicationProfileIds : applicationProfileIds
                });

                view.mask(mask);
                Ext.Ajax.request({
                    scope : this,
                    method : 'DELETE',
                    url : ETexas.util.UrlProvider.getApplicationProfilesUrl() + '?' + queryParams,
                    success : function(response, opts) {

                        view.unmask();
                        me._updateRemoteApplicationProfileStore();
                    },
                    failure : function(response, opts) {

                        view.unmask();
                    }
                });
            }
        });
    },

    /**
     * @method onEmbeddedApplicationProfileSelectionChange
     * 
     * Handles the event that is generated when the user changes the selected embedded application
     * profile(s).
     * 
     * @protected
     * @param {Ext.selection.Model} model The selection model.
     * @param {Ext.data.Model[]} selected The selected embedded application profiles.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onEmbeddedApplicationProfileSelectionChange : function(model, selected, eOpts) {

        this.getViewModel().set('selectedEmbeddedApplicationProfile', selected.length === 1 ? selected[0] : null);
    },

    /**
     * @method onHelpToolClicked
     * 
     * Handles the event that is generated when the user clicks the Connected Vehicle Applications
     * Help tool.
     * 
     * @protected
     * @param {Ext.panel.Tool} tool The clicked tool.
     * @param {Ext.event.Event} e The click event.
     * @param {Ext.Component} owner The owner of the tool.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onHelpToolClicked : function(tool, e, owner, eOpts) {

        window.open(ETexas.util.UrlProvider.getHelpContentUrl() + '/Applications/applications.htm');
    },

    /**
     * @method onJarApplicationProfileSelectionChange
     * 
     * Handles the event that is generated when the user changes the selected JAR application
     * profile(s).
     * 
     * @protected
     * @param {Ext.selection.Model} model The selection model.
     * @param {Ext.data.Model[]} selected The selected JAR application profiles.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onJarApplicationProfileSelectionChange : function(model, selected, eOpts) {

        this.getViewModel().set('selectedJarApplicationProfile', selected.length === 1 ? selected[0] : null);
        this.getView().lookupReference('jarApplicationProfileToolbar').lookupReference('deleteButton').setDisabled(selected.length === 0);
    },

    /**
     * @method onNativeApplicationProfileSelectionChange
     * 
     * Handles the event that is generated when the user changes the selected native application
     * profile(s).
     * 
     * @protected
     * @param {Ext.selection.Model} model The selection model.
     * @param {Ext.data.Model[]} selected The selected native application profiles.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onNativeApplicationProfileSelectionChange : function(model, selected, eOpts) {

        this.getViewModel().set('selectedNativeApplicationProfile', selected.length === 1 ? selected[0] : null);
        this.getView().lookupReference('nativeApplicationProfileToolbar').lookupReference('deleteButton').setDisabled(selected.length === 0);
    },

    /**
     * @method onRemoteApplicationProfileSelectionChange
     * 
     * Handles the event that is generated when the user changes the selected remote application
     * profile(s).
     * 
     * @protected
     * @param {Ext.selection.Model} model The selection model.
     * @param {Ext.data.Model[]} selected The selected remote application profiles.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onRemoteApplicationProfileSelectionChange : function(model, selected, eOpts) {

        this.getViewModel().set('selectedRemoteApplicationProfile', selected.length === 1 ? selected[0] : null);
        this.getView().lookupReference('remoteApplicationProfileToolbar').lookupReference('deleteButton').setDisabled(selected.length === 0);
    },

    /**
     * @method showCreateNativeApplicationProfileForm
     * 
     * Shows the form the create a new native {@link ETexas.model.ApplicationProfileModel}.
     * 
     * @protected
     */
    showCreateNativeApplicationProfileForm : function() {

        var form = Ext.create('ETexas.view.applicationprofile.CreateNativeApplicationProfileForm', {
            id : Ext.id(null, 'create-native-application-profile-form-')
        });

        form.on('nativeapplicationprofileadded', this._updateNativeApplicationProfileStore, this);
        form.show();
    },

    /**
     * @method showCreateRemoteApplicationProfileForm
     * 
     * Shows the form the create a new remote {@link ETexas.model.ApplicationProfileModel}.
     * 
     * @protected
     */
    showCreateRemoteApplicationProfileForm : function() {

        var form = Ext.create('ETexas.view.applicationprofile.CreateRemoteApplicationProfileForm', {
            id : Ext.id(null, 'create-remote-application-profile-form-')
        });

        form.on('remoteapplicationprofileadded', this._updateRemoteApplicationProfileStore, this);
        form.show();
    },

    /**
     * @method showEditJarApplicationProfileForm
     * 
     * Shows the form to edit the selected JAR {@link ETexas.model.ApplicationProfileModel}.
     * 
     * @protected
     */
    showEditJarApplicationProfileForm : function() {

        var form = Ext.create('ETexas.view.applicationprofile.EditJarApplicationProfileForm', {
            id : Ext.id(null, 'edit-jar-application-profile-form-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        form.on('jarapplicationprofileupdated', this._updateJarApplicationProfileStore, this);
        form.show();
    },

    /**
     * @method showEditNativeApplicationProfileForm
     * 
     * Shows the form to edit the selected native {@link ETexas.model.ApplicationProfileModel}.
     * 
     * @protected
     */
    showEditNativeApplicationProfileForm : function() {

        var form = Ext.create('ETexas.view.applicationprofile.EditNativeApplicationProfileForm', {
            id : Ext.id(null, 'edit-native-application-profile-form-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        form.on('nativeapplicationprofileupdated', this._updateNativeApplicationProfileStore, this);
        form.show();
    },

    /**
     * @method showEditRemoteApplicationProfileForm
     * 
     * Shows the form to edit the selected remote {@link ETexas.model.ApplicationProfileModel}.
     * 
     * @protected
     */
    showEditRemoteApplicationProfileForm : function() {

        var form = Ext.create('ETexas.view.applicationprofile.EditRemoteApplicationProfileForm', {
            id : Ext.id(null, 'edit-remote-application-profile-form-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        form.on('remoteapplicationprofileupdated', this._updateRemoteApplicationProfileStore, this);
        form.show();
    },

    /**
     * @method showEmbeddedApplicationParameterProfilesWindow
     * 
     * Shows the window to view the parameter profiles for the selected embedded
     * {@link ETexas.model.ApplicationProfileModel}.
     * 
     * @protected
     */
    showEmbeddedApplicationParameterProfilesWindow : function() {

        var window = Ext.create('ETexas.view.applicationprofile.ApplicationParameterProfileWindow', {
            id : Ext.id(null, 'embedded-application-parameter-profiles-window-')
        });

        var grid = window.lookupReference('applicationParameterProfileGrid');
        grid.setStore(this.getViewModel().get('selectedEmbeddedApplicationProfile').getParameterProfiles());
        window.show();
    },

    /**
     * @method showJarApplicationParameterProfilesWindow
     * 
     * Shows the window to view the parameter profiles for the selected JAR
     * {@link ETexas.model.ApplicationProfileModel}.
     * 
     * @protected
     */
    showJarApplicationParameterProfilesWindow : function() {

        var window = Ext.create('ETexas.view.applicationprofile.ApplicationParameterProfileWindow', {
            id : Ext.id(null, 'jar-application-parameter-profiles-window-')
        });

        var grid = window.lookupReference('applicationParameterProfileGrid');
        grid.setStore(this.getViewModel().get('selectedJarApplicationProfile').getParameterProfiles());
        window.show();
    },

    /**
     * @method showNativeApplicationParameterProfilesWindow
     * 
     * Shows the window to view the parameter profiles for the selected native
     * {@link ETexas.model.ApplicationProfileModel}.
     * 
     * @protected
     */
    showNativeApplicationParameterProfilesWindow : function() {

        var window = Ext.create('ETexas.view.applicationprofile.ApplicationParameterProfileWindow', {
            id : Ext.id(null, 'native-application-parameter-profiles-window-')
        });

        var grid = window.lookupReference('applicationParameterProfileGrid');
        grid.setStore(this.getViewModel().get('selectedNativeApplicationProfile').getParameterProfiles());
        window.show();
    },

    /**
     * @method showRemoteApplicationParameterProfilesWindow
     * 
     * Shows the window to view the parameter profiles for the selected remote
     * {@link ETexas.model.ApplicationProfileModel}.
     * 
     * @protected
     */
    showRemoteApplicationParameterProfilesWindow : function() {

        var window = Ext.create('ETexas.view.applicationprofile.ApplicationParameterProfileWindow', {
            id : Ext.id(null, 'remote-application-parameter-profiles-window-')
        });

        var grid = window.lookupReference('applicationParameterProfileGrid');
        grid.setStore(this.getViewModel().get('selectedRemoteApplicationProfile').getParameterProfiles());
        window.show();
    },

    /**
     * @method showUploadJarApplicationProfileForm
     * 
     * Shows the form the upload new JAR {@link ETexas.model.ApplicationProfileModel}s.
     * 
     * @protected
     */
    showUploadJarApplicationProfileForm : function() {

        var form = Ext.create('ETexas.view.applicationprofile.UploadJarApplicationProfileForm', {
            id : Ext.id(null, 'upload-jar-application-profile-form-')
        });

        form.on('jarapplicationprofilesuploaded', this._updateJarApplicationProfileStore, this);
        form.show();
    },

    /**
     * @method _updateJarApplicationProfileStore
     * 
     * Updates the JAR application profile data for the current user.
     * 
     * @private
     * @param {Number} [jarApplicationProfileId] The ID of the selected JAR application profile.
     */
    _updateJarApplicationProfileStore : function(jarApplicationProfileId) {

        var viewModel = this.getViewModel();
        var applicationProfileStore = viewModel.get('applicationProfileStore');
        var jarApplicationProfileGrid = this.getView().lookupReference('jarApplicationProfileGrid');
        applicationProfileStore.load(function(record, operations, success) {
            var selectedJarApplicationProfile = jarApplicationProfileId ? applicationProfileStore.getById(jarApplicationProfileId) : null;
            jarApplicationProfileGrid.setSelection(selectedJarApplicationProfile);
            viewModel.set('selectedJarApplicationProfile', selectedJarApplicationProfile);
        });
    },

    /**
     * @method _updateNativeApplicationProfileStore
     * 
     * Updates the native application profile data for the current user.
     * 
     * @private
     * @param {Number} [nativeApplicationProfileId] The ID of the selected native application
     * profile.
     */
    _updateNativeApplicationProfileStore : function(nativeApplicationProfileId) {

        var viewModel = this.getViewModel();
        var applicationProfileStore = viewModel.get('applicationProfileStore');
        var nativeApplicationProfileGrid = this.getView().lookupReference('nativeApplicationProfileGrid');
        applicationProfileStore.load(function(record, operations, success) {
            var selectedNativeApplicationProfile = nativeApplicationProfileId ? applicationProfileStore.getById(nativeApplicationProfileId) : null;
            nativeApplicationProfileGrid.setSelection(selectedNativeApplicationProfile);
            viewModel.set('selectedNativeApplicationProfile', selectedNativeApplicationProfile);
        });
    },

    /**
     * @method _updateRemoteApplicationProfileStore
     * 
     * Updates the remote application profile data for the current user.
     * 
     * @private
     * @param {Number} [remoteApplicationProfileId] The ID of the selected remote application
     * profile.
     */
    _updateRemoteApplicationProfileStore : function(remoteApplicationProfileId) {

        var viewModel = this.getViewModel();
        var applicationProfileStore = viewModel.get('applicationProfileStore');
        var remoteApplicationProfileGrid = this.getView().lookupReference('remoteApplicationProfileGrid');
        applicationProfileStore.load(function(record, operations, success) {
            var selectedRemoteApplicationProfile = remoteApplicationProfileId ? applicationProfileStore.getById(remoteApplicationProfileId) : null;
            remoteApplicationProfileGrid.setSelection(selectedRemoteApplicationProfile);
            viewModel.set('selectedRemoteApplicationProfile', selectedRemoteApplicationProfile);
        });
    }
});
