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
 * @class ETexas.view.perspective.ApplicationsPerspective
 * @extends Ext.tab.Panel
 * 
 * A tab panel to view connected vehicle application content.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.perspective.ApplicationsPerspective', {
    extend : 'Ext.tab.Panel',
    xtype : 'applicationsperspective',

    requires : [ 'ETexas.view.applicationprofile.ApplicationProfileGrid', 'ETexas.view.applicationprofile.ApplicationProfileToolbar',
            'ETexas.view.applicationprofile.EmbeddedApplicationProfileToolbar', 'ETexas.view.applicationprofile.JarApplicationProfileGrid',
            'ETexas.view.applicationprofile.NativeApplicationProfileGrid', 'ETexas.view.perspective.ApplicationsPerspectiveController', 'ETexas.view.perspective.ApplicationsPerspectiveModel' ],

    controller : 'applicationsperspective',

    viewModel : {
        type : 'applicationsperspective'
    },

    title : 'Connected Vehicle Applications',
    frame : true,

    /** @inheritdoc */
    initComponent : function() {

        this.items = this.buildItems();
        this.tools = this.buildTools();
        this.callParent();
    },

    /**
     * @method buildItems
     * 
     * Builds the items for this tab panel.
     * 
     * @template
     * @protected
     * @return {Object[]} The items for this tab panel.
     */
    buildItems : function() {

        return [ this._buildEmbeddedApplicationProfileGrid(), this._buildJarApplicationProfileGrid(), this._buildNativeApplicationProfileGrid(), this._buildRemoteApplicationProfileGrid() ];
    },

    /**
     * @method _buildEmbeddedApplicationProfileGrid
     * 
     * Builds the embedded application profile grid for this tab panel.
     * 
     * @private
     * @return {ETexas.view.applicationprofile.ApplicationProfileGrid} The embedded application
     * profile grid for this tab panel.
     */
    _buildEmbeddedApplicationProfileGrid : function() {

        return {
            xtype : 'applicationprofilegrid',
            reference : 'embeddedApplicationProfileGrid',
            id : Ext.id(null, 'embedded-application-profile-grid-'),
            title : 'Embedded Applications',
            dockedItems : [ {
                xtype : 'embeddedapplicationprofiletoolbar',
                reference : 'embeddedApplicationProfileToolbar',
                id : Ext.id(null, 'embedded-application-profile-toolbar-'),
                dock : 'top'
            } ],
            selModel : {
                selType : 'rowmodel',
                mode : 'MULTI'
            },
            bind : {
                store : '{embeddedApplicationProfileStore}'
            },
            listeners : {
                profileparameters : 'showEmbeddedApplicationParameterProfilesWindow',
                selectionChange : 'onEmbeddedApplicationProfileSelectionChange'
            }
        };
    },

    /**
     * @method _buildJarApplicationProfileGrid
     * 
     * Builds the JAR application profile grid for this tab panel.
     * 
     * @private
     * @return {ETexas.view.applicationprofile.ApplicationProfileGrid} The JAR application profile
     * grid for this tab panel.
     */
    _buildJarApplicationProfileGrid : function() {

        return {
            xtype : 'jarapplicationprofilegrid',
            reference : 'jarApplicationProfileGrid',
            id : Ext.id(null, 'jar-application-profile-grid-'),
            title : 'JAR Applications',
            dockedItems : [ {
                xtype : 'applicationprofiletoolbar',
                reference : 'jarApplicationProfileToolbar',
                id : Ext.id(null, 'jar-application-profile-toolbar-'),
                dock : 'top'
            } ],
            selModel : {
                selType : 'rowmodel',
                mode : 'MULTI'
            },
            bind : {
                store : '{jarApplicationProfileStore}'
            },
            listeners : {
                createmodel : 'showUploadJarApplicationProfileForm',
                deletemodel : 'deleteJarApplicationProfiles',
                editmodel : 'showEditJarApplicationProfileForm',
                profileparameters : 'showJarApplicationParameterProfilesWindow',
                selectionchange : 'onJarApplicationProfileSelectionChange'
            }
        };
    },

    /**
     * @method _buildNativeApplicationProfileGrid
     * 
     * Builds the native application profile grid for this tab panel.
     * 
     * @private
     * @return {ETexas.view.applicationprofile.NativeApplicationProfileGrid} The native application
     * profile grid for this panel.
     */
    _buildNativeApplicationProfileGrid : function() {

        return {
            xtype : 'nativeapplicationprofilegrid',
            reference : 'nativeApplicationProfileGrid',
            id : Ext.id(null, 'native-application-profile-grid-'),
            title : 'Native Applications',
            dockedItems : [ {
                xtype : 'applicationprofiletoolbar',
                reference : 'nativeApplicationProfileToolbar',
                id : Ext.id(null, 'native-application-profile-toolbar-'),
                dock : 'top'
            } ],
            selModel : {
                selType : 'rowmodel',
                mode : 'MULTI'
            },
            bind : {
                store : '{nativeApplicationProfileStore}'
            },
            listeners : {
                createmodel : 'showCreateNativeApplicationProfileForm',
                deletemodel : 'deleteNativeApplicationProfiles',
                editmodel : 'showEditNativeApplicationProfileForm',
                profileparameters : 'showNativeApplicationParameterProfilesWindow',
                selectionchange : 'onNativeApplicationProfileSelectionChange'
            }
        };
    },

    /**
     * @method _buildRemoteApplicationProfileGrid
     * 
     * Builds the remote application profile grid for this tab panel.
     * 
     * @private
     * @return {ETexas.view.applicationprofile.ApplicationProfileGrid} The remote application
     * profile grid for this tab panel.
     */
    _buildRemoteApplicationProfileGrid : function() {

        return {
            xtype : 'applicationprofilegrid',
            reference : 'remoteApplicationProfileGrid',
            id : Ext.id(null, 'remote-application-profile-grid-'),
            title : 'Remote Applications',
            dockedItems : [ {
                xtype : 'applicationprofiletoolbar',
                reference : 'remoteApplicationProfileToolbar',
                id : Ext.id(null, 'remote-application-profile-toolbar-'),
                dock : 'top'
            } ],
            selModel : {
                selType : 'rowmodel',
                mode : 'MULTI'
            },
            bind : {
                store : '{remoteApplicationProfileStore}'
            },
            listeners : {
                createmodel : 'showCreateRemoteApplicationProfileForm',
                deletemodel : 'deleteRemoteApplicationProfiles',
                editmodel : 'showEditRemoteApplicationProfileForm',
                profileparameters : 'showRemoteApplicationParameterProfilesWindow',
                selectionchange : 'onRemoteApplicationProfileSelectionChange'
            }
        };
    },

    /**
     * @method buildTools
     * 
     * Builds the tools for this tab panel.
     * 
     * @template
     * @protected
     * @return {Ext.panel.Tool[]} The tools for this tab panel.
     */
    buildTools : function() {

        return [ {
            type : 'help',
            reference : 'helpTool',
            id : Ext.id(null, 'help-tool-'),
            callback : 'onHelpToolClicked',
            tooltip : 'Connected Vehicle Applications Help'
        } ];
    }
});
