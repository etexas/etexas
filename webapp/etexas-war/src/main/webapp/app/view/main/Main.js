/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
/**
 * @class ETexas.view.main.Main
 * @extends Ext.panel.Panel
 * 
 * A panel to view web application content.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.main.Main', {
    extend : 'Ext.panel.Panel',
    xtype : 'main',

    requires : [ 'ETexas.view.account.AccountPanel', 'ETexas.view.main.MainController', 'ETexas.view.main.MainModel', 'ETexas.view.perspective.ApplicationsPerspective',
            'ETexas.view.perspective.ExecutionsPerspective', 'ETexas.view.perspective.SimulationsPerspective', 'Ext.plugin.Viewport' ],

    controller : 'main',

    viewModel : {
        type : 'main'
    },

    plugins : 'viewport',

    layout : 'border',

    /** @inheritdoc */
    initComponent : function() {

        this.items = this.buildItems();
        this.bbar = this.buildBbar();
        this.callParent();
    },

    /**
     * @method buildItems
     * 
     * Builds the items for this panel.
     * 
     * @template
     * @protected
     * @return {Object[]} The items for this panel.
     */
    buildItems : function() {

        return [ {
            region : 'north',
            xtype : 'container',
            reference : 'headerContainer',
            id : Ext.id(null, 'header-container-'),
            cls : 'etexas-header',
            height : 52,
            layout : {
                type : 'hbox',
                align : 'middle'
            },

            items : [ {
                xtype : 'component',
                reference : 'titleComponent',
                id : Ext.id(null, 'title-component-'),
                cls : 'etexas-title',
                html : 'eTEXAS',
                flex : 1
            }, {
                xtype : 'toolbar',
                reference : 'navigationToolbar',
                id : Ext.id(null, 'navigation-toolbar-'),
                cls : 'etexas-navigation-bar',
                items : [ {
                    xtype : 'button',
                    reference : 'applicationsButton',
                    id : Ext.id(null, 'applications-button-'),
                    cls : 'etexas-navigation-button',
                    enableToggle : true,
                    toggleGroup : 'navigation',
                    text : 'Applications',
                    listeners : {
                        toggle : 'onApplicationsButtonToggled'
                    }
                }, {
                    xtype : 'button',
                    reference : 'executionsButton',
                    id : Ext.id(null, 'executions-button-'),
                    cls : 'etexas-navigation-button',
                    enableToggle : true,
                    toggleGroup : 'navigation',
                    text : 'Executions',
                    listeners : {
                        toggle : 'onExecutionsButtonToggled'
                    }
                }, {
                    xtype : 'button',
                    reference : 'simulationsButton',
                    id : Ext.id(null, 'simulations-button-'),
                    cls : 'etexas-navigation-button',
                    enableToggle : true,
                    toggleGroup : 'navigation',
                    text : 'Simulations',
                    pressed : true,
                    listeners : {
                        toggle : 'onSimulationsButtonToggled'
                    }
                }, {
                    xtype : 'button',
                    reference : 'helpButton',
                    id : Ext.id(null, 'help-button-'),
                    cls : 'etexas-navigation-button',
                    text : 'Help',
                    handler : 'onHelpButtonClicked'
                }, {
                    xtype : 'button',
                    reference : 'userButton',
                    id : Ext.id(null, 'user-button-'),
                    cls : 'etexas-user-button',
                    menu : [ {
                        reference : 'accountItem',
                        id : Ext.id(null, 'account-item-'),
                        handler : 'onAccountItemClicked',
                        text : 'Manage Account'
                    }, '-', {
                        reference : 'logOutItem',
                        id : Ext.id(null, 'log-out-item-'),
                        handler : 'onLogOutItemClicked',
                        text : 'Log Out'
                    } ]
                } ]
            } ]
        }, {
            region : 'center',
            xtype : 'container',
            reference : 'perspectivesContainer',
            id : Ext.id(null, 'perspectives-container-'),
            layout : 'card',

            items : [ {
                xtype : 'accountpanel',
                reference : 'accountPanel',
                id : Ext.id(null, 'account-panel-'),
                listeners : {
                    accountdeleted : 'onAccountDeleted',
                    passwordchanged : 'onPasswordChanged',
                    usernamechanged : 'onUsernameChanged'
                }
            }, {
                xtype : 'applicationsperspective',
                reference : 'applicationsPerspective',
                id : Ext.id(null, 'applications-perspective-')
            }, {
                xtype : 'executionsperspective',
                reference : 'executionsPerspective',
                id : Ext.id(null, 'executions-perspective-')
            }, {
                xtype : 'simulationsperspective',
                reference : 'simulationsPerspective',
                id : Ext.id(null, 'simulations-perspective-')
            } ]
        } ];
    },

    /**
     * @method buildBbar
     * 
     * Builds the bottom toolbar for this panel.
     * 
     * @template
     * @protected
     * @return {Object[]} The bottom toolbar for this panel.
     */
    buildBbar : function() {

        return [ {
            xtype : 'displayfield',
            reference : 'copyrightField',
            id : Ext.id(null, 'copyright-field-'),
            value : '&copy; 2017.'
        }, {
            xtype : 'displayfield',
            reference : 'linkField',
            id : Ext.id(null, 'link-field-'),
            value : '<a href="http://harmonia.com">Harmonia Holdings Group, LLC</a>.'
        }, '->', {
            xtype : 'displayfield',
            reference : 'titleField',
            id : Ext.id(null, 'title-field-')
        }, {
            xtype : 'displayfield',
            reference : 'versionField',
            id : Ext.id(null, 'version-field-')
        }, '-', {
            xtype : 'displayfield',
            reference : 'buildField',
            id : Ext.id(null, 'build-field-')
        } ];
    }
});