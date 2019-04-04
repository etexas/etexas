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
 * @class ETexas.view.login.LoginContainer
 * @extends Ext.container.Container
 * 
 * A container to provide login controls and application information.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.login.LoginContainer', {
    extend : 'Ext.container.Container',
    xtype : 'logincontainer',

    requires : [ 'ETexas.view.login.LoginContainerController', 'ETexas.view.login.LoginForm', 'Ext.plugin.Viewport' ],

    controller : 'logincontainer',

    plugins : 'viewport',

    /**
     * @cfg {ETexas.view.login.LoginForm} loginForm The login form for this container.
     */
    loginForm : null,

    /** @cfg {Ext.panel.Panel} blogPanel The blog panel for this container. */
    blogPanel : null,

    /** @cfg {Ext.panel.Panel} linkPanel The link panel for this container. */
    linkPanel : null,

    scrollable : true,

    /**
     * @method buildLoginForm
     * 
     * Builds the login form for this container.
     * 
     * @protected
     */
    buildLoginForm : function() {

        this.loginForm = Ext.create('ETexas.view.login.LoginForm', {
            id : Ext.id(null, 'login-form-'),
            renderTo : 'loginForm',
            listeners : {
                login : 'login'
            }
        });
    },

    /**
     * @method buildBlogPanel
     * 
     * Builds the blog panel for this container.
     * 
     * @protected
     */
    buildBlogPanel : function() {

        this.blogPanel = Ext.create('Ext.panel.Panel', {
            id : Ext.id(null, 'blog-panel-'),
            title : 'Latest from the Blog',
            renderTo : 'blog',
            frame : false,
            border : false,
            bodyStyle : 'background: transparent',
            listeners : {
                afterrender : 'onBlogPanelRendered'
            }
        });
    },

    /**
     * @method buildLinkPanel
     * 
     * Builds the link panel for this container.
     * 
     * @protected
     */
    buildLinkPanel : function() {

        this.linkPanel = Ext.create('Ext.panel.Panel', {
            id : Ext.id(null, 'link-panel-'),
            title : 'Learn more about eTEXAS',
            renderTo : 'links',
            frame : false,
            border : false,
            bodyStyle : 'background: transparent',
            html : this._buildLinkItems()
        });
    },

    /**
     * @method _buildLinkItems
     * 
     * Builds the list of eTEXAS link items.
     * 
     * @private
     * @return {String} The list of eTEXAS link items.
     */
    _buildLinkItems : function() {

        var html = '<ul><li><a href="http://etexas.harmonia.com/blog/">Visit the blog</a></li>';
        html = html.concat('<li><a href="https://code.google.com/p/etexas/">Download eTEXAS </a></li>');
        html = html.concat('<li><a href="/help/Content/Home.htm">Get Help</a></li></ul>');

        return html;
    },

    /** @inheritdoc */
    doDestroy : function() {

        Ext.destroy(this.loginForm);
        Ext.destroy(this.blogPanel);
        Ext.destroy(this.linkPanel);
        this.callParent();
    }
});
