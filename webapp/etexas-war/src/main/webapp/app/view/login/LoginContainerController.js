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
 * @class ETexas.view.login.LoginContainerController
 * @extends Ext.app.ViewController
 * 
 * The {@link ETexas.view.login.LoginContainer} controller.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.login.LoginContainerController', {
    extend : 'Ext.app.ViewController',
    alias : 'controller.logincontainer',

    /** @inheritdoc */
    init : function(view) {

        var queryParams = Ext.Object.fromQueryString(location.search);
        if (queryParams.restCall === 'verifyEmail' && queryParams.email && queryParams.token) {

            this._verifyEmailAddress(queryParams.email, queryParams.token);
        }
        else {

            var me = this;
            Ext.Ajax.request({
                scope : this,
                method : 'GET',
                url : '/login.html',
                success : function(response, opts) {

                    view.update(response.responseText);
                    view.buildLoginForm();
                    view.buildBlogPanel();
                    view.buildLinkPanel();
                    me._loadVersionInformation();

                },
                failure : function(response, opts) {

                    view.update('Login content could not be loaded.');
                }
            });
        }
    },

    /**
     * @method _verifyEmailAddress
     * 
     * Verifies the email address for the current user.
     * 
     * @private
     * @param {String} email The email address for the user.
     * @param {String} token The authentication token for the user.
     */
    _verifyEmailAddress : function(email, token) {

        Ext.Ajax.request({
            scope : this,
            method : 'PUT',
            url : ETexas.util.UrlProvider.getEmailVerificationUrl(),
            params : {
                email : email,
                token : token
            },
            success : function(response, opts) {

                var message = 'You have successfully verified your email address. You may now login with your username and password.';
                Ext.Msg.alert('Registration Complete', message, function(btn, text) {
                    window.location = window.location.pathname;
                });
            }
        });
    },

    /**
     * @method _loadVersionInformation
     * 
     * Loads the version information for the login container.
     * 
     * @private
     */
    _loadVersionInformation : function() {

        Ext.Ajax.request({
            scope : this,
            method : 'GET',
            url : ETexas.util.UrlProvider.getVersionUrl(),
            success : function(response) {

                var versionInfo = Ext.JSON.decode(response.responseText);
                document.getElementById('webapp-title').innerHTML = versionInfo.title;
                document.getElementById('webapp-version').innerHTML = versionInfo.version;
                document.getElementById('webapp-build').innerHTML = versionInfo.build;
            },
            failure : function(response) {

                document.getElementById('webapp-title').innerHTML = 'eTEXAS';
                document.getElementById('webapp-version').innerHTML = '';
                document.getElementById('webapp-build').innerHTML = '';
            }
        });
    },

    /**
     * @method onBlogPanelRendered
     * 
     * Sets the blog content when the blog panel is rendered.
     * 
     * @protected
     * @param {Ext.Component} component The blog panel component.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onBlogPanelRendered : function(component, eOpts) {

        Ext.Ajax.request({
            scope : this,
            method : 'GET',
            url : ETexas.util.UrlProvider.getBlogUrl(),
            params : {
                blogCount : 2
            },
            success : function(response, opts) {

                var html = '';
                var feed = Ext.JSON.decode(response.responseText);

                for (var i = 0; i < feed.length; i++) {

                    var title = feed[i].title;
                    var description = feed[i].description;
                    var link = feed[i].link;
                    var date = feed[i].date;

                    html += '<div>';
                    html += '<span class="date">' + date + '</span>';
                    html += '<h5><a href="' + link + '">' + title + '</a></h5>';
                    html += '<p class="italic">' + description + '</p>';
                    html += '</div>';
                }

                component.setHtml(html);
            },
            failure : function(response, opts) {

                component.setHtml('Blog posts could not be loaded at this time.');
            }
        });
    },

    /**
     * @method login
     * 
     * Logs the user into the application.
     * 
     * @protected
     * @param {String} username The username for the user.
     * @param {String} token The authentication token for the user.
     */
    login : function(username, token) {

        sessionStorage.user = username;
        sessionStorage.token = token;
        ETexas.util.Config.setUser(username);
        ETexas.util.Config.setToken(token);
        Ext.destroy(this.getView());
        Ext.create('ETexas.view.main.Main');
    }
});
