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
 * @override Ext.form.field.Text
 * 
 * Overrides the default behavior for text fields. Text fields now have error messages that include
 * punctuation and are restricted to a maximum length of 255 characters. The REST service throws a
 * 500 error if the maximum number of characters exceeds 255.
 */
Ext.define('Ext.overrides.form.field.Text', {
    override : 'Ext.form.field.Text',
    blankText : "This field is required.",
    invalidText : "The value in this field is invalid.",
    maxLength : 255,
    maxLengthText : "The maximum length for this field is {0}.",
    minLengthText : "The minimum length for this field is {0}."
});

/**
 * @override Ext.form.field.Number
 * 
 * Overrides the default behavior for number fields. Number fields now have error messages that
 * include punctuation.
 */
Ext.define('Ext.overrides.form.field.Number', {
    override : 'Ext.form.field.Number',
    minText : 'The minimum value for this field is {0}.',
    maxText : 'The maximum value for this field is {0}.',
    nanText : '{0} is not a valid number.',
    negativeText : 'The value cannot be negative.'
});

/**
 * @override Ext.form.Panel
 * 
 * Overrides the default behavior for form panels. Form panels now have a custom error reader for
 * compatibility with REST service POSTS.
 */
Ext.define('Ext.overrides.form.Panel', {
    override : 'Ext.form.Panel',

    /**
     * @protected
     * @method initComponent
     * @override Ext.form.Panel.initComponent
     * 
     * Initializes the error reader for the form.
     */
    initComponent : function() {

        this.errorReader = {

            type : 'json',
            read : function(response) {

                var json = Ext.JSON.decode(response.responseText, true);

                if (json && json.statusCode) {

                    response.status = json.statusCode;
                }

                return {

                    success : response.status >= 200 && response.status < 300
                };
            }
        };

        this.callParent();
    }
});

/**
 * @override Ext.form.field.Base
 * 
 * Overrides the default behavior for base form fields. Base form fields now have error messages
 * that appear beside the field.
 */
Ext.define('Ext.overrides.form.field.Base', {
    override : 'Ext.form.field.Base',
    msgTarget : 'side'
});

/**
 * @override Ext.form.Basic
 * 
 * Overrides the default behavior for basic forms. Basic forms now handle submission failures by
 * displaying the appropriate alert message.
 */
Ext.define('Ext.overrides.form.Basic', {
    override : 'Ext.form.Basic',

    /**
     * @public
     * @method submit
     * @override Ext.form.Basic.submit
     * 
     * Submits the form.
     */
    submit : function(options) {

        if (options.failure !== undefined && options.failure !== null) {

            var failureInput = options.failure;
        }

        var onFailure = function(form, action) {

            if (action.response.status < 200 || action.response.status >= 300) {

                var exception = Ext.JSON.decode(action.response.responseText, true);

                if (exception && exception.exceptionType) {

                    ETexas.util.Functions.showExceptionMessage(exception);
                }
                else {

                    switch (action.failureType) {

                        case Ext.form.action.Action.CLIENT_INVALID:

                            Ext.Msg.alert('Client Failure', 'Form fields may not be submitted with invalid values.');

                            break;

                        case Ext.form.action.Action.CONNECT_FAILURE:

                            if (action.response.timedout) {

                                Ext.Msg.alert('Connection Failure', 'The connection has timed out.');
                            }
                            else {

                                Ext.Msg.alert('Connection Failure', 'An unknown connection failure has occurred.');
                            }

                            break;

                        case Ext.form.action.Action.SERVER_INVALID:

                            if (action.result.msg !== undefined && action.result.msg !== null) {

                                Ext.Msg.alert('Server Failure', action.result.msg);
                            }
                            else {

                                Ext.Msg.alert('Server Failure', 'An unknown server failure has occurred.');
                            }
                    }
                }

                if (failureInput !== undefined && failureInput !== null) {

                    failureInput(form, action);
                }
            }
        };

        arguments[0].failure = onFailure;
        this.callParent(arguments);
    }
});

/**
 * @override Ext.Ajax
 * 
 * Overrides the default behavior for Ajax requests. Ajax request exceptions now result in an
 * appropriate alert message.
 */
Ext.Ajax.on('requestexception', function(conn, response, options, e) {

    var error = response.status + ' - ' + response.statusText;

    if (response.status < 200 || response.status >= 300) {

        if (response.status === 401) {

            if (!(ETexas.util.Config.getUser() === 'log' && ETexas.util.Config.getToken() === 'out')) {

                sessionStorage.setItem('authInvalidFlag', true);
            }
            else {

                sessionStorage.removeItem('authInvalidFlag');
            }

            sessionStorage.removeItem('user');
            sessionStorage.removeItem('token');
            window.location.reload();
        }
        else {

            var exception = Ext.JSON.decode(response.responseText, true);

            if (exception && exception.exceptionType) {

                ETexas.util.Functions.showExceptionMessage(exception);
            }
            else if (response.status > 400) {

                var errorData = Ext.JSON.decode(response.responseText, true);
                if (errorData === undefined || errorData === null) {
                    errorData = response.responseText;
                }

                Ext.Msg.alert('REST Error Message', 'Ajax Request Exception! ' + error + ': ' + errorData);
            }
            else if (response.timedout) {

                Ext.Msg.alert('Communication Failure', 'The connection has timed out.');
            }
        }
    }
});

/**
 * @overrides Ext.Ajax.timeout
 * 
 * Overrides the default timeout (30 seconds) for Ajax requests. The default timeout is now
 * increased to 300 seconds in order to have a longer timeout period than the server.
 */
Ext.Ajax.setTimeout(300000);

/**
 * @overrides the default headers to fix a bug in firefox.
 * 
 */
Ext.Ajax.setDefaultHeaders({
    'Accept' : '*/*'
});

/**
 * @override Ext.Ajax
 * 
 * Overrides the default behavior for Ajax requests. Ajax request now include user authentication
 * credentials such as the username and authentication token for the current user.
 */
Ext.define('Ext.overrides.Ajax', {
    override : 'Ext.Ajax',

    /**
     * @public
     * @method request
     * @override Ext.Ajax.request
     * 
     * Sends an HTTP (Ajax) request to a remote server.
     */
    request : function(options) {

        arguments[0].withCredentials = true;
        arguments[0].username = ETexas.util.Config.getUser();
        arguments[0].password = ETexas.util.Config.getToken();
        this.callParent(arguments);
    }
});

/**
 * @override Ext.data.Store
 * 
 * Overrides the default behavior for data stores. Data stores are no longer affected by an Ext JS
 * 5.1.1 bug that causes an exception when stores are filtered. NOTE: this override is only
 * applicable for Ext JS 5.1.1 and should be removed when updating to Ext JS 5.1.2 or later.
 */
Ext.define('Overrides.Store', {
    override : 'Ext.data.Store',

    /**
     * @public
     * @method getByInternalId
     * @override Ext.data.Store.getByInternalId
     * 
     * Returns the record with the specified internal ID.
     * 
     * @param {Mixed} internalId The internal ID to match.
     * @return {Ext.data.Model} The record with the specified internal ID or null if no such record
     * exists.
     */
    getByInternalId : function(internalId) {

        var data = this.getData(), keyCfg;

        if (!this.hasInternalKeys) {
            keyCfg = {
                byInternalId : {
                    property : 'internalId',
                    rootProperty : ''
                }
            };
            this.hasInternalKeys = true;
        }

        if (data.filtered) {
            if (keyCfg) {
                data.setExtraKeys(keyCfg);
            }
            data = data.getSource();
        }

        if (keyCfg) {
            data.setExtraKeys(keyCfg);
        }

        if (data.byInternalId) {
            return data.byInternalId.get(internalId) || null;
        }

        var rec = null;

        data.each(function(item) {
            if (item.internalId === internalId) {
                rec = item;
                return false;
            }
        });
        return rec;
    }
});